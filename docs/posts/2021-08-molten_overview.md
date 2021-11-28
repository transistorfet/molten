
An Overview Of Molten Internals
===============================

###### *Written August 2021 by transistor_fet*

Molten is both a language, and a compiler that's written in Rust which uses LLVM for its final code
generation and linking of native binaries.  I started it as a larger project to learn more about
Rust, after trying a few smaller projects.  The internal design has evolved a lot as I've learned
better ways of doing things in Rust.  In this article, I'd like to describe how code is represented
inside the compiler and the stages it passes through until being handed over to LLVM.

Internal Representations
------------------------

The compiler has 3 different internal representations: AST, HIR, and LLCode.  The AST is the direct
output of the parser and represents the code as written in the input file.  It is rather quickly
turned into HIR (High-level Intermediate Representation), which is easier to compile and has
syntactic sugar from the AST converted into the more verbose code that it's equivalent to.  Finally,
after analysis is done but before the final code generation, the HIR is transformed into LLCode,
lowering the code to a more C-like representation.  At this point, all the high level features like
closures and exception handling are directly implemented in the LLCode without any need for special
consideration by the code generation stage.  The codegen stage then converts the LLCode to LLVM IR
using the [llvm-sys crate](https://gitlab.com/taricorp/llvm-sys.rs), and LLVM handles the rest,
converting it to native assembly language, optimizing, and then finally encoding it into a binary
object file.  The compiler only outputs object files which must be linked together using `lli` or
`gcc` to make an executable.

During each run of the compiler, a special `Session` value is created which contains all the
associated data collected while compiling one module.  It's passed between all the stages, often as
the first argument, and acts as the common point of coordination.  It uses HashMaps indexed by a
`UniqueID` (a unique integer associated with each node in the HIR tree) as well as each type and
definition.  Using HashMaps in this way makes it easier in Rust to create graph-like representations
of data.  A similar structure with hashmap is found in the Rust compiler itself.  The Session struct
contains five different HashMaps for: definitions (functions, classes, enums, local variables, class
fields, etc); scopes for mapping variable and type names to definition IDs; references from one
`UniqueID` to another; types associated with a `UniqueID`; and trait constraints on type variables.

Types are represented using the `Type` enum.  A `Type` can be:

- an object, with a name, definition ID, and optional type parameters (class/enum objects)
- a tuple of `Type`s
- a record (a tuple with named fields)
- a reference to `Type` (a mutable boxed value)
- a function with a tuple as arguments, a return type, and the ABI it uses
- a universal type variable (generics, representing a type parameter)
- a placeholder variable (an unknown type that must be resolved during type checking)

Each thing that is defined and can be referenced has an associated `Def`, such as functions,
classes, class fields, variables, function arguments, trait definitions, enums, etc.  `Def` is an
enum with a variant for each type of definition, which holds a reference to a definition-specific
struct that can be used to hold information about that definition.  Even though each definition type
might be quite different, they are stored in the same table for simplicity.  By looking up a
definition ID, all the relevant info can be found, and modified through definition-specific methods.


Stages
------

In order to get from the input text to the output binary, the code will pass through the following
stages:

- Parsing (which generates the AST)
- Refining (desugaring, which generates the HIR)
- Name Binding
- Type Checking
- Validation
- Exporting (create the external declarations file)
- Transforming (lowering, which generates LLCode)
- Codegen (which builds the in-memory LLVM representation)
- LLVM Optimization and Binary Emitting


Parsing
-------

Molten's parser uses the [Nom](https://github.com/Geal/nom) crate for parsing, which is based on
parser combinators.  Simple parsers are combined together to make larger and more complicated
parsers.  Molten doesn't use a lexer, which saves a bit of complexity, but the downside is that it
requires checks for whitespace and comments to be scattered throughout the parser.  These appear in
the form of the `wscom!(<subparser>)` macro, which will match the given subparser and return its
result with any whitespace or comments remove from before and after the subparser's match.

There are a few places in the parser where we do some extra processing outside of the normal
combinators, namely when parsing infix operators, invocations, accessors, and so on.  We do this to
apply order of operations to the pieces of an expression, which is rather hard to implement using a
recursive decent parser.  The parser will first read in a simple term, and then read as many pairs
as it can of an operator followed by a simple term.  Once it has a list of terms and operators, it
passes the list to the `AST::fold_op` function, which implements the [Shunting-yard
Algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm) to build the list into a tree that
reflects the order of operations for all infix operators.

A similar thing is done for the simple terms, which themselves can contain an invocation
(`foo(arg)`), an index operation (`foo[index]`), a resolve operation (`Foo::bar`), or an access
operation (`foo.bar`).  The folding is much simpler here, as all all the operations are
left-associative and have equal precedence, so we can assemble it directly into a left-associated
tree.


Refining
--------

After parsing, the AST is passed to the refinery, which converts it inline into HIR while
manipulating the code at the same time, and checking for syntax errors and forbidden combinations.
This stage is also where importing occurs, so the parser will be invoked again during refining in
order to load and parse the declaration files of imported libraries.

The larger changes that are made to the tree here involve array literals, `for` loops, classes,
indexing, and the initialization of the module being compiled.  Array literals (eg.  `let array = [1,
2, 3]`) are converted into a sequence of code which creates a new array using the `new` keyword, and
then pushes each element one by one into the newly created array.  It is hard coded to use the array
implementation included in libcore.mol.

`for` loops are converted into a while loop which takes a subclass of `Iterator`, and runs the for
loop's body on each element.  It uses a `match` block inside the conditional portion of the while
loop to unpack the `Option` returned by `Iterator.next()` and runs the body of the loop inside that
match, returning true or false to continue the loop based on whether `Some` or `None` was received.
The body of the while loop is empty.

Classes are checked for any functions named "new", and if found, the first argument must be "self"
or an error will be raised.  It will then insert a return of "self" as the last item in the body
of any "new" functions.

HIR Visitor
-----------

The stages that use HIR use a common `Visitor` trait which makes it easier to traverse the immutable
HIR code.  Each stage implements the `Visitor` trait and defines only the node visitors it needs,
leaving the default implementations for the rest.


Name Binding
------------

The name binding stage is pretty straight-forward.  It traverses the immutable HIR code, in which
each node has a UniqueID number.  If it encounters a definition, such as a function, class, or local
variable, it will create a new UniqueID and set a ref in `Session` from the node id to the
definition id.  It then calls the appropriate definition function to create a `Def` struct to hold
any definition-specific information, which also sets the name in the current scope to point to the
definition ID for later lookup.

When a reference to a definition is encountered, a new ref will be set to point from that node's id
to the found definition (or an error if the name is not defined).  A few things are not bound here
but instead delayed until type checking, namely overloadable function and method references, which
cannot be linked to a specific function implementation without knowing the types first.

During this stage, types are also bound to their definition ID.  The parser sets the ID of object
and variable types to `UniqueID(0)`.  The binding stage traverses all types and if it encounters a
`UniqueID(0)` it looks up the type name in the current scope and either assigns the ID found to the
type's ID, or raises an error if the type is undefined.  Once this is complete, we can used the
type ID to quickly look up the definitions in `Session` rather than using the names and scopes all
the time.


Type Checking
-------------

The type checking phase does both the inferring of types and the checking of them.  It traverses the
HIR and returns a `Type` for each node.  If a type cannot be determined, a new placeholder type
variable will be returned which will have to be resolved by the end of type checking, or else an
error will be raised that not enough type information is present.  It also updates the type stored
in `Session` which is associated with each node's ID, and updates the type associated with the
definition ID, so we can easily look them up later.  When updating a type in `Session`, the
`check_type` function is called, which tries to find the most general type which unifies the old and
new type.  If no unifier can be found, a type mismatch error is raised.

In some cases, when making use of a definition, such as calling a function or pattern matching an
enum variant's values, the universal type variables in the definition type must be mapped to
placeholder variables so that specific types can be substituted for the placeholders.  We also store
both the definition type (if known) and the substituted type.  We will need both types in order to
determine where automatic trait object packing/unpacking needs to be inserted, which is done in the
transform stage.

The other thing done during type checking is late binding.  Molten supports overloading of function
and method names, so there may be multiple function definitions with the same name but different
types.  During the name binding stage, we record all these definitions using the `OverloadDef`
struct, which can keep track of all the variants and the scopes in which they are defined, but we
can't set the references from function calls to specific overloaded variants until we know more
about their types.  During type checking, when we come across a function call site, we can calculate
its type and then look up any overloaded variants that match the type signature.  If we find no
variants or more than one variant, we raise an error, and if we find exactly one, we set the
reference from that invocation to that specific variant ID, so that we'll be able to generate a
static function call later.


Validation
----------

This stage is a more recent edition and will likely be expanded later.  It's the last chance to
check any error conditions that might make compilation impossible, but which need to be done after
type checking has completed and the types are known.

We check that overloaded function and method definitions do not have overlapping types.  In some
cases, we can use the invocation of a function or method to determine its type, rather than
requiring its type to be fully known from the definition alone, but since we wont visit all the call
sites until after visiting the overloaded function definitions, we can't check for this overlap
during type checking without requiring function argument type hints in the definition.  We therefore
do it here when all the types are known.

We also check each class definition to make sure it has at least one `new` constructor if it also
contains data fields, that each data field is explicitly assigned a value in each constructor, and
that each constructor also calls the `Super::new` function if it has a parent and that parent has a
`new` constructor.  This prevents the condition where a field might not be initialized before use.

The last check it does is to make sure any `raise` expressions are always the last node in a block,
since any code after it won't run.  A raise expression is currently the only expression that will
never return.


Exporting
---------

This stage just traverses the tree, once all the types are determined, and generates a text file
with the `.dec` extension containing the declarations of all the functions, classes, methods, enums,
traits definitions, and trait method declarations that are publicly exported from the module being
compiled.  This file will be needed when importing the declarations into another file.

Each source file that's compiled represents a module, and will have its own closure at the top
module scope which is turned into a "run" function to be called when the module is imported, or
called by main if the file is compiled without the `-l` flag.  The run function will check a
module-specific memo value which is initialized to false and set once the run function has
completed.  If set, the run function will not attempt to initialize again.


Transformation
--------------

The transformation stage is currently one of the most complicated stages.  I'm planning to
eventually break it up into multiple independent stages.  It takes the fully typed and
reference-connected HIR code and transforms it into LLCode, converting the high level language
features into their low level equivalents.  It generates LLCode to build all the vtables for classes
and traits, builds all the enum constructor functions, inserts arguments for the exception handling
information and the relevant `setjmp` and `longjmp` calls used by exception handling, and inserts
code to pack and unpack trait objects whenever a value is passed into a function which takes a
universal type variable.  The biggest thing, however, is closure conversion.

Closure Conversion
------------------

In order to represent a closure in low level compiled code, we need some way of accessing the
variables defined outside of the closure's local scope from within that local scope.  We do this by
putting all the non-local variable values or references that are accessed inside the closure into a
unique struct, and passing this struct as an argument into the compiled version of the closure
whenever we call it.  Our function references will need to change, to be a pair of pointers rather
than a single pointer.  One pointer in the pair will point to the C function that implements the
closure, and the other to the context struct associated with it.  We also need to modify the closure
body itself, the location where the closure is defined, and every location where any closure is
called.

Firstly, we modify the function definition and body.  We create a new C-like function with an
additional argument added to the end for the closure context struct.  We then traverse the body of
the closure converting it to LLCode as we go.  If we come across a reference to a variable outside
the current scope, we add a field to the closure context struct (which is stored in the
`ClosureDef`) of the current closure being converted, and transform the reference into a field
access on the context struct argument in the local scope.  By the end of this process, we will have
recorded all the fields that the closure might access, and converted all those references to work
locally.

Next, we build brand-new HIR code which will, at runtime, create a copy of the closure context struct
and assign each variable reference to the corresponding field in the context.  We do this in HIR
code because we can then run it through the name binding and type checking stages on its own to
connect any references and determine any types.  It also allows us to convert nested closures, which
is described in more detail below.  Immediately after analyzing our new HIR code, we convert it into
LLCode and insert it into the closure definition site.

The last thing to do at the closure definition site is to insert LLCode which will build a reference
to the closure (a pair of pointers), which will be the return value of the closure definition
expression.  The advantage of using a pair of pointers, as opposed to putting the function pointer
into the context struct itself, is that if we have a closure function which doesn't reference any
external values, then we wont need to allocate a context struct for it, allowing the closure to be
represented as a static value with a NULL pointer for the context struct field.  We can then treat
all closure call sites the same, while allowing us to statically define all builtin functions
without the need to be initialized at runtime.  We also set the global reference to the closure
value here, if the closure is both named and exported.

We need to patch up all closure call sites, but we do this at the same time as the rest of the
transformation process.  Whenever we come across a call to a function, we get the type information
which contains the ABI.  All functions and methods that use the molten ABI are closures, so we know
that the LLCode value for the function will be our pair of function pointer and context.  We
generate code to extract the first element from the closure value and call it as a function pointer,
with the second argument extracted and passed in as the last argument in the call.

Nested closures are handled by virtue of the fact that we convert the closures from the most deeply
nested closure up to the top level closure.  The body of the most nested closure will be converted,
with any references added to its closure context struct.  When we analyze and transform the HIR code
generated to initialize that context, if we're inside another closure body that's being transformed,
then the references in that code block will also be checked to see if they are local or non-local to
the parent closure, and will be converted into a closure context access themselves, with a record
added to the parent closure's context.  All of the necessary values referenced in the most deeply
nested closure will be added to all the closure contexts in between.


Codegen
-------

In the transformation stage, we converted the nested HIR into a list of global definitions and
declarations, which is passed to the codegen stage.  All the references to types and definitions in
the Session hashmaps have been removed, and a new set of `SetValue` and `GetValue` operations
embedded within the LLCode will be used to link together `LLVMValueRef`s using a new HashMap stored
in the `codegen::LLVM` struct, as well as another HashMap for linking `LLVMTypeRef`s via the
`LLType::Alias(UniqueID)` enum variant.

The stage first initializes the LLVM library, and adds some base struct types for the exception jump
buffer and the boxed universal type variable.  It then passes over the global definitions list and
declares all types, named structs, functions, and global variables it encounters without filling in
and code blocks for functions.  Once all the declarations are created, we wont encounter a situation
where the type required isn't declared yet.  We can then pass over the global definitions again to
build the bodies of all the functions.

Universal type variable boxing and unboxing is handled here.  The strategy being used for generics
is to make all generic values look the same by boxing any data that isn't already a pointer, and
treated all generic values as pointers.  For more info on the different ways of implementing
generics, check out this excellent article by [Tristan
Hume](https://thume.ca/2019/07/14/a-tour-of-metaprogramming-models-for-generics/) Throughout the
build process we call a method on the codegen stage called `.build_cast` which will build the
instructions to cast between different `LLVMTypeRef`s automatically, based on the `LLVMTypeKind` of
the source and destination types, and only if the types are different.  This should only happen when
either casting from a child class object to its parent object type when calling a parent method, or
when casting between type variables and the types being substituted for them.  The universal type
variables from HIR will always be converted into a custom LLVM type `%TypeVar*`, so we can assume
that any attempt to cast between a pointer and a non-pointer type will be a generics cast, and we
will either box the non-pointer data, or unbox it from the pointer depending on which is the source
and destination type.  This is a bit of a hack, but it saves some complexity in the transform stage.
The memory allocated for the boxing comes from the `molten_malloc` function which is just an alias
for the libgc function `GC_malloc` when compiling with libgc support.  It is never explicitly freed
and left to libgc to clean up.

In order to support trait objects, all type variables in LLCode are treated as a pair of pointers,
with one pointer being the boxed or reference data, and the other pointer being the vtable for the
trait constraints of the universal type variable.  Currently, all type variables are treated as a
pair, even if there are no constraints and the vtable is set to NULL.  The actual conversion from a
value to a trait object and from a trait object to a value occurs during the transformation stage.
The code to set the appropriate vtable value is generated there and the only thing for the codegen
stage to do is either box or unbox the data.  This boxing/unboxing conversion in the codegen stage
might be moved to the transform stage eventually, when the transform stage is broken up. 

Once the LLVM IR has been built, the codegen stage invokes the LLVM optimizer, and then the LLVM
emitter.  When compiling with the `-S` command line flag, it will output the code as an .ll file in
the text-based LLVM IR format.  When compiling with the `-c` flag, it will output an object file
compatible with with the LLVM or GCC linker, which must be linked to an implementation of libc and
libm, as well as to libgc, the conservative garbage collector library.


The Molten Command
------------------

The `molten` command included in the repository is a python script that will build and run an
executable.  When the compiler is run with the `-b` command line flag, it will only generate a link
file and then exit before compiling the rest of the program.  The link file contains a list of files
that are imported by the module being compiled.  The `molten` script will build the link file for
the main target file, and then build the link files for all of its dependencies until it has a
complete list of files that will be included in the binary, in the order of dependency.  It then
fully compiles each file in turn, and links them together using `lli` to get the final binary.  It
will then run the resulting binary, if the `run` command was used.

That completes the process of compiling in Molten.

