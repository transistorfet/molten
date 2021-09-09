
Molten
======

###### *Started November 06, 2017*

Molten is a programming language which borrows from the ML family of languages,
as well as from Rust and Python.  The compiler is written in Rust and uses LLVM
to generate IR which can be compiled to machine code.

I originally started this project in order to learn Rust.  It is intended to be
a high level language with a full object system that facilitates both functional
and object-oriented programming.  Some syntax elements have been changed from
typical ML languages to follow conventions found in more common languages, such
as C++, Rust, and Python (eg. parenthesis-delimited blocks, conventional class
definitions, generics/type parameters with angle brackets, etc).  For more info
on the internals, see [An Overview Of Molten
Internals](https://transistorfet.github.io/posts/molten_overview.html)


Installing
----------

You will need `rustc` and `cargo` installed.  It's recommended that you use
`rustup` to install these.  I've most recently tested it with rustc version
1.52.  You will also need LLVM 11 installed, as well as libgc
(Boehm-Demers-Weiser's Garbage Collector), and clang for linking, although clang
can be replace with gcc by editing the `molten` python script.

On Debian/Ubuntu, run:
`sudo apt-get install llvm-11 llvm-11-runtime llvm-11-dev clang libgc-dev`

On macOS, run:
`brew install llvm@11`

You may need to add /usr/local/opt/llvm@11/bin to your path, and you will probably
need to install libgc separately

Running
-------

The `molten` script helps with compiling and linking IR files.  To run an example:

```
./molten run examples/fac.mol
```

This will run cargo to build the compiler if needed, then compile the fac.mol
file, as well as all of its dependencies (in this case, the libcore.mol
library), link them together using clang, along with libgc, and then run the
binary.  It can also compile to LLVM IR, and run LLVM bitcode by using the `-S`
flag.  The resulting .bc file can be run using `lli-11`.


Example
-------

```
fn fac(x) {
    if x < 1 then
        1
    else
        x * fac(x - 1)
}

println(str(fac(10)))
```

### Types
```
()                  // unit type
Nil
Bool
Byte
Char                // UCS-4 character
Int
Real
String
(Int, Int) -> Int   // function type
'a                  // universal type variable
Array<Int>          // array of integers
(Int, Real)         // tuple type
{ a: Int, b: Real } // record type
```

### Declarations
```
let foo = 0
let bar: String = "Hey"
```

### Functions
```
fn foo(x, y) => x + y		    // named inline function

fn foo(x, y) { x + y }		    // named block function

let foo = fn x, y => x + y	    // anonymous function

fn foo(x: Int, y) -> Int { x + y }  // with optional type annotations

```

### Invoking Functions
Unlike in ML, the brackets of a function call are not elidable.  This is a
design decision to improve readability of the code and to make the parser
simpler and more predictable.
```
foo(1, 2)
```

### Blocks
A block is a collection of expressions which return the result of the last
expression in the block.  They can be used in place of a single expression.
They do not create their own local scope, at least at the moment, so variables
defined inside blocks will appear in the parent scope (usually the function the
block is in).  Each expression in the block must end in a newline or semi-colon
character (or be the last expression in the block).  This applies to the top
level.
```
let is_zero = if self.x <= 0 then {
    self.x = 0
    true
} else {
    false
}
```

### Math
Infix operators are evaluated using order of operations.  Both sides of an infix
operation must be on the same line, or a `\` character can be used to
continue the line.
```
5 + 12 * 2      // equals 29

42 * 4 \
   % 5          // equals 3
```

### And / Or
The keyword operators `and` and `or` have side-effects and will not execute the
second expression if the result can be determined from the first expression.
The expressions must have the same type, and the returned value is the first
expression that returns a non-zero value.
```
let is_cat = true
let result = is_cat and println("It's a cat") == ()
```
Since is_cat is Bool, both sides of the `and` must be Bool.  Since the
`println()` function returns Unit, we compare it with itself which will always
be true.  The `println()` will only execute if `is_cat` is true, and `result`
will be true if `is_cat` is true, or false if `is_cat` is false.

### Tuples
```
let tup = (1, "String", 4.5)
println(tup.1)                  // prints "String"
```

### Records
Records are like tuples but with named fields.  Record literals use the equals
sign ("=") to assign a value to a field.  Specifying a record type uses a colon
(":") to separate the field name from the type.
```
let rec = { i = 1, s = "String", r = 4.5 }
println(rec.s)                  // prints "String"

let rec: { i: Int, s: String, r: Real }
```

Records can be updated, which will copy all fields of the record into a new
record, but with some of the fields modified.
```
let rec = { i = 1, s = "String", r = 4.5 }

let newrec = { rec with s = "Updated" }
println("New Value: " + newrec.s)       // prints "Updated"
println("Old Value: " + rec.s)          // prints "String"
```

### Arrays
```
let array1 = [ 1, 3, 6 ]
for x in array1
    println(str(x))

let array2 = new Array<String>();
array2.insert(0, "Hello")
println(array2[0])
```
The Array type is defined in libcore, which must be imported if arrays are used.

### Flow Control
The return value of an if expression is the result of evaluating either the
`then` clause or `else` clause.  The types of both clauses must match.  The
`else` clause can be left out as long as the true clause evaluates to Nil.
```
if x == 5 then
    "It's five"
else
    "It's not five"
```

A match expression allows pattern matching, which can unpack refs, tuples,
records, and enums.  It can bind values to named variables in the pattern and
creates a new scope for each arm of the match expression.
```
match x with
| 1 => "It's one"
| 5 => "It's five"
| num => "It's not one or five, it's " + str(num)
```

Values can be unpacked with match as well, including records, tuples, refs, and
enum variants.  A underscore `_` will match any value, and an identifier (eg.
`value`) will match anything and bind that value to the name in the process, so
that it can be referenced inside the match arm.  Here's an example using a
record:
```
match { num = 10, name = "Ten" } with
| { num = 10, name = value } => println(value)
| { num = _, name = value } => println("Something else named " + value)
```

### Loops
```
while true
    println("looping")

for i in iter([ 1, 2, 3 ])
    println("counting " + i)
```
For loops take an instance of `Iterator<'item>` and calls the `.next()` method
on it, running the body for each `Option::Some('item)` returned.  The `iter`
function is defined for different types to convert them into an appropriate
iterator.  In the case of arrays, it will return the result of `new
ArrayInterator<'item>(input_array)`

### Refs
A ref is an indirect reference to some data.  It can be passed around as a
value, and dereferenced to get or set the data inside of it.  The internal value
of a reference is always mutable
```
let r = ref 42
println(str(*r))                // prints 42
*r = 65
println(str(*r))                // prints 65

fn foo(x: ref Int) { }          // ref types look similar to ref constructors

let r = ref { a = 42, b = "The Answer" }
println(*r.b)                   // prints "The Answer"
```

### Classes
```
class Foo {
    // A field with type String
    val mut name: String

    fn new(self, name) {
        self.name = name
    }

    fn get(self) => self.name

    fn static(x) => x * 2
}

class Bar extends Foo {
    fn get(self, title) => self.name + " " + title
}

let bar = new Bar("Mischief")
bar.get("The Cat")              // returns "Mischief The Cat"
Foo::static(5)
```
All methods are both closures, and virtual methods, so they can access variables
in their parents' scopes and also be overridden by a child class's
implementation of the same method, accessible with a reference to the parent
class type.

Fields can have an optional type, but not an initializer.  If a class has at
least one field, it must have at least one "new" constructor, which must assign
to each field an initial value.  The type can be inferred from this assignment
if the optional type is not supplied.  If a field is declared as mutable, it can
be reassigned to, but if the `mut` keyword is absent, the field can only be
assigned to within the constructor, and will be immutable after the constructor
has returned.  Every constructor must also call the `Super::new` method of its
parent class, if it has a parent that has a constructor.

### Enums (Tagged Unions)
An enum can either have no arguments, or a tuple of arguments.  Constructing an
enum variant requires using the Resolve (::) notation.  Pattern matching is
currently the only way to get values out of a variant.  Unlike with classes,
which allocate memory for a new instance, enums are immediate data types like
tuples and records.  In order to store it in a memory location, a `ref` must be
used.  A ref is required in order to make a recursive enum.
```
enum Value =
| None
| Integer(Int)
| String(String)
| Pair(String, String)
| Reference(ref Value)          // A recursive reference

let val = Value::String("Hey")

match val with
| Value::String(s) => println(s)
| _ => ()
```

Methods can be added to enums using a `methods` body.  Currently it can only be
used with enums.

```
methods Value {
    fn is_some(val: Value) {
        match val with
        | Value::None => false
        | _ => true
    }
}

val.is_some()
```

### Traits and Trait Objects
A trait can be defined with method declarations in the body, with the predefined
type alias "Self" used to refer to the current trait object
```
trait Add {
    decl add(Self, Self) -> Self
}
```

A trait can then be implemented for a given type.  An impl block can only have
function definitions that match the trait declarations.  Inside the impl block
the "Self" type alias will refer to the implementation type.  Trait objects that
are passed to arguments with type "Self" will automatically be unpacked into
their impl type, and if the return type has type "Self", the result will
automatically be packed back into a trait object.
```
impl Add for Int {
    fn add(x: Self, y: Self) -> Self {
        x + y
    }
}

impl Add for Real {
    fn add(x: Self, y: Self) -> Self {
        x + y
    }
}
```

Traits are not an object type, but instead are specified as a constraint on a
universal variable using a `where` clause.  If the actual type given does not
implement the constrained trait, then an type error will be raised.  Currently
trait objects can only be created by passing them into a function that takes a
universal variable with a constraint.  At the moment, only one trait can be
specified as a constraint but this will be changed in future.
```
fn do_some_adding(x: 'a) -> 'a where a: Add {
    x.add(x)
}

println(str(do_some_adding(400)))
println(str(do_some_adding(1.5)))
```

The above example will output:
```
800
3.000000
```

### Exceptions
All exceptions must be an instance of the `Exception` class defined in libcore.
```
try open("file.txt")
with e => println("Exception Occurred: " + e.msg)

try {
    //...
    raise new Exception("Problem")
} with
    e => println(e.msg)
```

### Annotations
A value can be type annotated using a colon followed by the type.
```
5 : Int
str(i : Int)
(func() : String)
```

### Import
```
import libcore
```

### External Functions
A function can be declared without being implemented, and functions can also be
defined with an ABI specifier so that they are accessible to other languages.
Only C support is currently implemented. A C function cannot be a closure.
```
decl foo(Int) -> Int         // external molten function
decl bar(Int) -> Int / C     // external C function

fn baz(i: Int) / C {
    // molten function that can be called from C
}
```

### Linking to C
An example of writing a C file, and linking it to a molten program is shown in
`lib/libccore.c`.  When imported into a molten file and compiled with the
`molten` script, the library will be compiling using clang and the
`libccore.cdec` (manually maintained) will be copied to `libccore.dec`.  The
importing molten program will be able to use declarations from libccore.cdec.
Some declarations that can be used in C are in `include/molten.h`, such as
accessing the garbage collected allocator.


Previously Uncompleted
----------------------

- Dynamic Dispatch/vtables works now!

- Closures have been implemented! Most functions and methods are now
  closures, although there is a new ABI type (MF) which is a
  non-closure function that can still be overloaded and can still
  throw exceptions (when they're implemented). It's mostly used by
  builtin functions

- Class field initializers are working!  It now adds a new closure to
  each class called __init__ which is called during "new" to initialize
  the class members

- Exceptions have finally been added using the setjmp/longjmp functions

- Enums have been added but with a lot of limitations.  It's not possible
  to use an enum inside itself (eg. to make a tree data structure).  It
  also doesn't properly calculate the enum size, because that will require
  having the target architecture info.

- Garbage collection has been added using the Boehm-Demers-Weiser
  Conservative C Garbage Collector.  It is possible to compile without
  the garbage collector by using the `--no-gc` command line argument.

- Traits have finally been added.  They currently are limited to one
  trait constraint per type variable, but I will hopefully add
  trait depedencies/inheritence soon


I'd be happy to hear of any additional features ideas or suggestions, if
you'd like to leave them under "Issues" on github.


