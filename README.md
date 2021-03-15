
Molten
======

###### *Started November 06, 2017*

Molten is a programming language which borrows from the ML family of languages,
as well as from Rust and Python.  The compiler is written in Rust and uses
LLVM to generate IR which can be compiled to machine code.

I originally started this project in order to learn Rust.  It is intended to be
a high level language with a full object system that facilitates both functional
and object-oriented programming.  Some syntax elements have been changed from
typical ML languages to follow conventions found in more common languages, such
as C++, Rust, and Python (eg. parenthesis-delimited blocks, conventional class
definitions, generics/type parameters with angle brackets, etc)


Installing
----------

You will need `rustc` and `cargo` installed.  It's recommended that you use
`rustup` to install these.  I've most recently tested it with rustc version 1.28.
You will also need LLVM 7 installed, as well as libgc (Boehm-Demers-Weiser's
Garbage Collector), and clang, although clang can be replace with gcc by editing
the `molten` python script.

On Debian/Ubuntu, run:
`sudo apt-get install llvm-7 llvm-7-runtime llvm-7-dev clang libgc-dev`

On macOS, run:
`brew install llvm@7`

You may need to add /usr/local/opt/llvm@7/bin to your path, and you will probably
need to install libgc separately

Running
-------

The `molten` script helps with compiling and linking IR files.  To run an example:

```
./molten run examples/fac.mol
```

This will run cargo to build the compiler if needed, then compile the fac.mol
file, as well as all of its dependencies (in this case, the libcore.mol library),
link them together using clang, along with libgc, and then run the binary.  It
can also compile to LLVM IR, and run LLVM bitcode by using the `-S` flag.  The
resulting .bc file can be run using `lli-7`.


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
'a                  // type variable
List<Int>           // list of integers
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

### Classes
```
class Foo {
    let mut name: String

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

### Blocks
A block is a collection of expressions which return the result of the last
expression in the block.  They can be used in place of a single expression.
They do not create their own local scope, at least at the moment, so variables
defined inside blocks will appear in the parent scope (usually the function
the block is in).  Each expression in the block must end in a newline or
semi-colon character (or be the last expression in the block).  This applies
to the top level.
```
let is_zero = if self.x <= 0 then {
    self.x = 0
    true
} else {
    false
}
```

### Flow Control
The return value of an if statement is the result of the expression of the
clause that is evaluated.  The types of both clauses must match.  The `else`
clause can be left out as long as the true clause evaluates to Nil.
```
if x == 5 then
    "It's five"
else
    "It's not five"

match x {
    1 => "It's one"
    5 => "It's five"
    _ => "It's not one or five"
}
```

### And / Or
The keyword operators `and` and `or` have side-effects and will not execute
the second expression if the result can be determined from the first
expression.  The resulting value is the last expression that was executed.
Operands are not limited to Bool values, although that may change in future.

### Math
Infix operators are evaluated using order of operations.  Both sides of an
infix operation must be on the same line, or else a `\` character can be used
to continue the line.
```
5 + 12 * 2      // equals 29

42 * 4 \
   % 5          // equals 3
```

### Loops
```
while is_true
    println("looping")

for i in [ 1, 2, 3 ]
    println("counting " + i)
```

### Lists
```
let list1 = [ 1, 3, 6 ]
for x in list1
    println(str(x))

let list2 = new List<String>();
list2.insert(0, "Hello")
println(list2[0])
```

### Tuples
```
let tup = (1, "String", 4.5)
println(tup.1)                  // prints "String"
```

### Records
Records are like tuples but with named fields.  Record literals use the
equals sign ("=") to assign a value to a field.  Specifying a record type
uses a colon (":") to separate the field name from the type.
```
let rec = { i = 1, s = "String", r = 4.5 }
println(rec.s)

let rec: { i: Int, s: String, r: Real }
```

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

### Enums (Tagged Unions)
An enum can either have no arguments, or a tuple of arguments.  Constructing an
enum variant requires using the Resolve (::) notation.  Pattern matching is
currently the only way to get values out of a variant.  The current implementation
needs work and does not currently support nested variant types
```
enum Value =
| None
| Integer(Int)
| String(String)
| Pair(String, String)

let val = Value::String("Hey")

match val {
    Value::String(s) => println(s)
    _ => ()
}
```

### Exceptions
Exceptions aren't quite settled yet.
```
try open("file.txt") {
    e => println("Exception Occurred: " + e)
}

try {
    //...
    raise "Problem"
} catch {
    e => println(e)
}
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
A function can be declared without being implemented, and functions can
also be defined with an ABI specifier so that they are accessible to
other languages.  Only C support is currently implemented. A C function
cannot be a closure.
```
decl foo(Int) -> Int         // external molten function
decl bar(Int) -> Int / C     // external C function

fn baz(i: Int) / C {
    // molten function that can be called from C
}
```

### Linking to C
An example of writing a C file, and linking it to a molten program is shown
in `lib/libccore.c`.  When imported into a molten file and compiled with the
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


I'd be happy to hear of any additional features ideas or suggestions, if
you'd like to leave them under "Issues" on github.


