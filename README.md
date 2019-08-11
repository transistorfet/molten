
Molten
======

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
You will also need LLVM 7 installed.

On Debian/Ubuntu, run:
`sudo apt-get install llvm-7 llvm-7-runtime llvm-7-dev`

On macOS, run:
`brew install llvm@7`

You may need to add /usr/local/opt/llvm@7/bin to your path

Running
-------

The `molten` script helps with compiling and linking IR files.  To run an example:

```
./molten run examples/fac.ml
```

This will run cargo to build the compiler if needed, then compile the fac.ml
file, as well as the libcore.ml library, link them together, and then run the
output using `lli-7`

The `*.ll` files contain IR code for a single file.  The `*.dec` files contain
declarations for use when importing from another file.  The `*.bc` files
contain LLVM Bitcode, which can be executed using `lli-7` or compiled using
`llc-7`.

```
llc-7 -filetype=obj lib/libcore.ll
llc-7 -filetype=obj example/fac.ll
gcc -lm -no-pie example/fac.o lib/libcore.o
```
Note: the `-no-pie` flag may be required when linking or you may get an error


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
()                  // Unit Type
Nil
Bool
Byte
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
A block is a collection of statements which return the result of the last
expression in the block.  They can be used in place of a single expression.
They do not create their own local scope, at least at the moment, so variables
defined inside blocks will appear in the parent scope (usually the function
the block is in).
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
value, and dereferenced to get or set the data inside of it.  The interal value
of a reference is mutable
```
let r = ref 42
println(str(!r))                // prints 42
!r = 65
println(str(!r))                // prints 65

fn foo(x: ref Int) { }          // ref types look similar to ref constructors

let r = ref { a = 42, b = "The Answer" }
println(!r.b)                   // prints "The Answer"
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
decl foo : (Int) -> Int         // external molten function
decl bar : (Int) -> Int / C     // external C function

fn baz(i: Int) / C {
    // molten function that can be called from C
}
```


Yet To Complete
---------------

- Garbage collection is not yet implemented

- Improved pattern matching


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

I'd be happy to hear of any additional features ideas or suggestions, if
you'd like to leave them under "Issues" on github.


