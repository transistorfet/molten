 
Molten
======

Molten is a programming language which borrows from the ML family of languages,
as well as from Rust and Python.  The compiler is written in Rust and uses
LLVM to generate IR which can be compiled to machine code.

The goal of this project is to create a language with a focus on pragmatism.
It is not a purely functional language, as it has some side-effects, and it
has a class-based object system with function and method overloading.


Installing
----------

You will need `rustc` and `cargo` installed.  It's recommended that you use
`rustup` to install these.  I've only tested it with rustc version 1.23.
You will also need LLVM 5.0 installed.  On Debian/Ubuntu, the packages are
`llvm-5.0`, `llvm-5.0-runtime`, and `llvm-5.0-dev`

You may need to run the following before the rust llvm package will compile:
```
sudo ln -s /usr/bin/llvm-config-5.0 /usr/bin/llvm-config
```

Running
-------

The `molten` script helps with compiling and linking IR files.  To run an example:

```
./molten run examples/fac.ml
```

This will run cargo to build the compiler if needed, then compile the fac.ml
file, as well as the libcore.ml library, link them together, and then run the
output using `lli-5.0`

The `*.ll` files contain IR code for a single file.  The `*.dec` files contain
declarations for use when importing from another file.  The `*.bc` files
contain LLVM Bitcode, which can be executed using `lli-5.0` or compiled using
`llc-5.0`


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
Nil
Bool
Byte
Int
Real
String
() -> Int           // function type
'a                  // type variable
List<Int>           // list of integers
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

fn foo(x: Int, y) -> Int => x + y    // with optional type annotations

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
    let name: String

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
    _ => "It's not five"
}
```

### Loops
```
while is_true
    println("looping")

for i in [ 1, 2, 3 ]
    println("counting " + i)
```

### Blocks
A block is a collection of statements which return the result of the last
expression in the block.  They can be used in place of a single expression.
```
let is_zero = if self.x <= 0 then {
    self.x = 0
    true
} else {
    false
}
```

### Lists
```
let list1 = [ 1, 3, 6 ]
for x in list1
    println(str(x))

let list2 = new List<String>();
list2[1] = "Hello"
println(list2[1])
```

### And / Or
The keyword operators `and` and `or` have side-effects and will not execute
the second expression if the result can be determined from the first
expression.  The resulting value is the last expression that was executed.
Operands are not limited to Bool values, although that may change in future.

### Import
```
import libcore
```

Yet To Complete
---------------

- Closure conversion is not yet fully implemented.  I haven't decided yet
  whether to make all functions be closures (and to add a way to declare a
  function as an external C or C++ function, which might be a good idea
  regardless), or to add a distinct type for closures vs non-closure functions
  (like Closure<(Int) -> Int>, or perhaps something shorter)

- Execptions haven't been implemented yet.  This somewhat relates to the
  above issue of making all functions support throwing execptions, which
  means a need to declare C/C++ functions that do not allow exceptions.

- Class members can be declared with an optional initial value, but the
  initial value is ignored.  Instead you must initialize the value in the
  class constructor.  Ideally I'd like to get it working, but given that
  the AST cannot be duplicated without side effects, and it needs to work
  with inherited members too, I haven't gotten it working yet.  The simple
  solution is to just disallow initial values in class members

- Dynamic dispatch for class methods is not yet implemented.  At the moment,
  it's possible to declare a class member that holds a function-typed value,
  with each object storing a reference to the correct subclass method in the
  class member.  It's a hack, but it works for now.

- Garbage collection is not yet implemented

