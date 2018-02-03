 
Molten
======

Molten is a programming language which borrows heavily from the ML family of
languages.  The compiler is written in Rust and uses LLVM to generate IR which
can be compiled to machine code.


Installing
----------

You will need `rustc` and `cargo` installed.  It's recommended that you use
`rustup` to install these.  I've only tested it with rustc version 1.23.
You will also need LLVM 5.0 installed.  On Debian/Ubuntu, the packages are
`llvm-5.0`, `llvm-5.0-runtime`, and `llvm-5.0-dev`


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

puts(str(fac(10)))
```


