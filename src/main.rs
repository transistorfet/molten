
#[macro_use]
extern crate nom;

mod parser;
use parser::*;

fn main() {

    println!("Hello, world!");

    let a = parse("

        2 * (10 + -3) + 5
        2 + 4 * 7 - 1 / 20  * 10 - 50 * 12
        (2 * 3) + (4 - 5)
        2 + 4 * 7 - 1 / 20  == 10 - 50 * 12

        let a = 123.24 * 3
        123 + ~124 * 25
        123 + (124 * 25)

        thing.stuff() * ~foo().bar


        let recfoo = fn x -> do
            if x < 1 then
                1
            else
                x * recfoo(x - 1)
        end
        recfoo(3)

        class Thing {
            fn thing -> {
                things * 4
            }
        }

        for x in list
            x * 100

        []
        [1, 2, 3 * 10 + 3]

        let fac = fn x : int, y = 5 * 3, z: string = \"hey\" -> if not x then 1 else x - -123

        type newint = {
            things: int,
            stuff: int
        }

        // TODO this at the end causes a parse error, but works everywhere else
        //type newfloat = float


    ".as_bytes());
    println!("{:?}", a);

    /*
        import thing.stuff

        dothings

        let a : int = 123.24

        while x
            noop
        while x > 0
            stuff(5)

        thing(5)
        // TODO this probably shouldn't parse without a ; or \n
        stuff \"things\"

        0123
        0x234
        Inf
        Info
        -Inf
        NaN
        true false
        thing.stuff() * ~foo().bar

        let a = 123.24
        // comments
        123 + 124 * 25
        let fac = fn x, y -> if not x then 1 else x - -123
        fac(3)

        do
            stuff
        end

        {
            stuff
            { things }
        }

        match x == true with
            1 -> x
            2 -> x * 4
            _ -> x * 16



        let a = 123.24 * 3
        123 + 124 * 25
        123 + (124 * 25)
        do 123 * 342 end
    */

    traverse(a.unwrap().1);
}


fn traverse(tree: Vec<AST>) {
    for node in tree {
        println!("{:?}", node);
        match node {
            AST::Block(x) => traverse(x),
            _ => ()
        }
    }
}

