
        import libcore

        let msg = "Hey"
        let stringstuff = fn x: String => msg
        puts(msg)
        puts(str(10))

        2 + 5

        let e = 3
        if e < 1 and e > 4 then
            1
        else
            e

        (12).add(3)
        e.add(3)
        fn test(x: Int) => x.add(2)

        let a = 5 * 5
        let b : Real = 123.24

        let x = 2 + 4 * 7 - 1 / 20  == 10 - 50 * 12

        let testy = fn x => x + 1
        let fac = fn (x : Int, y = 5 * 3, z: String = "hey") => if not x then 1 else x - -123
        // TODO this causes an error because the type used in not() cannot be determined
        //let fac2 = fn x, y, z => if not x then 1 else x - -123
        testy(3)

	let test2 = fn x => x()
	let test3 = fn x => x() + 1
        puts(str(test3(fn => 3)))

        let recfoo = fn x => begin  //comment
            if x < 1 then           //comment
                1                   //comment
            else                    //comment
                x * recfoo(x - 1)   //comment
        end                         //comment
        puts(str(recfoo(5)))

        let stuff = class Stuff {
            let foo = fn self, a => {
                a * 4
            }

            fn foo2(self, a) {
                a * 4
            }

            fn foo2(self, a) {
                a * 4.0
            }

            let a = 1
            let b: String = "aoeu"

        }

        class TestClass extends Stuff {
            let bar = fn self, x => {
                self.foo2(x)
            }

            // TODO causing issues
            fn foo2(self, a, b) {
                a * 4.0 * b
            }

            let add = fn self, x => {
                self.a = self.a + x
            }
        }

        let thingy = TestClass::new()
        thingy.a = 1337
        thingy.add(124)
        puts(str(thingy.a))

        while thingy.a > 0 {
            puts("Hey again")
            thingy.a = thingy.a - 1000
        }

        let r = match a {
            1 => a
            2 => a * 4
            _ => a * 16
	}

        puts(str(r))

        fn strnum(num: Int) -> String {
            let buffer: String = malloc(22)
            sprintf(buffer, "%d", num)
            buffer
        }

        fn strnum(num: Real) -> String {
            let buffer: String = malloc(22)
            sprintf(buffer, "%f", num)
            buffer
        }

        fn overload() {
            // TODO we don't allow recursion in overloaded functions, although I don't know that that can be fixed without constricted types
            fn strnum(num: Int, suffix: String) -> String {
                strnum(num)
                "poop"
            }

            strnum(1, "px")
            strnum(1.0)
        }

	puts("thing" + "stuff\n")
        puts("FUCK".push(" shit"))
        puts(strnum(12))
        puts(strnum(1.214))

        // TODO this causes a stack overflow during resolve_type()
        //let buffer = malloc(30)

        //try str(123) with
        //    _ => a * 16


        /*
        class Thing {
            let foo = fn a => {
                a * 4
            }

            let bar = fn a => [ a, a, a ]
            let arr = [ 1, 2, 3, 4 ]
            let foobar = [ fn x => x * 16, fn x => x * 100 ]

            let baz = fn self: Thing, a => {

            }
        }

        let thing = Thing::new()
        let get_thing = fn => thing

        [ 0, 1, 2, 3 ][0]
        let arr = [ 1, 2, 3, 4 ]
        arr[2]
        thing.arr[1]
        get_thing().arr[2]
        thing.bar()[1]
        (thing.bar())[1]
        get_thing().bar()[1]
        thing.foobar[2](123)
        get_thing().foobar[2](123)
        thing.baz(521)
        get_thing().baz(985)
        Thing::foo(a)
        */

    /*

        while a > 0     //things
            recfoo(5)

        class Thing {
            fn thing => {
                a * 4
            }
        }

        Thing.thing()


        for x in [ 1, 2, 3 ]
            x * 100

        match a with
            1 => a
            2 => a * 4
            _ => a * 16

        try raise a with
            1 => a
            2 => a * 4
            _ => a * 16

        [ 0, 1, 2, 3 ][0]
 

        import thing.stuff

        dothings

        let a : int = 123.24


        while x
            noop
        while x > 0     //things
            stuff(5)

        2 * (10 + -3) + 5
        2 + 4 * 7 - 1 / 20  * 10 - 50 * 12
        (2 * 3) + (4 - 5)
        2 + 4 * 7 - 1 / 20  == 10 - 50 * 12

        let a = 123.24 * 3
        123 + ~124 * 25
        123 + (124 * 25)

        thing.stuff() * ~foo().bar

        let recfoo = fn x => begin  //comment
            if x < 1 then           //comment
                1                   //comment
            else                    //comment
                x * recfoo(x - 1)   //comment
        end                         //comment
        recfoo(3)

        let fac = fn x : int, y = 5 * 3, z: string = "hey" => if not x then 1 else x - -123

        thing(5)
        // TODO this probably shouldn't parse without a ; or \n
        stuff "things"

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
        let fac = fn x, y => if not x then 1 else x - -123
        fac(3)

        begin
            stuff
        end

        {
            stuff
            { things }
        }

        match x == true with
            1 => x
            2 => x * 4
            _ => x * 16

        match a with
            1 => a
            2 => a * 4
            _ => a * 16


        let a = 123.24 * 3
        123 + 124 * 25
        123 + (124 * 25)
        begin 123 * 342 end


        class Thing {
            fn thing => {
                things * 4
            }
        }

        for x in list
            x * 100

        []
        [1, 2, 3 * 10 + 3]

        type newint = {
            things: int,
            stuff: int
        }

        // TODO this at the end causes a parse error, but works everywhere else
        //type newfloat = float

    */

        // things
        /* stuff */
        4 + /* things */ 5

