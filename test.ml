
        import lib.libcore

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

        // TODO this once again causes a segfault; the typevar of x is not resolved correctly, I think; it only panicks instead of segfault when creating llvm type if typevars are not silently turned into ints
	//let test2 = fn x => x()
	//let test3 = fn x => x() + 1
        //puts(str(test3(fn => 3)))

        let recfoo = fn x => begin  //comment
            if x < 1 then           //comment
                1                   //comment
            else                    //comment
                x * recfoo(x - 1)   //comment
        end                         //comment
        puts(str(recfoo(5)))

        0123
        0x234
        Inf
        //Info  // checks that Inf parses correctly
        -Inf
        NaN
        true false

        2 * (10 + -3) + 5
        2 + 4 * 7 - 1 / 20  * 10 - 50 * 12
        (2 * 3) + (4 - 5)
        2 + 4 * 7 - 1 / 20  == 10 - 50 * 12

        // NOTE this sorta causes a type error, as it should
        //let ab = 123.24 * 3
        // TODO implement ~ complement op
        //123 + ~124 * 25
        123 + (124 * 25)


        class Stuff {
            fn new(self) { }

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
            fn new(self) { }

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

        let thingy = new TestClass()
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

        match x == true {
            true => x
            false => x == true
            //false => !x   // TODO this causes a parse error
            _ => false
        }

        match a {
            1 => a
            2 => a * 4
            _ => a * 16
        }

        if a > 1 then {
            puts("It's more than 1!")
            nil
        }

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
            // TODO we don't allow recursion in overloaded functions, although I don't know that that can be fixed without constrained types
            fn strnum(num: Int, suffix: String) -> String {
                strnum(num)
                "poop"
            }

            strnum(1, "px")
            strnum(1.0)
        }

	puts("thing" + "stuff\n")
        puts("STUFF".push(" things"))
        puts(strnum(12))
        puts(strnum(1.214))


        // TODO not yet implemented
        //try str(123) with
        //    _ => a * 16


        let alloc = malloc(30)

	let buffer = new Buffer[Int](5)
        buffer[0] = 124
	puts(str(buffer[0]))

        let list3 = new List[Int]()
        list3.push(4)
        list3[4] = 123
        puts(str(list3[4]))

        let list: List['thing] = [ 1, 2, 3 ]
        list[1] = 5
        puts(str(list[1]))
        //puts(str("Thing"[2]))
        let list2 = [ new TestClass(), new Stuff(), new TestClass() ]


        // TODO this doesn't compile because of a type error

        class NumList['a] extends List[Int] {
            let x: 'a = nil

        }

        class A['it] extends List['it] {
            let foo = 1.2
        }

        class B['it, 'jt] extends A['jt] {
            let bar: 'it = nil

        }

        let c = new B[Real, Int]()
        // TODO compiles but can't run because the real isn't cast to varpointer
        //c.bar = 3.2

        class Thing {
            fn new(self) { }

            let foo = fn a => {
                a * 4
            }

            let bar = fn a => [ a, a, a ]
            let arr = [ 1, 2, 3, 4 ]
            let foobar = [ fn x => x * 16, fn x => x * 100 ]

            let baz = fn self: Thing, a => {

            }
        }

        let thing = new Thing()
        let get_thing = fn => thing

        []
        [1, 2, 3 * 10 + 3]
        [ 0, 1, 2, 3 ][0]
        let arr = [ 1, 2, 3, 4 ]
        arr[2]
        //thing.arr[1]
        //get_thing().arr[2]

        // TODO casting issues
        //thing.baz(521)
        //get_thing().baz(985)
        //thing.bar()[1]
        //(thing.bar())[1]
        //get_thing().bar()[1]

        // TODO causes segfault
        //thing.foobar[2](123)
        //get_thing().foobar[2](123)

        Thing::foo(a)


        // TODO this probably shouldn't parse without a ; or \n
        thing "things"


        for x in [ 1, 2, 3 ]
            puts("Count: " + str(x))


    /*

        while a > 0     //things
            recfoo(5)
        while x
            noop

        for x in [ 1, 2, 3 ]
            x * 100


        try raise a with
            1 => a
            2 => a * 4
            _ => a * 16


        import thing.stuff

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


