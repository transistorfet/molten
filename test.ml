
        import lib.libcore

        let msg = "Hey"
        // TODO this is actually a closure, which isn't implemented yet
        //let stringstuff = fn x: String => msg
        println(msg)
        println(str(10))

        2 + 5

        let e = 3
        if e < 1 and e > 4 then
            1
        else
            e

/*
        /* this ends up causing various problems */
        (12).add(3)
        e.add(3)
        fn test(x: Int) => x.add(2)
*/

        let a = 5 * 5
        let b : Real = 123.24

        let x = 2 + 4 * 7 - 1 / 20  == 10 - 50 * 12

        let testy = fn x => x + 1
        // TODO this wasn't compiling because of the default arguments not being compiled correctly (possibly just missing atm?)
        //let fac = fn (x : Int, y = 5 * 3, z: String = "hey") => if not x then 1 else x - -123
        // TODO this wasn't compiling because it could not resolve the type of x, so couldn't disambiguate "not"
        //let fac2 = fn x, y, z => if not x then 1 else x - -123
        //fac2(3)
        testy(3)

	let test2 = fn x => x()
	let test3 = fn x => x() + 1
        println(str(test3(fn => 3)))

        /*
        // TODO why does this work, when the recursive reference requires a closure?  I guess when compiling, I'm somehow
        //  making an exception for accessing functions by name instead of accessing the variable the name is actually refering to
        let recfoo = fn x => begin  //comment
            if x < 1 then           //comment
                1                   //comment
            else                    //comment
                x * recfoo(x - 1)   //comment
        end                         //comment
        println(str(recfoo(5)))
        */

        0123
        0x234
        Inf
        //Info  // checks that Inf parses correctly, and that Info doesn't
        -Inf
        NaN
        true false
        ()

        2 * (10 + -3) + 5
        2 + 4 * 7 - 1 / 20  * 10 - 50 * 12
        (2 * 3) + (4 - 5)
        2 + 4 * 7 - 1 / 20  == 10 - 50 * 12
        123 + ~124 * 25
        123 + (124 * 25)
        (1, 2)

        begin 123 * 342 end

        begin
            a
        end

        {
            a
            { b }
        }

        println("Rem: " + str(10 % 3))
        println("2^16: " + str(2.0 ^ 16.0))
        println("0xff & 0x80: " + str(0xff & 0x80))
        println("0x00 | 0x20: " + str(0x00 | 0x20))
        println("Com: " + hex(~0x5555555555555555))

        // NOTE this correctly causes a type error (actually an overload/no variant error)
        //let ab = 123.24 * 3


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

            let mut a = 1
            let b: String = "aoeu"

            fn virtfun(self) {
                println("I'm Stuff")
            }
        }

        let s = new Stuff()
        println(str(s.foo2(123)))

        class TestClass extends Stuff {
            fn new(self) { }

            let bar = fn self, x: Int => {
                self.foo2(x)
            }

            fn foo2(self, a, b) {
                a * 4.0 * b
            }

            let add = fn self, x => {
                self.a = self.a + x
            }

            fn virtfun(self) {
                println("I'm TestClass")
            }
        }

        let thingy = new TestClass()
        thingy.a = 1337
        ////thingy.add(124)     // this method is not initialized
        println(str(thingy.a))
        thingy.virtfun()

        let stuffy: Stuff = thingy
        stuffy.virtfun()
        fn vtest(s: Stuff) {
            s.virtfun()
        }
        vtest(thingy)

        while thingy.a > 0 {
            println("Hey again " + str(thingy.a))
            thingy.a = thingy.a - 1000
        }

        let r = match a {
            1 => a
            2 => a * 4
            _ => a * 16
	}
        println(str(r))

        // TODO this doesn't work without brackets around the if statement
        //let h = 5 + if r > 4 then 5 else 10

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
            println("It's more than 1!")
        }

        let d = if a > 1 then {
            a;   
        } else {
            a * -1;
        }
        println("semicolon: " + str(d))

        fn strnum(num: Int) -> String {
            let buffer: String = malloc(22)
            sprintf(buffer, "%d", num, nil)
            buffer
        }

        fn strnum(num: Real) -> String {
            let buffer: String = malloc(22)
            sprintf(buffer, "%f", num, nil)
            buffer
        }


        // TODO this causes a segfault during codegen
        //fn overload() / C {
        //    let test = "Thing"
        //    println(test)

        //    fn strnum(num: Int, suffix: String) -> String {
        //        strnum(num) + suffix
        //    }

        //    println(strnum(1, "px"))
        //    println(strnum(1.0))
        //}
        //overload()

        // TODO these don't work because it cannot be disambiguated during the recursive invoke
        //fn recoverload(x) {
        //    if x <= 0 then
        //        1
        //    else
        //        recoverload(x - 1)
        //}
        //
        //fn recoverload(x) {
        //    if x <= 0.0 then
        //        1.0
        //    else
        //        recoverload(x - 1.0)
        //}

	println("thing" + "stuff\n")
        println("STUFF".push(" things"))
        println(strnum(12))
        println(strnum(1.214))


        /// Exceptions

        // TODO not yet implemented
        //try str(123) with
        //    _ => a * 16

        //try raise a with
        //    1 => a
        //    2 => a * 4
        //    _ => a * 16



        /// Lists, Buffers, Alloc

        let alloc = malloc(30)

	let buffer = new Buffer<Int>(5)
        buffer[0] = 124
	println(str(buffer[0]))

        let list3 = new List<Int>()
        list3.push(4)
        list3.insert(1, 5)
        list3[1] = 123
        println(str(list3[1]))

        let list: List<'thing> = [ 1, 2, 3 ]
        list[1] = 5
        println(str(list[1]))
        println(str("Thing"[2]))
        // TODO this isn't compiling correctly (expected Stuff found TestClass, seems odd)
        //let list2 = [ new TestClass(), new Stuff(), new TestClass() ]

        class NumList<'a> extends List<Int> {
            let x: 'a = nil

        }

        class A<'it> extends List<'it> {
            let foo = 1.2

            fn test(self) {

            }
        }

        class B<'it, 'jt> extends A<'jt> {
            let mut bar: 'it = nil

            fn test2(self) {
                Super::test(self)
            }
        }

        let c = new B<Real, Int>()
        c.bar = 3.2

        let ftest = [ fn x => x * 16, fn x => x * 100 ]

        class Thing {
            fn new(self) {
                self.bar = fn a => [ a, a, a ]
                self.arr = [ 1, 2, 3, 4 ]
                self.foobar = [ fn x => x * 16, fn x => x * 100 ]
            }

            fn foo(a) {
                a * 4
            }

            let mut bar = fn a => [ a, a, a ]
            let mut arr: List<Int>
            let mut foobar: List<(Int) -> Int>
            //let arr = [ 1, 2, 3, 4 ]
            //let foobar = [ fn x => x * 16, fn x => x * 100 ]

            fn baz(self: Thing, a) {

            }
        }

        let thing = new Thing()
        let get_thing = fn => thing

        []
        [1, 2, 3 * 10 + 3]
        [ 0, 1, 2, 3 ][0]
        let arr = [ 1, 2, 3, 4 ]
        arr[2]
        thing.arr[1]
        get_thing().arr[2]

        thing.baz(521)
        get_thing().baz(985)
        thing.bar()[1]
        (thing.bar())[1]
        get_thing().bar()[1]
        thing.baz

        thing.foobar[1](123)
        get_thing().foobar[1](123)

        Thing::foo(a)


        // TODO this probably shouldn't parse without a ; or \n, but it does
        thing "things"



        /// For Loops

        let numbers = [ 1, 2, 3 ]
        numbers.insert(1, 5)
        for x in numbers
            println("Count: " + str(x))


        /// Type Defs

        type newint = {
            things: Int,
            stuff: Int
        }

        type newreal = Real
        let ni: newint = { things = 1, stuff = 2 }
        println(str(ni.stuff))

        /*
        // TODO these features wont be supported after all
        // these are now records instead of structs and cannot be assigned to at the moment, because literal values are not mutable
        ni.things = 745
        println(str(ni.things))

        fn thi(a: newint) {
            //a.things = 5
        }
        */


        /// Tuples

        let t = (1, 3, 5)
        println("Tuple #2: " + str(t.1))

        /// Records

        let nr = { a = 1, b = 4 }
        println("Record a: " + str(nr.b))


        /// References

        let rr = ref 999
        let v = !rr
        println(str(v))


/*
        /// Closures

        fn test() {
            let a = 98899
            fn closure() {
                a
            }
        }

        println(str(test()()))
*/



        // TODO parse test?
        //thing.stuff() * ~foo().bar

        // things
        /* stuff */
        4 + /* things */ 5


