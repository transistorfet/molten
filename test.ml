
        let msg = "Hey"
        let stringstuff = fn x: String -> msg
        puts(msg)

        2 + 5

        let e = 3
        if e < 1 then
            1
        else
            e

        let a = 5 * 5
        let b : Real = 123.24

        let x = 2 + 4 * 7 - 1 / 20  == 10 - 50 * 12

        let testy = fn x -> x + 1
        let fac = fn x : Int, y = 5 * 3, z: String = "hey" -> if not x then 1 else x - -123
        let fac2 = fn x, y, z -> if not x then 1 else x - -123
        testy(3)

        // TODO this causes a segfault
	//let test = fn x -> x()
        // TODO this causes a typeerror
	//let test = fn x -> x() + 1

        let recfoo = fn x -> begin  //comment
            if x < 1 then           //comment
                1                   //comment
            else                    //comment
                x * recfoo(x - 1)   //comment
        end                         //comment
        recfoo(3)


    /*

        while a > 0     //things
            recfoo(5)

        class Thing {
            fn thing -> {
                a * 4
            }
        }

        Thing.thing()


        for x in [ 1, 2, 3 ]
            x * 100

        match a with
            1 -> a
            2 -> a * 4
            _ -> a * 16

        try raise a with
            1 -> a
            2 -> a * 4
            _ -> a * 16

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

        let recfoo = fn x -> begin  //comment
            if x < 1 then           //comment
                1                   //comment
            else                    //comment
                x * recfoo(x - 1)   //comment
        end                         //comment
        recfoo(3)

        let fac = fn x : int, y = 5 * 3, z: string = "hey" -> if not x then 1 else x - -123

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
        let fac = fn x, y -> if not x then 1 else x - -123
        fac(3)

        begin
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

        match a with
            1 -> a
            2 -> a * 4
            _ -> a * 16


        let a = 123.24 * 3
        123 + 124 * 25
        123 + (124 * 25)
        begin 123 * 342 end


        class Thing {
            fn thing -> {
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
        4 * /* things */ 5

