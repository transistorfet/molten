 
import lib.libcore

class Input {
    let stopflag: Bool

    fn new(self) {
        self.stopflag = false
    }

    fn loop(self) {
        while not self.stopflag {
            println("> ")
            let input = readline()

            if input == "quit" then {
                self.stopflag = true;
                nil                     // NOTE: this is because the parser can't yet insert a nil after 
                                        // the semi-colon, and the "if" expr must return the same type
            }
            else {
                println(input)
            }
        }
    }
}

let input = new Input()
input.loop()

