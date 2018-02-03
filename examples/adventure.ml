 
import lib.libcore

class Input {
    let stopflag = false

    fn new(self) {
        self.stopflag = false
    }

    fn loop(self) {
        while not self.stopflag {
            puts("> ")
            let input = readline()

            if input == "quit" then {
                self.stopflag = true
                nil
            }
            else {
                puts(input)
            }
        }
    }
}

let input = new Input()
input.loop()

