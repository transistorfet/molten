 
import lib.libcore

class Input {
    let mut stopflag: Bool

    fn new(self) {
        self.stopflag = false
    }

    fn loop(self) {
        while not self.stopflag {
            println("> ")
            let input = readline()

            if input == "quit" then {
                self.stopflag = true;
            }
            else {
                println(input);
            }
        }
    }
}

let input = new Input()
input.loop()

