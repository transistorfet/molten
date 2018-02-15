
class String {
    fn [](self, index: Int) {
        getindex(self, index)
    }

    fn push(self, s) {
        let selflength = strlen(self)
        let slength = strlen(s)
        let buffer: String = malloc(selflength + slength + 1)
        sprintf(buffer, "%s%s", self, s)
        buffer
    }

    // TODO this doesn't handle UTF-8
    fn substr(self, start: Int, length: Int) {
        let newstr: String = malloc(length + 1)
        // TODO this doesn't work because you can't get a pointer to an indexed position
        memcpy(newstr, self, length)
        //newstr[length] = 0
    }
}

fn ==(str1: String, str2: String) -> Bool {
    if strcmp(str1, str2) == 0 then
        true
    else
        false
}

fn +(s1: String, s2: String) -> String {
    s1.push(s2)
}


fn str(num: Int) -> String {
    let buffer: String = malloc(22)
    sprintf(buffer, "%ld", num)
    buffer
}

fn hex(num: Int) -> String {
    let buffer: String = malloc(22)
    sprintf(buffer, "0x%lX", num)
    buffer
}

fn str(num: Real) -> String {
    let buffer: String = malloc(22)
    sprintf(buffer, "%f", num)
    buffer
}




class List<'item> {
    let capacity = 0
    let length = 0
    let data: Buffer<'item> = nil

    fn new(self) {
        self.length = 0
        self.capacity = 10
        self.data = new Buffer<'item>(self.capacity)
    }

    fn len(self) {
        self.length
    }

    fn resize(self, capacity) {
        self.capacity = capacity
        self.data = self.data.resize(self.capacity)
        nil
    }

    fn push(self, item: 'item) {
        if self.length + 1 >= self.capacity then
            self.resize(self.capacity + 10)
        self.data[self.length] = item
        self.length = self.length + 1
    }

    fn [](self, index: Int) {
        if index >= self.length then
            nil //raise -1
        else
            self.data[index]
    }

    fn [](self, index: Int, item: 'item) {
        if index >= self.length then
            nil //raise "IndexError: array index is out of bounds"
        else {
            self.data[index] = item
        }
    }

    fn get(self, index: Int) {
        self[index]
    }

    fn insert(self, index: Int, item: 'item) {
        self.move_right(index, item)
    }

    fn move_right(self, index: Int, item: 'item) {
        if index >= self.length then {
            self.push(item)
        } else {
            let cur = self.data[index]
            self.data[index] = item
            self.move_right(index + 1, cur)
        }
    }
}


/*
class HashMap<'item> {
    let size = 0
    let data: Buffer<'item> = nil

    fn new(self) {
        self.size = 10
        self.data = new Buffer<'item>(self.size)
    }

    fn resize(self, size) {
        self.size = size
        self.data = self.data.resize(self.size)
    }

}
*/

