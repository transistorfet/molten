
/*
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
*/

pub fn ==(str1: String, str2: String) -> Bool / MF {
    if strcmp(str1, str2) == 0 then
        true
    else
        false
}

pub fn !=(str1: String, str2: String) -> Bool / MF {
    if strcmp(str1, str2) != 0 then
        true
    else
        false
}

pub fn +(s1: String, s2: String) -> String / MF {
    //s1.push(s2)
    let s1length = strlen(s1)
    let s2length = strlen(s2)
    let buffer: String = malloc(s1length + s2length + 1)
    sprintf(buffer, "%s%s", s1, s2)
    buffer
}


pub fn str(unit: ()) -> String {
    "()"
}

pub fn str(num: Bool) -> String {
    if num then
        "true"
    else
        "false"
}

pub fn str(num: Int) -> String {
    let buffer: String = malloc(22)
    sprintf(buffer, "%ld", num, ())
    buffer
}

pub fn hex(num: Int) -> String {
    let buffer: String = malloc(22)
    sprintf(buffer, "0x%lX", num, ())
    buffer
}

pub fn str(num: Real) -> String {
    let buffer: String = malloc(22)
    sprintf(buffer, "%f", num, ())
    buffer
}


class Exception {
    let mut msg = ""

    fn new(self, msg) {
        self.msg = msg
    }

    fn print(self) {
        println(self.msg)
    }
}


class Option<'item> {
    let mut has = false
    let mut item: 'item = nil

    fn new(self) { }

    fn Some(item: 'item) {
        let opt = new Option<'item>()
        opt.has = true
        opt.item = item
        opt
    }

    fn None() {
        let opt = new Option<'item>()
        opt.has = false
        opt.item = nil
        opt
    }

    fn unwrap(self) -> 'item {
        if not self.has then {
            raise "Panic: unwrap on none"
            // TODO this is here because raise returns unit instead of being marked as 'no return' somehow
            self.item
        }
        else
            self.item
    }

    fn unwrap_or(self, item) {
        if self.has then
            self.item
        else
            item
    }

    fn is_some(self) {
        self.has
    }

    fn is_none(self) {
        not self.has
    }
}

class Iterator<'item> {
    fn next(self) -> Option<'item> {
        Option::None()
    }

    fn reset(self) => ()
}

class List<'item> {
    let mut capacity: Int
    let mut length: Int
    let mut data: Buffer<'item> = nil

    fn new(self) {
        self.length = 0
        self.capacity = 10
        //self.data = new Buffer<'item>(self.capacity)
        self.data = bufalloc(self.capacity)
    }

    fn len(self) {
        self.length
    }

    fn resize(self, capacity) {
        self.capacity = capacity
        self.data = bufresize(self.data, self.capacity);
    }

    fn push(self, item: 'item) {
        if self.length + 1 >= self.capacity then
            self.resize(self.capacity + 10)
        bufset(self.data, self.length, item)
        self.length = self.length + 1
    }

    fn [](self, index: Int) -> 'item {
        if index >= self.length then
            nil //raise -1
        else
            bufget(self.data, index)
    }

    fn [](self, index: Int, item: 'item) -> 'item {
        if index >= self.length then
            nil //raise "IndexError: array index is out of bounds"
        else {
            bufset(self.data, index, item);
            item
        }
    }

    fn get(self, index: Int) {
        self[index]
    }

    /*
    fn insert(self, index: Int, item: 'item) {
        self.move_right(index, item)
    }

    fn move_right(self, index: Int, item: 'item) {
        if index >= self.length then {
            self.push(item)
        } else {
            let cur = bufget(self.data, index)
            bufset(self.data, index, item)
            self.move_right(index + 1, cur)
        }
    }
    */

    fn insert(self, index: Int, item: 'item) {
        if index >= self.length then {
            self.push(item);
            // TODO this is here because of a bug during the type refactor that was creating invalid IR fur the return type
        } else {
            let cur = bufget(self.data, index)
            bufset(self.data, index, item)
            self.insert(index + 1, cur)
        }
    }

    // TODO can't forward reference ListIterator
    //fn iter() {
    //    new ListIterator<'item>(self)
    //}

    // TODO add map, filter, reduce? Or should they go on the iterator parent class?
}

class ListIterator<'item> {
    let mut index = 0
    let mut list: List<'item> = nil

    fn new(self, list) {
        self.index = 0
        self.list = list
    }

    fn next(self) -> Option<'item> {
        if self.index < self.list.len() then {
            let item = self.list[self.index]
            self.index = self.index + 1
            Option::Some(item)
        } else {
            Option::None()
        }
    }

    fn reset(self) {
        self.index = 0;
    }
}

/*
class HashMapNode<'item> {
    let mut key: String
    let mut data: 'item
    let mut next: HashMapNode<'item>

    fn new(self, key, data) {
        self.key = key
        self.data = data
        self.next = nil
    }
}

class HashMap<'item> {
    let mut size = 0
    let mut data: Buffer<HashMapNode<'item>> = nil

    fn new(self) {
        self.size = 10
        self.data = new Buffer<HashMapNode<'item>>(self.size)
    }

    fn resize(self, size) {
        self.size = size
        self.data = self.data.resize(self.size)
    }

    fn insert(self, key: String, item: 'item) {

    }



    fn hash(key: String) -> Int {

    }
}
*/


