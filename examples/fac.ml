
import lib.libcore

fn fac(x) {
    if x < 1 then
	1
    else
	x * fac(x - 1)
}
puts(str(fac(10))) 


