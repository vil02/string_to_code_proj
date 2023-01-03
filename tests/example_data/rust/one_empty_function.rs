fn fun_0() {}

fn fun_1() {
    fun_0();
    print!("C");
}

fn main() {
    fun_1();
}
