fn fun_0() {
    print!("{{");
    print!("%");
}

fn fun_1() {
    fun_0();
    println!();
    fun_0();
}

fn main() {
    fun_1();
}
