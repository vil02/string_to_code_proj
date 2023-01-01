fn fun_b() {}

fn fun_a() {
    fun_b();
    print!("C");
}

fn main() {
    fun_a();
}
