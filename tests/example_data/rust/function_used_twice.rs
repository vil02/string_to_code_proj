fn fun_b() {
    print!("{{");
    print!("%");
}

fn fun_a() {
    fun_b();
    println!();
    fun_b();
}

fn main() {
    fun_a();
}
