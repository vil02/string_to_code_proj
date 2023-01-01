public class Printer {
    public static void main(String[] args) {
        fun_a();
    }

    static void fun_b() {
    }

    static void fun_a() {
        fun_b();
        System.out.print('C');
    }
}
