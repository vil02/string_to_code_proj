public class Printer {
    public static void main(String[] args) {
        fun_a();
    }

    static void fun_b() {
        System.out.print('{');
        System.out.print('%');
    }

    static void fun_a() {
        fun_b();
        System.out.print('\n');
        fun_b();
    }
}
