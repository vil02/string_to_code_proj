public class Printer {
    public static void main(String[] args) {
        fun1();
    }

    static void fun0() {
        System.out.print('{');
        System.out.print('%');
    }

    static void fun1() {
        fun0();
        System.out.print('\n');
        fun0();
    }
}
