import ballerina/io;

function fun_0() {
}

function fun_1() {
    fun_0();
    io:print("C");
}

public function main() {
    fun_1();
}
