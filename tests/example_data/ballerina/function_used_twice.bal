import ballerina/io;

function fun_0() {
    io:print("{");
    io:print("%");
}

function fun_1() {
    fun_0();
    io:println();
    fun_0();
}

public function main() {
    fun_1();
}
