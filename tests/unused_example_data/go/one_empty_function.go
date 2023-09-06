package main

import "fmt"

func fun0() {
}

func fun1() {
	fun0()
	fmt.Print("C")
}

func main() {
	fun1()
}
