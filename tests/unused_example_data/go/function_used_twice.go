package main

import "fmt"

func fun0() {
	fmt.Print("{%")
}

func fun1() {
	fun0()
	fmt.Println()
	fun0()
}

func main() {
	fun1()
}
