//go:ignore build
package main

import "fmt"

var x = foo(1)

func foo(y int) int {
	return bar(y)
}

var y, _ = fmt.Println("world")

func bar(_ int) int {
	return foo(x)
}

func main() {
	var foo func()
	var bar func()
	foo = func() {
		bar()
	}
	bar = func() {
		foo()
	}
}
