package types

import "github.com/smasher164/gflat/parser"

func Infer(n parser.Node) Type {
	switch n := n.(type) {
	case parser.File:
		panic(n)
	}
	panic("unreachable")
}
