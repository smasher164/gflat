package names

import "github.com/smasher164/gflat/parser"

type TypedNode interface {
	parser.Node
	Type() Type
}

// Infer accepts an untyped AST and returns a typed AST.
func (r *Resolver) Infer(env *Env, n parser.Node) parser.Node {
	// switch n := n.(type) {

	// }
	panic("TODO")
}

// Check accepts an AST and a type and returns whether the AST is of that type.
func (r *Resolver) Check(n parser.Node, t bool) bool {
	panic("TODO")
}
