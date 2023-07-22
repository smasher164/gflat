package types2

import (
	"github.com/smasher164/gflat/ast"
)

type Env struct {
	parent  *Env
	symbols map[string]Bind // should this be a def instead of node? to indicate the kind of binding. yeah.
}

func (e *Env) AddScope() *Env {
	return NewEnv(e)
	// return &Env{
	// 	parent:  e,
	// 	symbols: make(map[string]Bind),
	// }
}

func (c *Checker) AddSymbol(env *Env, name string, bind Bind) {
	// should this be a def instead of node?
	// check for dupes?
	// check for underscore?
	// e.parent.AddSymbol(name, x)
	if name == "_" {
		return
	}
	c.unique[name] = struct{}{}
	env.symbols[name] = bind
}

func (e *Env) LookupLocal(name string) (Bind, bool) {
	b, ok := e.symbols[name]
	return b, ok
}
func (e *Env) LookupStack(name string) (b Bind, p *Env, ok bool) {
	p = e
	for p != nil {
		if b, ok = p.LookupLocal(name); ok {
			return b, p, ok
		}
		p = p.parent
	}
	return nil, nil, false
}

func NewEnv(parent *Env) *Env {
	return &Env{
		parent:  parent,
		symbols: make(map[string]Bind),
	}
}

type Bind interface {
	isBind()
}

type VarBind struct {
	Type Type
}

func (VarBind) isBind() {}

type Unresolved struct{}

func (Unresolved) isBind() {}

type ConsBind struct{}

func (ConsBind) isBind() {}

type PackageBind struct {
	Pkg *ast.Package
}

func (PackageBind) isBind() {}

type TypeBind struct {
	Def ast.Node
}

func (TypeBind) isBind() {}

type TypeVarBind struct {
}

func (TypeVarBind) isBind() {}

type BaseTypeBind struct {
	Type Base
}

func (BaseTypeBind) isBind() {}
