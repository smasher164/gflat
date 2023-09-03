package ast

import (
	"fmt"
	"io"
	"strings"
	"text/tabwriter"
)

type Env struct {
	parent  *Env
	Symbols map[string]Bind
}

func (e *Env) AddScope() *Env {
	return NewEnv(e)
	// return &Env{
	// 	parent:  e,
	// 	symbols: make(map[string]Bind),
	// }
}

func (e *Env) Add(name string, bind Bind) {
	if name == "_" {
		return
	}
	if _, ok := e.Symbols[name]; ok {
		panic(fmt.Sprintf("duplicate symbol %q", name))
	}
	e.Symbols[name] = bind
}

func NewEnv(parent *Env) *Env {
	return &Env{
		parent:  parent,
		Symbols: make(map[string]Bind),
	}
}

type Bind interface {
	isBind()
}

func envString(buf io.Writer, e Env) {
	if e.parent != nil {
		envString(buf, *e.parent)
		fmt.Fprint(buf, "↑\n")
	}
	if len(e.Symbols) == 0 {
		fmt.Fprintf(buf, "(empty)\n")
	} else {
		for name, bind := range e.Symbols {
			fmt.Fprintf(buf, "%s:\t%#v\n", name, bind)
		}
	}
}

func (e Env) String() string {
	sb := new(strings.Builder)
	buf := tabwriter.NewWriter(sb, 0, 0, 1, ' ', 0)
	envString(buf, e)
	buf.Flush()
	s := sb.String()
	return s
}

var Universe = func() *Env {
	e := NewEnv(nil)
	for name, base := range BaseMap {
		e.Add(name, TypeBind{Type: base})
	}
	e.Add("true", VarBind{Type: Bool})
	e.Add("false", VarBind{Type: Bool})
	return e
}()

type VarBind struct {
	Type Type
}

func (VarBind) isBind() {}

type Type interface {
	isType()
}

type TypeBind struct {
	// Def ast.Node
	// ReifiedType Type
	Type Type
}

func (TypeBind) isBind() {}

type PackageBind struct{}

func (PackageBind) isBind() {}

type UnresolvedImportedBind struct{}

func (UnresolvedImportedBind) isBind() {}

type Base int

const (
	Bool Base = iota
	Int
	Int8
	Int16
	Int32
	Int64
	Uint
	Uint8
	Uint16
	Uint32
	Uint64
	Float32
	Float64
	String

	Byte = Uint8
	Rune = Int32
)

func (b Base) String() string {
	switch b {
	case Bool:
		return "bool"
	case Int:
		return "int"
	case Int8:
		return "int8"
	case Int16:
		return "int16"
	case Int32:
		return "int32"
	case Int64:
		return "int64"
	case Uint:
		return "uint"
	case Uint8:
		return "uint8"
	case Uint16:
		return "uint16"
	case Uint32:
		return "uint32"
	case Uint64:
		return "uint64"
	case Float32:
		return "float32"
	case Float64:
		return "float64"
	case String:
		return "string"
	default:
		panic("unreachable")
	}
}

func (Base) isType() {}

var BaseMap = map[string]Base{
	"bool":    Bool,
	"int":     Int,
	"int8":    Int8,
	"int16":   Int16,
	"int32":   Int32,
	"int64":   Int64,
	"uint":    Uint,
	"uint8":   Uint8,
	"uint16":  Uint16,
	"uint32":  Uint32,
	"uint64":  Uint64,
	"float32": Float32,
	"float64": Float64,
	"string":  String,
	"byte":    Byte,
	"rune":    Rune,
}
