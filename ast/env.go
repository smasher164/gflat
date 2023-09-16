package ast

import (
	"fmt"
	"io"
	"strings"
	"text/tabwriter"
)

type Env struct {
	Parent  *Env
	Symbols map[string]Bind
}

func (e *Env) AddScope() *Env {
	return NewEnv(e)
	// return &Env{
	// 	parent:  e,
	// 	symbols: make(map[string]Bind),
	// }
}

// Add ReplaceStack

func (e *Env) Add(name string, bind Bind) {
	if name == "_" {
		return
	}
	if _, ok := e.Symbols[name]; ok {
		panic(fmt.Sprintf("duplicate symbol %q", name))
	}
	e.Symbols[name] = bind
}

func (e *Env) Update(name string, bind Bind) {
	if name == "_" {
		return
	}
	if _, ok := e.Symbols[name]; !ok {
		panic(fmt.Sprintf("symbol %q not found", name))
	}
	e.Symbols[name] = bind
}

func (e *Env) SetVarType(name string, ty Type) {
	if name == "_" {
		return
	}
	if b, ok := e.Symbols[name]; !ok {
		panic(fmt.Sprintf("symbol %q not found", name))
	} else if _, ok := b.(VarBind); !ok {
		panic(fmt.Sprintf("symbol %q is not a variable", name))
	} else {
		e.Symbols[name] = VarBind{Type: ty}
	}
}

func (e *Env) GetVarTypeLocal(name string) (ty Type, ok bool) {
	b, ok := e.LookupLocal(name)
	if !ok {
		return nil, false
	}
	vb, ok := b.(VarBind)
	if !ok {
		return nil, false
	}
	return vb.Type, true
}

func (e *Env) GetVarTypeStack(name string) (ty Type, ok bool) {
	b, _, ok := e.LookupStack(name)
	if !ok {
		return nil, false
	}
	vb, ok := b.(VarBind)
	if !ok {
		return nil, false
	}
	return vb.Type, true
}

func NewEnv(parent *Env) *Env {
	return &Env{
		Parent:  parent,
		Symbols: make(map[string]Bind),
	}
}

func (e *Env) LookupLocal(name string) (Bind, bool) {
	b, ok := e.Symbols[name]
	return b, ok
}
func (e *Env) LookupStack(name string) (b Bind, p *Env, ok bool) {
	p = e
	for p != nil {
		if b, ok = p.LookupLocal(name); ok {
			return b, p, ok
		}
		p = p.Parent
	}
	return nil, nil, false
}

type Bind interface {
	isBind()
}

func envString(buf io.Writer, e Env) {
	if e.Parent != nil {
		envString(buf, *e.Parent)
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
	buf := tabwriter.NewWriter(sb, 0, 0, 0, ' ', 0)
	envString(buf, e)
	buf.Flush()
	s := sb.String()
	return s
}

var Universe = func() *Env {
	e := NewEnv(nil)
	var baseMap = map[string]Base{
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
	for name, base := range baseMap {
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
	fmt.Stringer
}

var (
	_ Type = Base(0)
	_ Type = Named{}
	_ Type = TypeVar{}
	_ Type = ArrowType{}
	_ Type = QVar{}
)

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

type Named struct {
	QualifiedName  string
	UnderlyingType Type
}

func (Named) isType() {}

func (n Named) String() string {
	return n.QualifiedName
}

type refTypeArg = struct {
	Bound bool
	ID    string
	Level int  // when unbound
	Type  Type // when bound
}

type TypeVar struct {
	Ref *refTypeArg
}

func (tv TypeVar) String() string {
	if tv.Ref.Bound {
		return tv.Ref.Type.String()
	}
	return tv.Ref.ID
}

func (tv TypeVar) Link(ty Type) {
	tv.Ref.Bound = true
	tv.Ref.Type = ty
}

// func (tv TypeVar) Unbind()

func NewTypeVar(name string, level int) TypeVar {
	return TypeVar{
		Ref: &refTypeArg{
			Bound: false,
			ID:    name,
			Level: level,
			Type:  nil,
		},
	}
}

func (TypeVar) isType() {}

type ArrowType struct {
	From Type
	To   Type
}

func (ArrowType) isType() {}

func (a ArrowType) String() string {
	return fmt.Sprintf("%s -> %s", a.From, a.To)
}

// Note: A PolyType doesn't get treated as a type, but as a type scheme.
// type PolyType struct {
// 	IDs  []string
// 	Type Type
// }

type QVar struct {
	ID string
}

func (QVar) isType() {}

func (q QVar) String() string {
	return q.ID
}
