package types

import "github.com/smasher164/gflat/parser"

type Type interface {
	// what should it be?
	Equal(Type) bool
}

var (
	_ Type = TypeVar{nil}
	_ Type = Named{}
	_ Type = Sum{}
	_ Type = Function{}
	_ Type = Tuple{}
	_ Type = Array{}
	_ Type = Slice{}
	_ Type = Map{}
	_ Type = Base(0)
	_ Type = Forall{}
)

type Forall struct {
	TypeVar TypeVar
	Type    Type
}

// Equal implements Type
func (Forall) Equal(Type) bool {
	panic("unimplemented")
}

type TypeVar struct {
	Ref *struct {
		Bound bool
		ID    string
		Type  Type
		Env   *Env
	}
}

// Equal implements Type
func (TypeVar) Equal(Type) bool {
	panic("unimplemented")
}

type Named struct {
	Name string
	Type Type
	Env  *Env
}

// Equal implements Type
func (Named) Equal(Type) bool {
	panic("unimplemented")
}

type Sum struct {
	Variants []Variant
}

// Equal implements Type
func (Sum) Equal(Type) bool {
	panic("unimplemented")
}

type Variant struct {
	Tag  string
	Type Type
	Env  *Env
}

type Function struct {
	Elements []Type
	// Env      *Env
}

// Equal implements Type
func (Function) Equal(Type) bool {
	panic("unimplemented")
}

type Tuple struct {
	Fields []Field
	// Env    *Env
}

var Unit = Tuple{}

// Equal implements Type
func (Tuple) Equal(Type) bool {
	panic("unimplemented")
}

type Field struct {
	Name string
	Type Type
	Env  *Env
}

func (f Field) IsNamed() bool {
	return f.Name != ""
}

type Array struct {
	Length  int
	Element Type
}

// Equal implements Type
func (Array) Equal(Type) bool {
	panic("unimplemented")
}

type Slice struct {
	Element Type
}

// Equal implements Type
func (Slice) Equal(Type) bool {
	panic("unimplemented")
}

type Map struct {
	Key   Type
	Value Type
}

// Equal implements Type
func (Map) Equal(Type) bool {
	panic("unimplemented")
}

//go:generate go run golang.org/x/tools/cmd/stringer -type=Base
type Base int

// Equal implements Type
func (t1 Base) Equal(t2 Type) bool {
	if t2, ok := t2.(Base); ok {
		return t1 == t2
	}
	return false
}

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

func IsTypeVar(t Type) bool {
	_, ok := t.(TypeVar)
	return ok
}

func SimpleEquals(t1, t2 Type) bool {
	// if they're both Base or Named
	if t1, ok := t1.(Base); ok {
		if t2, ok := t2.(Base); ok {
			return t1 == t2
		}
	}
	if t1, ok := t1.(Named); ok {
		if t2, ok := t2.(Named); ok {
			a, ok1 := t1.Env.LookupStack(t1.Name)
			b, ok2 := t2.Env.LookupStack(t2.Name)
			if !ok1 || !ok2 {
				panic("unreachable")
			}
			return a.Def == b.Def
			// return t1.Name == t2.Name
		}
	}
	return false
}

type InvalidType struct {
	Msg string
}

func (InvalidType) Equal(Type) bool {
	panic("unimplemented")
}

func IsTyped(n parser.Node) bool {
	_, ok := n.(TypedNode)
	return ok
}

// func GetType(n parser.Node) Type {
// 	return n.(TypedNode).Type
// }
