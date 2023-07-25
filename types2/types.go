package types2

import "github.com/smasher164/gflat/ast"

type Type interface{}

type Tuple struct {
	Fields []Field
}

var Unit = Tuple{}

type Field struct {
	Name *ast.Ident // should this be an ident?
	Type Type
}

func (f Field) IsNamed() bool {
	return f.Name != nil
}

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

type refTypeArg = struct {
	Bound bool
	ID    *ast.TypeArg
	Type  Type
}

type TypeVar struct {
	Ref *refTypeArg
}

func NewTypeVar(name *ast.TypeArg) TypeVar {
	return TypeVar{
		Ref: &refTypeArg{
			Bound: false,
			ID:    name,
			Type:  nil,
		},
	}
}

type Named struct {
	Name *ast.Ident
	Type Type
}
