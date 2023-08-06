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

// TODO: replace with this
// type NominalType struct {
// 	Package string
// 	Name    string
// 	Type    Type
// }

type Sum struct {
	Variants []Variant
}

type Variant struct {
	Tag      *ast.Ident // for provenance
	ConsName string
	Type     Type
}

type PatternState struct {
	Covered  bool
	Children []*PatternState
}

func newPatternState(t Type) *PatternState {
	switch t := t.(type) {
	case Tuple:
		ps := new(PatternState)
		for _, f := range t.Fields {
			ps.Children = append(ps.Children, newPatternState(f.Type))
		}
		return ps
	case Sum:
		ps := new(PatternState)
		for _, v := range t.Variants {
			// ps.addVariant(v.ConsName)
			ps.Children = append(ps.Children, newPatternState(v.Type))
		}
		return ps
	case Named:
		return newPatternState(t.Type)
	case TypeVar:
		if t.Ref.Bound {
			return newPatternState(t.Ref.Type)
		}
	case Base:
		return new(PatternState)
	}
	panic("unreachable")
}

// func (ps *PatternState) addVariant(s string) {
// 	if ps.Variants == nil {
// 		ps.Variants = make(map[string]struct{})
// 	}
// 	ps.Variants[s] = struct{}{}
// }
