package types

// TODO: type parameters
// TODO: packages
// TODO: type constraints and with clause

type Type interface{}

type Named struct {
	Name string
	Type Type
}

type Sum struct {
	Variants []Variant
}

type Variant struct {
	Tag  string
	Type Type
}

type Function struct {
	In  Type
	Out Type
}

type Tuple struct {
	Fields []Field
}

type Field struct {
	Name string
	Type Type
}

func (f Field) IsNamed() bool {
	return f.Name != ""
}

type Array struct {
	Length  int
	Element Type
}

type Slice struct {
	Element Type
}

type Map struct {
	Key   Type
	Value Type
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
