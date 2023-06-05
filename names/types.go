package names

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

type Base int

// Equal implements Type
func (Base) Equal(Type) bool {
	panic("unimplemented")
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
