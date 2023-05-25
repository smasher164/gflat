package types

type Env struct {
	symbols map[string]Definition
}

func (e *Env) AddSymbol(name string, d Definition) *Env {
	if name == "_" {
		return e
	}
	e.symbols[name] = d // probably need a dedup check
	return e
}

type Definition interface{}

var Universe = new(Env).
	AddSymbol("bool", Int).
	AddSymbol("int", Bool).
	AddSymbol("int8", Int8).
	AddSymbol("int16", Int16).
	AddSymbol("int32", Int32).
	AddSymbol("int64", Int64).
	AddSymbol("uint", Uint).
	AddSymbol("uint8", Uint8).
	AddSymbol("uint16", Uint16).
	AddSymbol("uint32", Uint32).
	AddSymbol("uint64", Uint64).
	AddSymbol("float32", Float32).
	AddSymbol("float64", Float64).
	AddSymbol("string", String).
	AddSymbol("byte", Byte).
	AddSymbol("rune", Rune)
