package types2

import (
	"errors"
	"fmt"
	"strconv"

	"github.com/smasher164/gflat/ast"
	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
	"golang.org/x/exp/slices"
)

// we don't need to determine init order, since the go compiler will handle that part.
// we are 1:1 with go anyway. we just need to ensure no cycles.

type Checker struct {
	importer *parser.Importer
	err      error
	envOf    map[ast.Node]*Env
	fileOf   map[string]*Env
	typeOf   map[ast.Node]Type   // basically a memoized infer
	unique   map[string]struct{} // TODO: this should be per-package i think
	universe *Env
	// intern table for constructed types.
}

// issue is that external users need access to the package checker to generate fresh names per package.
// is it worth it?
type packageChecker struct {
	*Checker
	unique map[string]struct{}
}

func newPackageChecker(c *Checker) *packageChecker {
	return &packageChecker{
		Checker: c,
		unique:  make(map[string]struct{}),
	}
}

func NewChecker(importer *parser.Importer) *Checker {
	c := &Checker{
		importer: importer,
		envOf:    make(map[ast.Node]*Env),
		fileOf:   make(map[string]*Env),
		typeOf:   make(map[ast.Node]Type),
		unique:   make(map[string]struct{}),
	}
	universe := NewEnv(nil)
	for name, ty := range BaseMap {
		c.AddSymbol(universe, name, BaseTypeBind{ty})
	}
	c.AddSymbol(universe, "true", BaseVarBind{Bool})
	c.AddSymbol(universe, "false", BaseVarBind{Bool})
	c.universe = universe
	return c
}

func (c *Checker) SetEnv(x ast.Node, env *Env) {
	c.envOf[x] = env
}

func (c *Checker) GetEnv(x ast.Node) (*Env, bool) {
	env, ok := c.envOf[x]
	return env, ok
}

func (c *Checker) GetType(x ast.Node) Type {
	return c.get(c.typeOf[x])
}

func (c *Checker) TryGetType(x ast.Node) (Type, bool) {
	if t, ok := c.typeOf[x]; ok {
		return c.get(t), true
	}
	return nil, false
}

func (c *Checker) ProcessBuild() error {
	for _, path := range c.importer.Sorted {
		// c.err = errors.Join(c.err, err) just do this when we get the rror
		c.resolve(c.universe, c.importer.PkgCache[path])
		// ^ after resolution, all mutually recursive definitions, environments, and external definitions should be resolved.
		c.infer(c.importer.PkgCache[path])
	}
	return c.err
}

func underlying(t Type) Type {
	if t, ok := t.(Named); ok {
		return underlying(t.Type)
	}
	if t, ok := t.(TypeVar); ok && t.Ref.Bound {
		return underlying(t.Ref.Type)
	}
	return t
}

func (c *Checker) inferPat(ps *PatternState, tH Type, pat ast.Node) {
OUTER:
	switch pat := pat.(type) {
	case *ast.TypeAnnotation:
		panic("TODO TypeAnnotation")
	case *ast.BinaryExpr:
		panic("TODO infer Or patterns")
		// c.inferPat(ps, pat.Left)
		// c.inferPat(ps, pat.Right)
	case *ast.CallExpr:
		switch caller := pat.Elements[0].(type) {
		case *ast.SelectorExpr:
			if _, tX, ok := c.CheckTypeSel(caller.X); ok {
				if tX, ok := tX.(Named); ok {
					if sum, ok := tX.Type.(Sum); ok {
						c.unify(tH, tX)
						// check that caller.Name is a valid constructor
						i := slices.IndexFunc(sum.Variants, func(v Variant) bool { return v.Tag.Name.Data == caller.Name.Name.Data })
						if i < 0 {
							panic(fmt.Sprintf("constructor %s not found in type %s", caller.Name.Name.Data, tX.Name.Name.Data))
						}
						variant := sum.Variants[i]
						c.inferPat(ps.Children[i], variant.Type, pat.Elements[1])
						// ps.Children[i].Covered = true
						// this only make sense if type is covered too.
						// still need to validate that all are covered

						// if len(ps.Variants) == 0 {
						// 	ps.Covered = true
						// }

						// now we typecheck the application
						// there shouldn't be more than 2 elements in the call unless there's generics right?
						// c.check(pat.Elements[1], variant.Type)
						// the type of the entire expression should be the type of the sum

						// c.typeOf[pat] = tX

						// we're done here
						break OUTER
					}
				}
			}
		case *ast.Ident:
			panic("TODO")
		}
		// c.resolve(env, pat.Elements[0])
		// for i := 1; i < len(pat.Elements); i++ {
		// 	c.resolvePattern(env, pat.Elements[i])
		// }
		// if caller, ok := pat.Elements[0].(*ast.SelectorExpr); ok {
		// 	switch sel := caller.X.(type) {
		// 	case *ast.Ident:
		// 		if b, _, ok := env.LookupStack(sel.Name.Data); ok {
		// 			switch b.(type) {
		// 			case TypeBind, BaseTypeBind:
		// 				consName := sel.Name.Data + "_" + caller.Name.Name.Data
		// 				if bc, _, ok := env.LookupStack(consName); ok {
		// 					if bc, ok := bc.(ConsBind); ok {
		// 						bc.
		// 					}
		// 				}
		// 			}
		// 		}
		// 	case *ast.SelectorExpr:
		// 	}
		// case *ast.Ident:
		// 	if e, ok := c.GetEnv(x); ok {
		// 		if b, _, ok := e.LookupStack(x.Name.Data); ok {
		// 			switch b := b.(type) {
		// 			case TypeBind:
		// 				return b, c.GetType(b.Def), true
		// 			case BaseTypeBind:
		// 				return b, b.Type, true
		// 			}
		// 		}
		// 	}
		// case *ast.SelectorExpr:
		// 	if _, def, ok := c.CheckPackageDef(x); ok {
		// 		switch def := def.(type) {
		// 		case TypeBind:
		// 			return def, c.GetType(def.Def), true
		// 		}
		// 	}
		// }
		// }
	case *ast.SelectorExpr:
		if _, tX, ok := c.CheckTypeSel(pat.X); ok {
			if tX, ok := tX.(Named); ok {
				if sum, ok := tX.Type.(Sum); ok {
					c.unify(tX, tH)
					// check that caller.Name is a valid constructor
					i := slices.IndexFunc(sum.Variants, func(v Variant) bool { return v.Tag.Name.Data == pat.Name.Name.Data })
					if i < 0 {
						panic(fmt.Sprintf("constructor %s not found in type %s", pat.Name.Name.Data, tX.Name.Name.Data))
					}
					variant := sum.Variants[i]
					c.unify(Unit, variant.Type)
					ps.Covered = true

					// delete(ps.Variants, variant.ConsName)
					// if len(ps.Variants) == 0 {
					// 	ps.Covered = true
					// }
					// now we typecheck the application
					// there shouldn't be more than 2 elements in the call unless there's generics right?
					// c.unify(Unit, variant.Type)
					// the type of the entire expression should be the type of the sum

					// c.typeOf[pat] = tX

					// we're done here
					break
				}
			}
		}
	// could be pkg.type.cons or type.cons
	case *ast.Ident:
		if pat.Name.Data == "_" {
			ps.Covered = true
			// c.typeOf[pat] = NewTypeVar(c.FreshTypeVar())
			break
		}
		if e, ok := c.GetEnv(pat); ok {
			if b, _, ok := e.LookupStack(pat.Name.Data); ok {
				switch b.(type) {
				case ConsBind:
					panic("TODO")
					// b.ConsName // somehow get the variant and the named sum type from here?
					// tname := b.ConsName[:len(b.ConsName)-len(pat.Name.Data)-1]

				case VarBind:
					ps.Covered = true
					// c.typeOf[pat] = NewTypeVar(c.FreshTypeVar()) // is this right?
				}
			}
		}
		// if _, ok := env.LookupLocal(pat.Name.Data); ok {
		// 	panic("redefinition of binding")
		// }
		// if b, _, ok := env.LookupStack(pat.Name.Data); ok {
		// 	if _, ok := b.(ConsBind); !ok {
		// 		c.AddSymbol(env, pat.Name.Data, VarBind{pat})
		// 	}
		// }
		// we're not allowing Ident to be a constructor.
		// actually nullary constructors that specify C _ or C () will work.
	case *ast.Number:
		c.unify(tH, Int)
		// c.typeOf[pat] = Int
	case *ast.BasicString:
		c.unify(tH, String)
		// c.typeOf[pat] = String
	case *ast.InterpolatedString:
		panic("TODO: interpolated string in pattern")
	case *ast.Tuple:
		if len(ps.Children) != len(pat.Elements) {
			panic("invalid count for patter")
		}
		ttup := underlying(tH).(Tuple)
		// var f []Field
		var uncovered bool
		for i := range ps.Children {
			elem := pat.Elements[i].(*ast.CommaElement).X
			c.inferPat(ps.Children[i], ttup.Fields[i].Type, elem)
			// telem := c.typeOf[elem]
			// c.typeOf[pat.Elements[i]] = telem
			// f = append(f, Field{Type: telem})
			if !ps.Children[i].Covered {
				uncovered = true
			}
		}
		// c.typeOf[pat] = Tuple{f}
		if !uncovered {
			ps.Covered = true
		}
	}
	var uncovered bool
	for i := range ps.Children {
		if !ps.Children[i].Covered {
			uncovered = true
		}
	}
	if !uncovered {
		ps.Covered = true
	}
}

// should infer handle type declarations?
// should its type be associated in typeOf?
func (c *Checker) infer(x ast.Node) {
	if _, ok := c.TryGetType(x); ok {
		return
	}
OUTER:
	switch x := x.(type) {
	case *ast.Package:
		for _, file := range x.PackageFiles {
			c.infer(file)
		}
		if x.ScriptFile != nil {
			c.infer(x.ScriptFile)
		}
	case *ast.File:
		c.infer(x.Body)
	case *ast.Block:
		var lastType Type = Unit
		for i, elem := range x.Body {
			c.infer(elem)
			if i == len(x.Body)-1 { // we don't need to check the previous elements
				if t, ok := c.TryGetType(elem); ok {
					lastType = t
				}
			}
		}
		c.typeOf[x] = lastType
	case *ast.Stmt:
		c.infer(x.Stmt)
		// c.typeOf[x] = Unit // I don't think statements have types
	case *ast.LetDecl:
		// get partial type annotation from destructure
		c.infer(x.Destructure)
		tdes := c.GetType(x.Destructure)
		_ = tdes
		c.infer(x.Rhs)
		trhs := c.GetType(x.Rhs)
		c.unify(c.GetType(x.Destructure), trhs)
		// c.typeOf[x] = trhs
		// if destructure has a type annotation, ...
		// otherwise, set its type to trhs
	case *ast.LetFunction:
	case *ast.TypeAnnotation:
		tann := c.reifyType(x.Type)
		c.typeOf[x.Destructure] = tann
		// e := c.envOf[x.Destructure]
		// if id, ok := x.Destructure.(*ast.Ident); ok {
		// 	if b, ok := e.symbols[id.Name.Data]; ok {
		// 		if vb, ok := b.(VarBind); ok {
		// 			if vb.Type == nil {
		// 				e.symbols[id.Name.Data] = VarBind{tann}
		// 			}
		// 		}
		// 	}
		// }
		c.typeOf[x] = tann
	case *ast.Ident:
		// look up in environment
		// get its type
		e := c.envOf[x]
		if b, _, ok := e.LookupStack(x.Name.Data); ok {
			switch b := b.(type) {
			case TypeBind:
				// is this okay?
				c.typeOf[x] = c.GetType(b.Def)
				break OUTER
			case BaseTypeBind:
				// is this okay?
				c.typeOf[x] = b.Type
				break OUTER
			case BaseVarBind:
				c.typeOf[x] = b.Type
				break OUTER
			case VarBind:
				if t, ok := c.TryGetType(b.Def); ok {
					c.typeOf[x] = t
					break OUTER
				}
			}
		}
		// is this okay for regular vars that are used?
		fresh := c.FreshTypeVar()
		tx := NewTypeVar(fresh)
		c.typeOf[x] = tx
	case *ast.Number:
		c.typeOf[x] = Int
	case *ast.BasicString:
		c.typeOf[x] = String
	case *ast.IndexExpr:
		c.infer(x.X)
		tX := c.GetType(x.X)
		switch tX := tX.(type) {
		case Tuple:
			if len(x.IndexElements) != 1 {
				panic("there can only be one indexer for tuples")
			}
			idx, ok := x.IndexElements[0].(*ast.CommaElement)
			if !ok {
				panic("index must be a constant number")
			}
			num, ok := idx.X.(*ast.Number)
			if !ok {
				panic("index must be a constant number")
			}
			if !num.IsNat() {
				panic("index must be a nat")
			}
			if idx, err := strconv.Atoi(num.Lit.Data); err == nil {
				if idx >= len(tX.Fields) {
					panic("index is out of bounds of tuple")
				}
				f := tX.Fields[idx]
				c.typeOf[x.IndexElements[0]] = Int
				c.typeOf[x] = f.Type
			} else {
				panic("index is not valid integer")
			}
		default:
			panic(fmt.Sprintf("unhandled index %T", tX))
		}
	case *ast.BinaryExpr:
		switch x.Op.Type {
		case lexer.Plus:
			c.infer(x.Left)
			c.typeOf[x] = c.check(x.Right, c.GetType(x.Left))
		case lexer.Minus, lexer.Times, lexer.LeftShift, lexer.RightShift, lexer.Remainder, lexer.Divide, lexer.Or, lexer.And, lexer.Caret, lexer.Exponentiation:
			c.check(x.Left, Int)
			c.check(x.Right, Int)
			c.typeOf[x] = Int
		case lexer.LogicalAnd, lexer.LogicalOr:
			c.check(x.Left, Bool)
			c.check(x.Right, Bool)
			c.typeOf[x] = Bool
		case lexer.LogicalEquals, lexer.NotEquals:
			c.infer(x.Left)
			c.check(x.Right, c.GetType(x.Left))
			c.typeOf[x] = Bool
		case lexer.LessThan, lexer.LessThanEquals, lexer.GreaterThan, lexer.GreaterThanEquals:
			c.check(x.Left, Int)
			c.check(x.Right, Int)
			c.typeOf[x] = Bool
		default:
			panic(fmt.Sprintf("unhandled binary operator %v", x.Op))
		}
	case *ast.PrefixExpr:
		switch x.Op.Type {
		case lexer.Plus, lexer.Minus, lexer.Caret:
			c.check(x.X, Int)
			c.typeOf[x] = Int
		case lexer.Not:
			c.check(x.X, Bool)
			c.typeOf[x] = Bool
		default:
			panic(fmt.Sprintf("unhandled prefix operator %v", x.Op))
		}
	case *ast.IfElse:
		c.check(x.IfHeader, Bool)
		c.infer(x.Body)
		c.typeOf[x] = c.check(x.ElseBody, c.GetType(x.Body)) // if-else needs an mgu
	case *ast.If:
		c.check(x.IfHeader, Bool)
		c.check(x.Body, Unit)
		c.typeOf[x] = Unit
	case *ast.IfHeader:
		c.infer(x.Cond)
		c.typeOf[x] = c.GetType(x.Cond)
		// should probably panic if something isn't in the typeOf map
	case *ast.IfMatch:
		c.infer(x.IfHeader)
		tH := c.typeOf[x.IfHeader]
		ps := newPatternState(tH)
		c0 := x.Cases[0]
		c.inferPat(ps, tH, c0.Pattern)
		// tH = c.unify(tH, c.typeOf[c0.Pattern])
		if c0.Guard != nil {
			c.check(c0.Guard, Bool)
		}
		c.infer(c0.Expr)
		tB := c.typeOf[c0.Expr]
		for i := 1; i < len(x.Cases); i++ {
			ci := x.Cases[i]
			c.inferPat(ps, tH, ci.Pattern)
			// tH = c.unify(tH, c.typeOf[ci.Pattern])
			if ci.Guard != nil {
				c.check(ci.Guard, Bool)
			}
			c.infer(ci.Expr)
			tB = c.unify(tB, c.typeOf[ci.Expr])
		}
		if !ps.Covered {
			panic("not exhaustive")
		}
		c.typeOf[x] = tB
		// check that this is a sum type, and get its variants

		// c.check(x.IfHeader, Bool)
		// c.infer(x.Cases[0])
		// tc0 := c.typeOf[x.Cases[0]]
		// for i := 1; i < len(x.Cases); i++ {
		// 	tc0 = c.check(x.Cases[i], tc0)
		// }
		// c.typeOf[x] = tc0
	case *ast.Tuple:
		// If it's a length 1 tuple with no trailing comma, it's just the type of the element
		// TODO: does this take into account assignments and stuff?
		if elem, ok := c.CheckTupleParam(x); ok {
			if binExp, ok := elem.X.(*ast.BinaryExpr); ok && binExp.Op.Type == lexer.Assign {
				if _, isIdent := binExp.Left.(*ast.Ident); isIdent {
					goto inner
				}
			}
			c.infer(elem.X)
			tX := c.GetType(elem.X)
			c.typeOf[elem] = tX
			c.typeOf[x] = tX
			break

		}
	inner:
		fields := make([]Field, len(x.Elements))
		for i, elem := range x.Elements {
			if assignExp, ok := c.CheckAssignElem(elem); ok {
				name := assignExp.Left.(*ast.Ident)
				c.infer(assignExp.Right)
				// I don't think the name and assignment get types here?
				fields[i] = Field{
					Name: name,
					Type: c.GetType(assignExp.Right),
				}
			} else {
				c.infer(elem)
				fields[i] = Field{
					Type: c.GetType(elem),
				}
			}
		}
		c.typeOf[x] = Tuple{fields}
	case *ast.CommaElement:
		c.infer(x.X)
		c.typeOf[x] = c.GetType(x.X)
	case *ast.TypeDecl:
		c.typeOf[x] = Named{
			Name: x.Name,
			Type: c.reifyType(x.Body), // this would probably break down with recursive types
		}
	case *ast.ImportDecl:
		// TODO: checking names inside
	case *ast.SelectorExpr:
		if _, def, ok := c.CheckPackageDef(x); ok {
			switch def := def.(type) {
			case VarBind:
				c.typeOf[x] = c.GetType(def.Def)
				break OUTER
			case TypeBind:
				// is this okay?
				c.typeOf[x] = c.GetType(def.Def)
				break OUTER
			default:
				panic(fmt.Sprintf("unhandled bind %T", def))
			}
		}
		c.infer(x.X)
		if _, tX, ok := c.CheckTypeSel(x.X); ok {
			switch tX := tX.(type) {
			case Named:
				if sum, ok := tX.Type.(Sum); ok {
					// check that caller.Name is a valid constructor
					i := slices.IndexFunc(sum.Variants, func(v Variant) bool { return v.Tag.Name.Data == x.Name.Name.Data })
					if i < 0 {
						panic(fmt.Sprintf("constructor %s not found in type %s", x.Name.Name.Data, tX.Name.Name.Data))
					}
					variant := sum.Variants[i]
					// now we typecheck the application
					// there shouldn't be more than 2 elements in the call unless there's generics right?
					c.unify(Unit, variant.Type)
					// the type of the entire expression should be the type of the sum
					c.typeOf[x] = tX
					// we're done here
					break OUTER
				}
			case Tuple:
				i := slices.IndexFunc(tX.Fields, func(f Field) bool { return f.Name != nil && f.Name.Name.Data == x.Name.Name.Data })
				if i < 0 {
					panic(fmt.Sprintf("no such field %q in %T", x.Name.Name.Data, tX))
				}
				c.typeOf[x] = tX.Fields[i].Type
			default:
				panic(fmt.Sprintf("unhandled selector %T", tX))
			}
		}
		// tX := c.GetType(x.X) // TODO: generate a constraint for this field's existence
		// switch tX := tX.(type) {
		// case Tuple:
		// 	i := slices.IndexFunc(tX.Fields, func(f Field) bool { return f.Name != nil && f.Name.Name.Data == x.Name.Name.Data })
		// 	if i < 0 {
		// 		panic(fmt.Sprintf("no such field %q in %T", x.Name.Name.Data, tX))
		// 	}
		// 	c.typeOf[x] = tX.Fields[i].Type
		// default:
		// 	panic(fmt.Sprintf("unhandled selector %T", tX))
		// }
	// if id, ok := x.X.(*ast.Ident); ok {
	// 	b, _, ok := env.LookupStack(id.Name.Data)
	// 	if ok {
	// 		if b, ok := b.(PackageBind); ok {
	// 			if pkgenv, ok := c.envOf[b.Pkg]; ok {
	// 				if _, ok := pkgenv.LookupLocal(x.Name.Name.Data); ok {

	// 				} else {
	// 					panic(fmt.Sprintf("%q not defined in package %q", x.Name.Name.Data, id.Name.Data))
	// 				}
	// 			}
	// 		}
	// 	}
	// }
	case *ast.CallExpr:
		// just constructors for now.
		// TODO: function application
		// TODO: constructors without namespace (so straight up ident)
		switch caller := x.Elements[0].(type) {
		case *ast.SelectorExpr:
			if _, tX, ok := c.CheckTypeSel(caller.X); ok {
				if tX, ok := tX.(Named); ok {
					if sum, ok := tX.Type.(Sum); ok {
						// check that caller.Name is a valid constructor
						i := slices.IndexFunc(sum.Variants, func(v Variant) bool { return v.Tag.Name.Data == caller.Name.Name.Data })
						if i < 0 {
							panic(fmt.Sprintf("constructor %s not found in type %s", caller.Name.Name.Data, tX.Name.Name.Data))
						}
						variant := sum.Variants[i]
						// now we typecheck the application
						// there shouldn't be more than 2 elements in the call unless there's generics right?
						c.check(x.Elements[1], variant.Type)
						// the type of the entire expression should be the type of the sum
						c.typeOf[x] = tX
						// we're done here
						break OUTER
					}
				}
			}
		}
		panic("unhandled CallExpr")
	default:
		panic(fmt.Sprintf("unhandled infer %T", x))
	}
}

// probably not taking into account
func (c *Checker) CheckTypeSel(x ast.Node) (Bind, Type, bool) {
	switch x := x.(type) {
	case *ast.Ident:
		if e, ok := c.GetEnv(x); ok {
			if b, _, ok := e.LookupStack(x.Name.Data); ok {
				switch b := b.(type) {
				case TypeBind:
					return b, c.GetType(b.Def), true
				case BaseTypeBind:
					return b, b.Type, true
				}
			}
		}
	case *ast.SelectorExpr:
		if _, def, ok := c.CheckPackageDef(x); ok {
			switch def := def.(type) {
			case TypeBind:
				return def, c.GetType(def.Def), true
			}
		}
	}
	return nil, nil, false
}

func (c *Checker) CheckPackageDef(x *ast.SelectorExpr) (string, Bind, bool) {
	if selID, ok := x.X.(*ast.Ident); ok {
		if e, ok := c.GetEnv(x.X); ok {
			if b, _, ok := e.LookupStack(selID.Name.Data); ok {
				if pkgbind, ok := b.(PackageBind); ok {
					if pkgenv, ok := c.GetEnv(pkgbind.Pkg); ok {
						if def, ok := pkgenv.LookupLocal(x.Name.Name.Data); ok {
							return pkgbind.Pkg.Name, def, true
						} else {
							panic(fmt.Sprintf("%q not defined in package %q", x.Name.Name.Data, selID.Name.Data))
						}
					}
				}
			}
		}
	}
	return "", nil, false
}

func (c *Checker) check(x ast.Node, t Type) Type {
	c.infer(x)
	return c.unify(c.typeOf[x], t)
}

func (c *Checker) reifyType(t ast.Node) Type {
	if t == nil {
		return nil
	}
	switch t := t.(type) {
	case *ast.Ident:
		if env, ok := c.GetEnv(t); ok {
			if bind, _, ok := env.LookupStack(t.Name.Data); ok {
				if tbind, ok := bind.(BaseTypeBind); ok {
					return tbind.Type
				}
				if tbind, ok := bind.(TypeBind); ok {
					rt := c.reifyType(tbind.Def) // woot woot
					// tbind.ReifiedType = rt
					// e.symbols[t.Name.Data] = tbind
					return rt
					// return Named{
					// 	Name: t,
					// 	Type: c.reifyType(tbind.Def),
					// }
				}
			}
		}
	case *ast.Tuple:
		fields := make([]Field, len(t.Elements))
		hasField := false
		hasNonField := false
		for i, elem := range t.Elements {
			rt := c.reifyType(elem)
			// better way to write this?
			if f, ok := rt.(Field); ok {
				fields[i] = f
				hasField = true
			} else {
				fields[i] = Field{
					Type: rt,
				}
				hasNonField = true
			}

		}
		if hasField && hasNonField {
			panic("cannot mix field an non-field")
		}
		// should we add this to typeof?
		return Tuple{fields}
	case *ast.CommaElement:
		return c.reifyType(t.X)
	case *ast.Field:
		return Field{
			Name: t.Name,
			Type: c.reifyType(t.Type),
		}
	case *ast.TypeDecl:
		c.infer(t)
		return c.typeOf[t]
	case *ast.SumType:
		variants := make([]Variant, len(t.Elements))
		for i, elem := range t.Elements {
			variants[i] = c.reifyType(elem).(Variant)
		}
		return Sum{variants}
	case *ast.SumTypeElement:
		var typ Type
		if t.Type == nil {
			typ = Unit
		} else {
			typ = c.reifyType(t.Type)
		}
		if e, ok := c.GetEnv(t.Name); ok {
			if b, ok := e.LookupLocal(t.Name.Name.Data); ok {
				if cons, ok := b.(ConsBind); ok {
					return Variant{
						Tag:      t.Name,
						ConsName: cons.ConsName,
						Type:     typ,
					}
				}
			}
		}
		panic("not a valid constructor")
	}
	panic(fmt.Sprintf("TODO: reifyType %T", t))
}

func (c *Checker) get(t Type) Type {
	if tvar, ok := t.(TypeVar); ok {
		if tvar.Ref.Bound {
			return c.get(tvar.Ref.Type)
		}
	}
	return t
}

func (c *Checker) simpleEquals(a, b Type) bool {
	if a, ok := a.(Base); ok {
		return a == b
	}
	if a, ok := a.(Named); ok {
		if b, ok := b.(Named); ok {
			// this doesn't work for type aliases, if we add them
			// should this peek inside?
			return a.Name.Name.Data == b.Name.Name.Data
		}
	}
	return false
}

func (c *Checker) GoConvertible(a, b Type) bool {
	if c.simpleEquals(a, b) {
		return true
	}
	if a, ok := a.(Named); ok {
		return c.GoConvertible(a.Type, b)
	}
	if b, ok := b.(Named); ok {
		return c.GoConvertible(a, b.Type)
	}
	if a, ok := a.(Variant); ok {
		return c.GoConvertible(a.Type, b)
	}
	if b, ok := b.(Variant); ok {
		return c.GoConvertible(a, b.Type)
	}
	if a, ok := a.(Tuple); ok {
		if b, ok := b.(Tuple); ok {
			if len(a.Fields) != len(b.Fields) {
				return false
			}
			for i := range a.Fields {
				af, bf := a.Fields[i], b.Fields[i]
				var aname string
				var bname string
				if af.Name != nil {
					aname = af.Name.Name.Data
				}
				if bf.Name != nil {
					bname = bf.Name.Name.Data
				}
				if aname != bname {
					return false
				}
				if !c.GoConvertible(af.Type, bf.Type) {
					return false
				}
			}
		} else {
			return false
		}
	} else {
		return false
	}
	return true
}

// return the mgu
// TODO: add sum types
func (c *Checker) unify(a, b Type) Type {
	a, b = c.get(a), c.get(b)
	if c.simpleEquals(a, b) {
		return a
	}
	if aTV, ok := a.(TypeVar); ok {
		// TODO: occurs check
		// i think this is redundant
		// if aTV.Ref.Bound {
		// 	if c.simpleEquals(aTV.Ref.Type, b) {
		// 		return
		// 	}
		// 	panic(fmt.Sprintf("bound metavar %s does not match %s", aTV.Ref.ID.TypeArg.Data, b))
		// }
		aTV.Ref.Bound = true
		aTV.Ref.Type = b
		return b
	}
	if bTV, ok := b.(TypeVar); ok {
		// TODO: occurs check
		// i think this is redundant
		// if bTV.Ref.Bound {
		// 	if c.simpleEquals(bTV.Ref.Type, a) {
		// 		return
		// 	}
		// 	panic(fmt.Sprintf("bound metavar %s does not match %s", bTV.Ref.ID.TypeArg.Data, a))
		// }
		bTV.Ref.Bound = true
		bTV.Ref.Type = a
		return a
	}
	// TODO: hold off until we understand subtyping and structural typing.
	if nom, ok := a.(Named); ok {
		if _, ok := b.(Named); !ok {
			return c.unify(nom.Type, b)
		}
	} else if nom, ok := b.(Named); ok {
		return c.unify(a, nom.Type)
	}
	var nf []Field
	// how to handle unordered inferred tuple?
	// punt that till we have row variables.
	if tup1, ok := a.(Tuple); ok {
		if tup2, ok := b.(Tuple); ok {
			if len(tup1.Fields) != len(tup2.Fields) {
				panic("tuple length mismatch")
			}
			for i := range tup1.Fields {
				f1 := tup1.Fields[i]
				f2 := tup2.Fields[i]
				if f1.Name != nil && f2.Name != nil {
					if f1.Name.Name.Data != f2.Name.Name.Data {
						panic(fmt.Sprintf("tuple field name mismatch: %v and %v", f1.Name.Name.Data, f2.Name.Name.Data))
					}
					nf = append(nf, Field{
						f1.Name,
						c.unify(tup1.Fields[i].Type, tup2.Fields[i].Type),
					})
				} else if f1.Name == nil && f2.Name == nil {
					nf = append(nf, Field{Type: c.unify(tup1.Fields[i].Type, tup2.Fields[i].Type)})
				} else {
					nf = append(nf, Field{Type: c.unify(tup1.Fields[i].Type, tup2.Fields[i].Type)})
					// panic("tuple field name mismatch")
				}
			}
			return Tuple{Fields: nf}
		} else {
			if len(tup1.Fields) == 1 {
				return c.unify(tup1.Fields[0].Type, b)
			}
		}
	} else if tup2, ok := b.(Tuple); ok {
		if len(tup2.Fields) == 1 {
			return c.unify(a, tup2.Fields[0].Type)
		}
	}
	// if sum1, ok := a.(Sum); ok {
	// 	if sum2, ok := b.(Sum); ok {

	// 	}
	// }
	panic(fmt.Sprintf("TODO: unify %T %T", a, b))
}

func checkUnderlyingTuple(t Type) (res Tuple, b bool) {
	switch t := t.(type) {
	case Named:
		return checkUnderlyingTuple(t.Type)
	case Variant:
		return checkUnderlyingTuple(t.Type)
	case Tuple:
		return t, true
	default:
		return
	}
}

// does not attempt to do unification
// assumes typevars are bound
func (c *Checker) typeEquals(a, b Type) bool {
	a, b = c.get(a), c.get(b)
	if c.simpleEquals(a, b) {
		return true
	}
	if nom, ok := a.(Named); ok {
		if _, ok := b.(Named); !ok {
			return c.typeEquals(nom.Type, b)
		}
	} else if nom, ok := b.(Named); ok {
		return c.typeEquals(a, nom.Type)
	}
	// how to handle unordered inferred tuple?
	// punt that till we have row variables.
	if tup1, ok := a.(Tuple); ok {
		if tup2, ok := b.(Tuple); ok {
			if len(tup1.Fields) != len(tup2.Fields) {
				return false
			}
			for i := range tup1.Fields {
				f1 := tup1.Fields[i]
				f2 := tup2.Fields[i]
				if f1.Name != nil && f2.Name != nil {
					if f1.Name.Name.Data != f2.Name.Name.Data {
						return false
					}
				}
				if !c.typeEquals(tup1.Fields[i].Type, tup2.Fields[i].Type) {
					return false
				}
			}
			return true
		}
	}
	return false
}

// should be called after unify
func (c *Checker) RankPromotable(a, b Type) bool {
	if ta, ok := checkUnderlyingTuple(a); ok {
		if len(ta.Fields) == 1 {
			ft := ta.Fields[0].Type
			// check that ft and b are equal
			return c.typeEquals(ft, b)
		}
	}
	return false
}

func (c *Checker) CheckAssignElem(elem ast.Node) (*ast.BinaryExpr, bool) {
	if comm, ok := elem.(*ast.CommaElement); ok {
		if binExp, ok := comm.X.(*ast.BinaryExpr); ok {
			if _, ok := binExp.Left.(*ast.Ident); ok { // is this too restrictive?
				if binExp.Op.Type == lexer.Assign {
					return binExp, true
				}
			}
		}
	}
	return nil, false
}

func (c *Checker) CheckTupleParam(x *ast.Tuple) (*ast.CommaElement, bool) {
	if len(x.Elements) == 1 {
		if elem, ok := x.Elements[0].(*ast.CommaElement); ok {
			if elem.Comma.Type != lexer.Comma {
				return elem, true
			}
		}
	}
	return nil, false
}

// If it's top-level, also check that it doesn't collide with imports.
func (c *Checker) FreshName(seed string) *ast.Ident {
	if seed == "" {
		seed = "tmp"
	}
	if _, ok := c.unique[seed]; !ok {
		c.unique[seed] = struct{}{}
		return &ast.Ident{
			Name: lexer.Token{
				Type: lexer.Ident,
				Data: seed,
			},
		}
	}
	num := 0
	for {
		name := seed + strconv.Itoa(num)
		if _, ok := c.unique[name]; !ok {
			c.unique[name] = struct{}{}
			return &ast.Ident{
				Name: lexer.Token{
					Type: lexer.Ident,
					Data: name,
				},
			}
		}
		num++
	}
}

func (c *Checker) FreshTypeVar() *ast.TypeArg {
	num := 0
	for {
		name := "'" + strconv.Itoa(num)
		if _, ok := c.unique[name]; !ok {
			c.unique[name] = struct{}{}
			return &ast.TypeArg{
				TypeArg: lexer.Token{
					Type: lexer.TypeArg,
					Data: name,
				},
			}
		}
		num++
	}
}

func (c *Checker) appErr(err error) {
	c.err = errors.Join(c.err, err)
}
