package types2

import (
	"errors"
	"fmt"
	"path"
	"strconv"

	"github.com/smasher164/gflat/ast"
	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
	"golang.org/x/mod/module"
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
	c.AddSymbol(universe, "true", VarBind{Bool})
	c.AddSymbol(universe, "false", VarBind{Bool})
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
	return c.typeOf[x]
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

func (c *Checker) infer(x ast.Node) {
	if _, ok := c.typeOf[x]; ok {
		return
	}
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
				if t, ok := c.typeOf[elem]; ok {
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
		c.infer(x.Rhs)
		trhs := c.typeOf[x.Rhs]
		c.unify(c.typeOf[x.Destructure], trhs)
		// c.typeOf[x] = trhs
		// if destructure has a type annotation, ...
		// otherwise, set its type to trhs
	case *ast.LetFunction:
	case *ast.TypeAnnotation:
		tann := c.reifyType(x.Type)
		c.typeOf[x.Destructure] = tann
		c.typeOf[x] = tann
	case *ast.Ident:
		// look up in environment
		// get its type
		if b, _, ok := c.envOf[x].LookupStack(x.Name.Data); ok {
			if b, ok := b.(VarBind); ok && b.Type != nil {
				c.typeOf[x] = b.Type
				break
			}
		}
		fresh := c.FreshTypeVar()
		c.typeOf[x] = NewTypeVar(fresh)
	case *ast.Number:
		c.typeOf[x] = Int
	case *ast.BasicString:
		c.typeOf[x] = String
	case *ast.BinaryExpr:
		switch x.Op.Type {
		case lexer.Plus:
			c.infer(x.Left)
			c.check(x.Right, c.typeOf[x.Left])
			c.typeOf[x] = c.typeOf[x.Left]
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
			c.check(x.Right, c.typeOf[x.Left])
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
		c.check(x.ElseBody, c.typeOf[x.Body])
		c.typeOf[x] = c.typeOf[x.Body]
	case *ast.If:
		c.check(x.IfHeader, Bool)
		c.check(x.Body, Unit)
		c.typeOf[x] = Unit
	case *ast.IfHeader:
		c.infer(x.Cond)
		c.typeOf[x] = c.typeOf[x.Cond]
		// should probably panic if something isn't in the typeOf map
	case *ast.Tuple:
		// If it's a length 1 tuple with no trailing comma, it's just the type of the element
		// TODO: does this take into account assignments and stuff?
		if len(x.Elements) == 1 {
			if elem, ok := x.Elements[0].(*ast.CommaElement); ok {
				if elem.Comma.Type != lexer.Comma {
					c.infer(elem.X)
					c.typeOf[elem] = c.typeOf[elem.X]
					c.typeOf[x] = c.typeOf[elem.X]
				}
			}
		}
	}
}

func (c *Checker) check(x ast.Node, t Type) {
	c.infer(x)
	c.unify(c.typeOf[x], t)
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
					return Named{
						Name: t,
						Type: c.reifyType(tbind.Def),
					}
				}
			}
		}
	}
	panic("TODO: reifyType")
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
			return a.Name == b.Name
		}
	}
	return false
}

func (c *Checker) unify(a, b Type) {
	a, b = c.get(a), c.get(b)
	if c.simpleEquals(a, b) {
		return
	}
	if aTV, ok := a.(TypeVar); ok {
		// TODO: occurs check
		if aTV.Ref.Bound {
			if c.simpleEquals(aTV.Ref.Type, b) {
				return
			}
			panic(fmt.Sprintf("bound metavar %s does not match %s", aTV.Ref.ID.TypeArg.Data, b))
		}
		aTV.Ref.Bound = true
		aTV.Ref.Type = b
		return
	}
	if bTV, ok := b.(TypeVar); ok {
		// TODO: occurs check
		if bTV.Ref.Bound {
			if c.simpleEquals(bTV.Ref.Type, a) {
				return
			}
			panic(fmt.Sprintf("bound metavar %s does not match %s", bTV.Ref.ID.TypeArg.Data, a))
		}
		bTV.Ref.Bound = true
		bTV.Ref.Type = a
		return
	}
	if tup1, ok := a.(Tuple); ok {
		if tup2, ok := b.(Tuple); ok {
			if len(tup1.Fields) != len(tup2.Fields) {
				panic("tuple length mismatch")
			}
			for i := range tup1.Fields {
				// TODO: deal with labels
				c.unify(tup1.Fields[i].Type, tup2.Fields[i].Type)
			}
			return
		}
	}
	panic(fmt.Sprintf("TODO: unify %#v %#v", a, b))
}

// type packageResolver struct {
// 	*Checker
// 	topLevelDeps map[string][]string
// 	sorted       []string
// 	pkg          parser.Package
// }

func (c *Checker) resolve(env *Env, x ast.Node) {
	switch x := x.(type) {
	case *ast.Package:
		packageScope := env.AddScope()
		c.resolveTopLevel(packageScope, nil, x)
	case *ast.File:
		// script
		c.resolve(env, x.Body)
		c.envOf[x] = env
	case *ast.Stmt:
		c.resolve(env, x.Stmt)
		c.envOf[x] = env
	case *ast.Number:
		c.envOf[x] = env
	case *ast.BasicString:
		c.envOf[x] = env
	case *ast.BinaryExpr:
		c.resolve(env, x.Left)
		c.resolve(env, x.Right)
		c.envOf[x] = env
	case *ast.PrefixExpr:
		c.resolve(env, x.X)
		c.envOf[x] = env
	case *ast.Block:
		blockScope := env.AddScope()
		for _, elem := range x.Body {
			c.resolve(blockScope, elem)
		}
		c.envOf[x] = blockScope
	case *ast.Ident:
		// TODO: what do we do here?
		c.envOf[x] = env
	case *ast.IfElse:
		c.resolve(env, x.IfHeader)
		c.resolve(env, x.Body)
		c.resolve(env, x.ElseBody)
		c.envOf[x] = env
	case *ast.If:
		c.resolve(env, x.IfHeader)
		c.resolve(env, x.Body)
		c.envOf[x] = env
	case *ast.IfHeader:
		c.resolve(env, x.Cond)
		c.envOf[x] = env
	case *ast.Tuple:
		for _, elem := range x.Elements {
			c.resolve(env, elem)
		}
		c.envOf[x] = env
	case *ast.CommaElement:
		c.resolve(env, x.X)
		c.envOf[x] = env
	// p := &packageResolver{
	// 	Checker:      c,
	// 	topLevelDeps: make(map[string][]string),
	// 	pkg:          x,
	// }
	// packageScope := env.AddScope()
	// p.SetEnv(x, packageScope) // will this persist after changes to Package?
	// p.collectTopLevelDecls(packageScope, x)
	// p.resolveTopLevel()
	// p.checkTopLevelCycles()
	// return p.pkg
	default:
		panic(fmt.Sprintf("unhandled node type %T", x))
	}
}

func (c *Checker) resolveSignature(env *Env, sig *ast.FunctionSignature) {
	switch param := sig.Param.(type) {
	case *ast.TypeAnnotation:
		c.envOf[sig.Param] = env
		c.envOf[param.Destructure] = env
		paramName := param.Destructure.(*ast.Ident)
		paramBind := VarBind{} // stick type in here?
		c.AddSymbol(env, paramName.Name.Data, paramBind)
		c.resolveTypeAnnotation(env, param.Type)
		// TODO: ignoring param type for now
	case *ast.Tuple:
		panic("TODO")
	}
	c.envOf[sig] = env
	// for _, arrow := range sig.Arrows {
	// TODO: signature
	// }
}

// If it's top-level, also check that it doesn't collide with imports.
func (c *Checker) FreshName() *ast.Ident {
	num := 0
	for {
		name := "tmp" + strconv.Itoa(num)
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

func (c *Checker) defineFun(packageScope, curr *Env, fname string, x ast.Node) {
	if f, ok := c.fileOf[fname]; ok && f == curr {
		panic("name collision")
	}
	switch x := x.(type) {
	case *ast.LetFunction:
		// TODO: type parameters and type constraints
		// TODO: forward declaration and mutual declarations
		// process signature and body
		bind := VarBind{}
		c.AddSymbol(packageScope, fname, bind)
		funScope := curr.AddScope()
		c.resolveSignature(funScope, x.Signature)
		c.resolve(funScope, x.Body)
	case *ast.Function:
		panic("TODO: function")
	}
}

func (c *Checker) resolveTypeAnnotation(curr *Env, T ast.Node) {
	switch T := T.(type) {
	case *ast.Ident:
		if _, _, ok := curr.LookupStack(T.Name.Data); ok {
			// TODO: check that it's a type binding
		} else {
			c.AddSymbol(curr, T.Name.Data, Unresolved{})
		}
	default:
		panic("TODO")
	}
	c.envOf[T] = curr
}

func (c *Checker) defineLet(packageScope, curr *Env, des ast.Node) {
	switch des := des.(type) {
	case *ast.Ident:
		if f, ok := c.fileOf[des.Name.Data]; ok && f == curr {
			panic("name collision")
		}
		bind := VarBind{}
		c.AddSymbol(packageScope, des.Name.Data, bind)
		c.envOf[des] = packageScope
		c.fileOf[des.Name.Data] = curr
	case *ast.TypeAnnotation:
		c.defineLet(packageScope, curr, des.Destructure)
		c.resolveTypeAnnotation(curr, des.Type)
	case *ast.Tuple:
		panic("TODO: tuple")
	}
}

func (c *Checker) resolveTopLevelDecl(packageScope, curr *Env, x ast.Node) {
	switch x := x.(type) {
	case *ast.Stmt:
		c.resolveTopLevelDecl(packageScope, curr, x.Stmt)
		if _, ok := x.Stmt.(*ast.ImportDecl); ok {
			c.envOf[x] = curr
		} else {
			c.envOf[x] = packageScope // TODO: remember this later during freshname generation in codegen.
		}
	case *ast.TypeDecl:
		panic("TODO: TypeDecl")
		// tname := x.Name.Name.Data
		// c.defineType(packageScope, x)
		// c.fileOf[tname] = curr
	case *ast.LetDecl:
		c.resolve(curr, x.Rhs)
		c.defineLet(packageScope, curr, x.Destructure)
		c.envOf[x] = packageScope
	case *ast.LetFunction:
		fname := x.Name.Name.Data
		c.defineFun(packageScope, curr, fname, x)
		c.fileOf[fname] = curr
		c.envOf[x] = packageScope
	case *ast.Function:
		panic("TODO: function")
		// if id, ok := x.Name.(parser.Ident); ok {
		// 	fname := id.Name.Data
		// 	c.defineFun(packageScope, fname, x)
		// 	c.fileOf[fname] = curr
		// } else {
		// 	panic(fmt.Sprintf("top-level functions need to have names"))
		// }
	case *ast.ImportDecl:
		c.defineImport(packageScope, curr, x.Package)
		c.envOf[x] = curr
	case *ast.ImplDecl:
		panic("TODO: resolve ImplDecl")
	}
}

func (c *Checker) defineImport(packageScope, curr *Env, x ast.Node) {
	switch x := x.(type) {
	case *ast.Tuple:
		for _, elem := range x.Elements {
			elem := elem.(*ast.CommaElement)
			c.defineImport(packageScope, curr, elem.X)
		}
		c.envOf[x] = curr
	case *ast.ImportDeclPackage:
		importPath := x.Path.Lit.Data
		pkg, ok := c.importer.PkgCache[importPath]
		if !ok {
			panic("imported package not found in cache")
		}
		bind := PackageBind{pkg}
		if x.Binding == nil {
			if prefix, _, ok := module.SplitPathVersion(x.Path.Lit.Data); ok {
				id := path.Base(prefix)
				// check if it's from the same file
				if f, ok := c.fileOf[id]; ok && f == curr {
					panic("name collision")
				}
				c.AddSymbol(curr, id, bind)
				c.fileOf[id] = curr
			} else {
				panic("unreachable")
			}
			c.envOf[x.Path] = curr
		} else {
			switch binding := x.Binding.(type) {
			case *ast.Ident:
				id := binding.Name.Data
				// check if it's from the same file
				if f, ok := c.fileOf[id]; ok && f == curr {
					panic("name collision")
				}
				c.AddSymbol(curr, id, bind)
				c.envOf[binding] = curr
				c.fileOf[id] = curr
			case *ast.Tuple:
				panic("TODO import tuple")
				// penv, ok := c.GetEnv(pkg)
				// if !ok {
				// 	panic("")
				// }
				// for _, elem := range binding.Elements {
				// 	elem := elem.(parser.CommaElement)
				// 	switch x := elem.X.(type) {
				// 	case parser.Ident:
				// 		// penv.Lookup(x.Name.Data)
				// 		// compute selector probably
				// 	case parser.IndexExpr:
				// 	}
				// }
			}
		}
		c.envOf[x] = curr
	}
}

// resolving top level is just like resolving any scope, except we also collect Uses for any expression, not just functions and types.
// so if A references an unresolved ident, and B references A, then it also references the unresolved ident.
// another way to restate it is that local scope constructs the graph, but throws an error if something is used before it's declared.
// so we could also just construct the uses graph, and at the end of the scope, when we do resolveUnresolved, we can either check for
// cycles, or that something was not used before it was declared.
// it might be more time-efficient to copy all the uses up as we go, but it's more amortized bookkeeping.
func (c *Checker) resolveTopLevel(packageScope, curr *Env, x ast.Node) {
	switch x := x.(type) {
	case *ast.Package:
		var pname string
		for i, pkgFile := range x.PackageFiles {
			if i == 0 {
				pname = pkgFile.PackageName.Name.Data
			}
			if pname != pkgFile.PackageName.Name.Data {
				c.appErr(fmt.Errorf("package names do not match: %s and %s", pname, pkgFile.PackageName.Name.Data))
			}
			fileScope := packageScope.AddScope()
			c.resolveTopLevel(packageScope, fileScope, pkgFile)
		}
		if x.ScriptFile != nil {
			scriptScope := packageScope.AddScope()
			c.resolve(scriptScope, x.ScriptFile)
		}
		c.envOf[x] = packageScope
		// TODO: resolve unresolved
		// c.resolveUnresolvedTopLevel(packageScope, x)
	case *ast.File:
		// blockScope := curr.AddScope() // no need for an extra blockScope for a file
		c.resolveTopLevel(packageScope, curr, x.Body)
		c.envOf[x] = curr
	case *ast.Block:
		for _, e := range x.Body {
			c.resolveTopLevelDecl(packageScope, curr, e)
		}
		c.envOf[x] = curr
	}
}

func (c *Checker) appErr(err error) {
	c.err = errors.Join(c.err, err)
}
