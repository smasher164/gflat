package types2

import (
	"fmt"
	"path"
	"strconv"

	"github.com/smasher164/gflat/ast"
	"golang.org/x/mod/module"
)

func (c *Checker) resolvePattern(env *Env, pat ast.Node) {
	switch pat := pat.(type) {
	case *ast.TypeAnnotation:
		panic("TODO TypeAnnotation")
	case *ast.BinaryExpr:
		panic("TODO: Or patterns")
		// adding a scope so that shared bindings work?
		// c.resolvePattern(env.AddScope(), pat.Left)
		// c.resolvePattern(env.AddScope(), pat.Right)
	case *ast.CallExpr:
		c.resolve(env, pat.Elements[0])
		for i := 1; i < len(pat.Elements); i++ {
			c.resolvePattern(env, pat.Elements[i])
		}
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
	// case *ast.SelectorExpr:
	// we need to deal with this case.
	// could be pkg.type.cons or type.cons
	case *ast.Ident:
		if _, ok := env.LookupLocal(pat.Name.Data); ok {
			panic("redefinition of binding")
		}
		if b, _, ok := env.LookupStack(pat.Name.Data); ok {
			if _, ok := b.(ConsBind); !ok {
				c.AddSymbol(env, pat.Name.Data, VarBind{pat})
			}
		} else {
			c.AddSymbol(env, pat.Name.Data, VarBind{pat})
		}
		// we're not allowing Ident to be a constructor.
		// actually nullary constructors that specify C _ or C () will work.
	case *ast.Number:
	case *ast.BasicString:
	case *ast.InterpolatedString:
		panic("TODO: interpolated string in pattern")
	case *ast.Tuple:
		for _, elem := range pat.Elements {
			c.resolvePattern(env, elem.(*ast.CommaElement).X)
		}
	}
	c.envOf[pat] = env
}

func (c *Checker) resolveMatchCase(env *Env, pc *ast.PatternCase) {
	// we have to figure out what names are introduced by the pattern
	// unfortunately, with something like
	// if(x)
	// | Y =>
	// we can't actually know if Y is a constructor or variable without knowing the type of x.
	// it's okay to delay resolution of this, since

	// we will ignore the above case for now.
	// if there is a constructor, it *must* be namespaced.
	// so A.B, not B
	// may have to turn this phase into a lazy thing, where inference calls the resolver.
	// envOf can be a lazily computed map.

	c.resolvePattern(env, pc.Pattern)
	if pc.Guard != nil {
		c.resolve(env, pc.Guard)
	}
	c.resolve(env, pc.Expr)
	c.envOf[pc] = env
}

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
		// c.AddSymbol(curr, x.Name.Data, Unresolved{})
		if _, _, ok := env.LookupStack(x.Name.Data); !ok {
			c.AddSymbol(env, x.Name.Data, Unresolved{})
		}
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
	case *ast.IfMatch:
		c.resolve(env, x.IfHeader)
		for _, pc := range x.Cases {
			c.resolveMatchCase(env.AddScope(), pc)
		}
		c.envOf[x] = env
	case *ast.Tuple:
		_, isTupleParam := c.CheckTupleParam(x)
		var hasAssign bool
		var hasNotAssign bool
		for _, elem := range x.Elements {
			if !isTupleParam {
				if binExp, ok := c.CheckAssignElem(elem); ok {
					c.resolve(env, binExp.Right) // should this be a new scope?
					c.envOf[binExp] = env
					c.envOf[elem] = env
					hasAssign = true
					continue
				}
				hasNotAssign = true
			}
			c.resolve(env, elem)
		}
		if hasAssign && hasNotAssign {
			panic("cannot mix positional and named assignment in tuple")
		}
		c.envOf[x] = env
	case *ast.CommaElement:
		// TODO: add support for assigning the fields
		c.resolve(env, x.X)
		c.envOf[x] = env
	case *ast.IndexExpr:
		// TODO: import-level index exprs
		c.resolve(env, x.X)
		for _, elem := range x.IndexElements {
			c.resolve(env, elem)
		}
		c.envOf[x] = env
	case *ast.SelectorExpr:
		c.resolve(env, x.X)
		c.envOf[x.Name] = env
	case *ast.CallExpr:
		for _, elem := range x.Elements {
			c.resolve(env, elem)
		}
		c.envOf[x] = env

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
	// if e, ok := c.envOf[x.X]; ok {
	// 	e.
	// }
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
		panic(fmt.Sprintf("resolve: unhandled node type %T", x))
	}
}

func (c *Checker) resolveSignature(env *Env, sig *ast.FunctionSignature) {
	switch param := sig.Param.(type) {
	case *ast.TypeAnnotation:
		c.envOf[sig.Param] = env
		c.envOf[param.Destructure] = env
		paramName := param.Destructure.(*ast.Ident)
		paramBind := VarBind{Def: paramName} // stick type in here?
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

func (c *Checker) defineFun(packageScope, curr *Env, fname string, x ast.Node) {
	c.checkTopLevelCollision(curr, fname)
	switch x := x.(type) {
	case *ast.LetFunction:
		// TODO: type parameters and type constraints
		// TODO: forward declaration and mutual declarations
		// process signature and body
		bind := VarBind{Def: x.Name}
		c.AddSymbol(packageScope, fname, bind)
		funScope := curr.AddScope()
		c.resolveSignature(funScope, x.Signature)
		c.resolve(funScope, x.Body)
	case *ast.Function:
		panic("TODO: function")
	}
}
func (c *Checker) checkTopLevelCollision(fileScope *Env, name string) {
	if f, ok := c.fileOf[name]; ok && f == fileScope {
		panic("name collision")
	}
	if _, ok := fileScope.parent.LookupLocal(name); ok {
		panic("name collision")
	}
}

func (c *Checker) defineLet(packageScope, curr *Env, des ast.Node) {
	switch des := des.(type) {
	case *ast.Ident:
		c.checkTopLevelCollision(curr, des.Name.Data)
		bind := VarBind{Def: des}
		c.AddSymbol(packageScope, des.Name.Data, bind)
		c.envOf[des] = packageScope
		c.fileOf[des.Name.Data] = curr
	case *ast.TypeAnnotation:
		c.defineLet(packageScope, curr, des.Destructure)
		c.resolveTypeAnnotation(curr, des.Type)
		c.envOf[des] = packageScope
	case *ast.Tuple:
		panic("TODO: tuple")
	}
}

func (c *Checker) resolveTypeBody(tname string, curr *Env, T ast.Node) {
	switch T := T.(type) {
	case *ast.Ident:
		if _, _, ok := curr.LookupStack(T.Name.Data); ok {
			// TODO: check that it's a type binding
		} else {
			c.AddSymbol(curr, T.Name.Data, Unresolved{})
		}
	case *ast.Tuple:
		paramScope := curr.AddScope()
		for _, elem := range T.Elements {
			// should we create a scope here?
			// how do we treat labels?
			c.resolveTypeBody(tname, paramScope, elem)
		}
	case *ast.CommaElement:
		c.resolveTypeBody(tname, curr, T.X)
	case *ast.Field:
		// TODO: default parameters
		fname := T.Name.Name.Data
		if _, ok := curr.LookupLocal(fname); ok {
			panic("field already defined")
		}
		c.envOf[T.Name] = curr
		c.AddSymbol(curr, fname, VarBind{Def: T.Name}) // what kind of binding is this?
		c.resolveTypeBody(tname, curr, T.Type)
	case *ast.SumType:
		sumScope := curr.AddScope()
		for _, elem := range T.Elements {
			c.resolveTypeBody(tname, sumScope, elem)
		}
	case *ast.SumTypeElement:
		consName := tname + "_" + T.Name.Name.Data
		if _, ok := curr.LookupLocal(T.Name.Name.Data); ok {
			panic("constructor already defined")
		} else if _, ok := curr.parent.parent.LookupLocal(consName); ok {
			panic("name collision with constructor name")
		} else {
			c.envOf[T.Name] = curr
			b := ConsBind{T.Name, consName}
			c.AddSymbol(curr, T.Name.Name.Data, b)
			c.AddSymbol(curr.parent.parent, consName, b)
			c.fileOf[consName] = curr.parent.parent
		}
		if T.Type != nil {
			c.resolveTypeBody(tname, curr, T.Type)
		}
	default:
		panic(fmt.Sprintf("resolveTypeBody %T", T))
	}
	c.envOf[T] = curr
}

func (c *Checker) resolveTypeAnnotation(curr *Env, T ast.Node) {
	c.resolveTypeBody("", curr, T)
	// switch T := T.(type) {
	// case *ast.Ident:
	// 	if _, _, ok := curr.LookupStack(T.Name.Data); ok {
	// 		// TODO: check that it's a type binding
	// 	} else {
	// 		c.AddSymbol(curr, T.Name.Data, Unresolved{})
	// 	}
	// default:
	// 	panic("TODO")
	// }
	// c.envOf[T] = curr
}

func (c *Checker) defineType(packageScope, curr *Env, typeDecl *ast.TypeDecl) {
	tname := typeDecl.Name.Name.Data
	c.checkTopLevelCollision(curr, tname)
	// ignore type parameters and constraints for now
	bind := TypeBind{Def: typeDecl}
	c.AddSymbol(packageScope, tname, bind)
	bodyScope := curr.AddScope()
	c.resolveTypeBody(tname, bodyScope, typeDecl.Body)
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
		tname := x.Name.Name.Data
		c.defineType(packageScope, curr, x)
		c.fileOf[tname] = curr
		c.envOf[x] = packageScope
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
		importPath, err := strconv.Unquote(x.Path.Lit.Data)
		if err != nil {
			panic("not a valid import path")
		}
		pkg, ok := c.importer.PkgCache[importPath]
		if !ok {
			panic(fmt.Sprintf("imported package %s not found in cache", importPath))
		}
		bind := PackageBind{pkg}
		if x.Binding == nil {
			if prefix, _, ok := module.SplitPathVersion(importPath); ok {
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
