package types3

import (
	"errors"
	"fmt"

	"github.com/smasher164/gflat/ast"
	"github.com/smasher164/gflat/parser"
)

/*
Requirements:
1. Resolve: Node -> ()
2. It can also call Infer internally. Generate fresh type variables for things you don't know.
3. Per-package checker.
*/

func NewChecker(importer *parser.Importer) *Checker {
	return &Checker{
		Importer:   importer,
		PkgChecker: make(map[string]*PackageChecker),
	}
}

type Checker struct {
	Importer   *parser.Importer
	PkgChecker map[string]*PackageChecker
	err        error
}

func (c *Checker) Check() error {
	for _, path := range c.Importer.Sorted {
		c.err = errors.Join(c.err, c.CheckPackage(path))
	}
	return c.err
}

type PackageChecker struct {
	pkg     *ast.Package
	tvCount int
	level   int // level-based let generalization. starts at 1.
	env     *ast.Env

	// access module exports and stuff
	c *Checker

	err error
}

func (pc *PackageChecker) enterLevel() {
	pc.level++
}

func (pc *PackageChecker) leaveLevel() {
	pc.level--
}

func NewPackageChecker(pkg *ast.Package, c *Checker) *PackageChecker {
	pc := &PackageChecker{pkg: pkg, c: c, level: 1, env: pkg.Env}
	return pc
}

func (pc *PackageChecker) Check() error {
	for _, f := range pc.pkg.PackageFiles {
		pc.CheckBlock(f.Body)
	}
	if pc.pkg.ScriptFile != nil {
		pc.CheckBlock(pc.pkg.ScriptFile.Body)
	}
	return pc.err
}

func (pc *PackageChecker) pushEnv(env *ast.Env) *ast.Env {
	old := pc.env
	pc.env = env
	return old
}

func (pc *PackageChecker) popEnv(old *ast.Env) {
	pc.env = old
}

func (pc *PackageChecker) CheckBlock(b *ast.Block) {
	old := pc.pushEnv(b.Env)
	defer pc.popEnv(old)
	for _, stmt := range b.Body {
		switch stmt := stmt.(type) {
		case *ast.LetDecl:
			// pc.CheckLetDecl(b.Env, stmt)
		case *ast.VarDecl:
		case *ast.TypeDecl:
			// pc.CheckTypeDecl(b.Env, stmt)
		case *ast.LetFunction:
			f := pc.inferFunction(stmt.Name.Name.Data, stmt.TypeParams, stmt.Signature, stmt.Body, stmt.Env)
			fmt.Println(f)
		case *ast.Function:
			// we assume it has a name here
			f := pc.inferFunction(stmt.Name.Name.Data, stmt.TypeParams, stmt.Signature, stmt.Body, stmt.Env)
			fmt.Println(f)
		case *ast.ImportDecl:
		case *ast.EmptyExpr:
		default: // expression
		}
	}
}

func (pc *PackageChecker) inferFunction(name string, typeParams []ast.Node, sig *ast.FunctionSignature, body ast.Node, fenv *ast.Env) ast.Type {
	pc.enterLevel()
	// TODO: explicit types, type params, and arrows
	typeOfFn := pc.NewTypeVar()
	typeOfParam := pc.NewTypeVar()
	if name != "" {
		pc.env.SetVarType(name, typeOfFn)
	}
	old := pc.pushEnv(fenv)
	if ann, ok := sig.Param.(*ast.TypeAnnotation); ok {
		if ann.Type != nil {
			// TODO: reify type
		} else {
			paramName := ann.Destructure.(*ast.Ident).Name.Data
			pc.env.Update(paramName, ast.VarBind{Type: typeOfParam})
		}
	}
	tyBody := pc.inferExpr(body)
	pc.popEnv(old)
	pc.unify(typeOfFn, ast.ArrowType{typeOfParam, tyBody})
	pc.leaveLevel()
	genTy := pc.gen(typeOfFn)
	if name != "" {
		pc.env.SetVarType(name, genTy)
	}
	return genTy
}

func force(ty ast.Type) ast.Type {
	if tvar, ok := ty.(ast.TypeVar); ok {
		if tvar.Ref.Bound {
			return force(tvar.Ref.Type)
		}
	}
	return ty
}

func trivialEq(ty1, ty2 ast.Type) bool {
	if ty1 == ty2 {
		return true
	}
	return false
	// panic("TODO trivialEq")
}

func isUnbound(ty1, ty2 ast.Type) (ast.TypeVar, ast.Type, bool) {
	if tvar, ok := ty1.(ast.TypeVar); ok {
		if tvar.Ref.Bound {
			panic("this shouldn't be possible")
		}
		return tvar, ty2, true
	}
	if tvar, ok := ty2.(ast.TypeVar); ok {
		if tvar.Ref.Bound {
			panic("this shouldn't be possible")
		}
		return tvar, ty1, true
	}
	return ast.TypeVar{}, nil, false
}

func isArrow(ty1, ty2 ast.Type) (ast.ArrowType, ast.ArrowType, bool) {
	if arrow1, ok := ty1.(ast.ArrowType); ok {
		if arrow2, ok := ty2.(ast.ArrowType); ok {
			return arrow1, arrow2, true
		}
	}
	return ast.ArrowType{}, ast.ArrowType{}, false
}

func (pc *PackageChecker) occurs(tv ast.TypeVar, ty ast.Type) {
	// TODO: do we need to force here?
	switch ty := force(ty).(type) {
	case ast.TypeVar:
		switch {
		case tv == ty:
			panic("occurs check failed")
		default:
			ty.Ref.Level = min(tv.Ref.Level, ty.Ref.Level)
		}
	case ast.ArrowType:
		pc.occurs(tv, ty.From)
		pc.occurs(tv, ty.To)
	}
}

func (pc *PackageChecker) unify(ty1, ty2 ast.Type) {
	ty1, ty2 = force(ty1), force(ty2)
	if trivialEq(ty1, ty2) {
		return
	}
	if tv, ty, ok := isUnbound(ty1, ty2); ok {
		pc.occurs(tv, ty)
		tv.Link(ty)
		return
	}
	if tyArr1, tyArr2, ok := isArrow(ty1, ty2); ok {
		pc.unify(tyArr1.From, tyArr2.From)
		pc.unify(tyArr1.To, tyArr2.To)
		return
	}
	panic(fmt.Sprintf("cannot unify %v and %v", ty1, ty2))
}

func (pc *PackageChecker) gen(ty ast.Type) ast.Type {
	switch ty := force(ty).(type) {
	case ast.TypeVar:
		if ty.Ref.Level > pc.level {
			return ast.QVar{ID: ty.Ref.ID}
		}
	case ast.ArrowType:
		return ast.ArrowType{
			pc.gen(ty.From),
			pc.gen(ty.To),
		}
	}
	return ty
}

// lookup
// inst
// gen

func (pc *PackageChecker) inst(pt ast.Type) ast.Type {
	tbl := make(map[string]ast.TypeVar)
	return pc.instType(pt, tbl)
	// tbl := lo.Associate(pt.IDs, func(id string) (string, ast.TypeVar) {
	// 	return id, pc.NewTypeVar()
	// })
	// return pc.instType(pt.Type, tbl)
	// slices.

	// switch ty := ty.(type) {
	// case ast.QVar:
	// case ast.TypeVar:
	// case ast.ArrowType:
	// }
}

func (pc *PackageChecker) instType(ty ast.Type, tbl map[string]ast.TypeVar) ast.Type {
	switch ty := force(ty).(type) {
	case ast.QVar:
		if tvar, ok := tbl[ty.ID]; ok {
			return tvar
		}
		tvar := pc.NewTypeVar()
		tbl[ty.ID] = tvar
		return tvar
	case ast.ArrowType:
		return ast.ArrowType{
			pc.instType(ty.From, tbl),
			pc.instType(ty.To, tbl),
		}
	}
	return ty
}

func must[T any](x T, ok bool) T {
	if !ok {
		panic("unreachable")
	}
	return x
}

func (pc *PackageChecker) inferExpr(x ast.Node) ast.Type {
	switch x := x.(type) {
	case *ast.Ident:
		ty := must(pc.env.GetVarTypeStack(x.Name.Data))
		return pc.inst(ty)
	default:
		panic(fmt.Sprintf("TODO inferExpr %T", x))
		// if bind, ok := pc.env.LookupStack(x.Name.Data); ok {
		// 	if varBind, ok := bind.(ast.VarBind); ok {
		// 		return varBind.Type
		// 	}
		// }
		// panic("TODO inferExpr")
	}
}

func (pc *PackageChecker) CheckLetDecl(env *ast.Env, d *ast.LetDecl) {
	// check destructure
	// unify rhs with it
}

func (pc *PackageChecker) NewTypeVar() ast.TypeVar {
	lvl := pc.level
	for {
		name := fmt.Sprintf("'%d", pc.tvCount)
		pc.tvCount++
		if !pc.pkg.Unique.Has(name) {
			pc.pkg.Unique.Put(name)
			return ast.NewTypeVar(name, lvl)
		}
	}
}

func (c *PackageChecker) CheckTypeDecl(env *ast.Env, d *ast.TypeDecl) {
	panic("TODO CheckTypeDecl")
	// The typename should be in the env.
	// Also, when reifying, attach Path, File, and Span.
	// if bind, origEnv, ok := env.LookupStack(d.Name.Name.Data); ok {
	// 	if typeBind, ok := bind.(*ast.TypeBind); ok {
	// 		if typeBind.Type == nil {
	// 			t := ast.Named{
	// 				QualifiedName: d.Name.QualifiedName(),
	// 				// UnderlyingType: c.NewTypeVar(),
	// 				// should this be unified with the type body?
	// 			}
	// 			// insert a fresh type variable
	// 			_ = origEnv
	// 		}
	// 	}
	// }
}

func (c *Checker) CheckPackage(importPath string) error {
	pc := NewPackageChecker(c.Importer.PkgCache[importPath], c)
	c.PkgChecker[importPath] = pc
	return pc.Check()
}

/*
If you repurpose Env for module exports, then how do you have a separate package checker?
So when resolving an import, you need to be able to reference the imported package.

How to add global definitions?
How to deal with package vs file scope?
*/
