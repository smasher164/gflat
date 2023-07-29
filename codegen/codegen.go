package codegen

import (
	"bytes"
	"fmt"
	"go/format"
	"io"
	"io/fs"
	"math/bits"
	"os"
	"path"
	"strconv"
	"strings"

	"github.com/smasher164/gflat/ast"
	"github.com/smasher164/gflat/fsx"
	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types2"
	"golang.org/x/exp/slices"
	"golang.org/x/mod/module"
)

// how should the output directory be mocked?
/*
package -> package
script file -> "go:build ignore" file in that same package directory
filename.gf -> filename.go (i think)
package -> package
package/subpackage -> package/subpackage
don't rely on concurrent writes right now, until the in-memory fs can be made thread-safe
*/

const formatSource = true

type Codegen struct {
	// assumes we have a resolved and typed build
	importer *parser.Importer
	checker  *types2.Checker
}

func NewCodegen(importer *parser.Importer, checker *types2.Checker) *Codegen {
	return &Codegen{
		importer: importer,
		checker:  checker,
	}
}

type packageCodegen struct {
	*Codegen
	importUnique map[string]string
}

// do the same thing in checker
func newPackageCodegen(c *Codegen) *packageCodegen {
	return &packageCodegen{
		Codegen: c,
	}
}

// TODO: key the build by hash of inputs (basic incremental compilation)
func (c *Codegen) CodegenBuild(outfs fs.FS) {
	for _, path := range c.importer.Sorted {
		path := path
		pkg := c.importer.PkgCache[path]
		pkgdir, err := fsx.Mkdir(outfs, path, 0)
		if err != nil {
			panic(err) // handle error
		}
		newPackageCodegen(c).codegen(pkgdir, pkg) // there should be no errors
	}
}

func withoutExt(path string) string {
	for i := len(path) - 1; i >= 0 && !os.IsPathSeparator(path[i]); i-- {
		if path[i] == '.' {
			return path[:i]
		}
	}
	return ""
}

func (c *packageCodegen) codegen(outfs fs.FS, x ast.Node) {
	switch x := x.(type) {
	case *ast.Package:
		for _, file := range x.PackageFiles {
			c.codegen(outfs, file)
		}
		if x.ScriptFile != nil {
			c.codegen(outfs, x.ScriptFile)
		}
	case *ast.File:
		filename := withoutExt(x.Filename) + ".go"
		f, err := fsx.Create(outfs, filename)
		if err != nil {
			panic(err) // handle error
		}
		if formatSource {
			defer func() {
				if err := f.Close(); err != nil {
					panic(err) // handle error
				}
				b, err := fs.ReadFile(outfs, filename)
				if err != nil {
					panic(err) // handle error
				}
				b, err = format.Source(b)
				if err != nil {
					panic(err) // handle error
				}
				f, err := fsx.Create(outfs, filename)
				if err != nil {
					panic(err) // handle error
				}
				defer f.Close()
				if _, err := f.Write(b); err != nil {
					panic(err) // handle error
				}
			}()
		} else {
			defer f.Close()
		}
		if x.PackageName != nil {
			fmt.Fprintf(f, "package %s\n\n", x.PackageName.Name.Data)
		} else {
			fmt.Fprintf(f, "//go:build ignore\n\npackage main\n\n")
		}
		// clear for every file?
		c.importUnique = make(map[string]string)
		for imp := range x.Imports {
			// why are we doing this here too?
			if prefix, _, ok := module.SplitPathVersion(imp); ok {
				idimp := path.Base(prefix)
				freshImp := c.checker.FreshName(idimp).Name.Data
				c.importUnique[idimp] = freshImp
				fmt.Fprintf(f, "import %s %q\n", freshImp, imp)
			}
		}
		buf := new(bytes.Buffer)
		if _, ok := x.Imports["unsafe"]; !ok {
			unsafeImport := c.checker.FreshName("unsafe").Name.Data
			c.importUnique["unsafe"] = unsafeImport
			fmt.Fprintf(f, "import %s \"unsafe\"\n", unsafeImport)
			fmt.Fprintf(buf, "type _ = %s.Pointer\n", unsafeImport)
		}
		// if _, ok := x.Imports["math/bits"]; !ok {
		// 	bitsImport := c.checker.FreshName("bits").Name.Data
		// 	c.importUnique["bits"] = bitsImport
		// 	fmt.Fprintf(f, "import %s \"math/bits\"\n", bitsImport)
		// 	fmt.Fprintf(buf, "const _ = %s.UintSize\n", bitsImport)
		// }
		io.Copy(f, buf)
		if x.PackageName != nil {
			c.codegenExprTopLevel(f, x.Body)
		} else {
			fmt.Fprintf(f, "\nfunc main() {\n")
			c.codegenExpr(f, x.Body, false)
			fmt.Fprintf(f, "}\n")
		}
	}
}

func (c *packageCodegen) codegenExprTopLevel(f fsx.WriteableFile, x ast.Node) {
	switch x := x.(type) {
	case *ast.Block:
		for _, elem := range x.Body {
			c.codegenExpr(f, elem, true)
		}
	}
}

func (c *packageCodegen) checkCodegenTuple(f fsx.WriteableFile, x ast.Node, dstType types2.Type, topLevel bool) string {
	ttup := c.checker.GetType(x)
	vars := c.codegenExpr(f, x, topLevel)
	if c.checker.GoConvertible(dstType, ttup) { // TODO: equivalent
		return vars[0]
	}
	return fmt.Sprintf("*(*%s)(%s.Pointer(&%s))", typeString(dstType), c.importUnique["unsafe"], vars[0])
}

func (c *packageCodegen) checkCodegenBinExp(f fsx.WriteableFile, x ast.Node, dstType types2.Type, topLevel bool) string {
	switch x := x.(type) {
	case *ast.Number:
		return x.Lit.Data
	case *ast.BasicString:
		return x.Lit.Data
	case *ast.CommaElement:
		// is this right?
		return c.checkCodegenBinExp(f, x.X, dstType, topLevel)
	default:
		return c.checkCodegenTuple(f, x, dstType, topLevel)
	}
}

func (c *packageCodegen) checkCodegenPromote(f fsx.WriteableFile, x ast.Node, dstType types2.Type, topLevel bool) string {
	v := c.checkCodegenBinExp(f, x, dstType, topLevel)
	return c.promoteCodegenTuple(f, dstType, c.checker.GetType(x), v)
}

func tagSizeType(x int) string {
	tagSize := bits.UintSize - bits.LeadingZeros(uint(x-1))
	switch {
	case tagSize == 0:
		panic("unreachable")
	case tagSize <= 8:
		return "uint8"
	case tagSize <= 16:
		return "uint16"
	case tagSize <= 32:
		return "uint32"
	case tagSize <= 64:
		return "uint64"
	}
	panic("unreachable")

	// local typedefs in go can't be mutually recursive
}

func (c *packageCodegen) codegenMaxSize(f fsx.WriteableFile, max string, variants []types2.Variant, topLevel bool) string {
	if max == "" {
		a, b := variants[0], variants[1]
		asize := c.checker.FreshName("size").Name.Data
		bsize := c.checker.FreshName("size").Name.Data
		diff := c.checker.FreshName("diff").Name.Data
		max = c.checker.FreshName("max").Name.Data
		fmt.Fprintf(f, "const %s = int64(%s.Sizeof(*new(%s)))\n", asize, c.importUnique["unsafe"], a.Tag.Name.Data)
		fmt.Fprintf(f, "const %s = int64(%s.Sizeof(*new(%s)))\n", bsize, c.importUnique["unsafe"], b.Tag.Name.Data)
		fmt.Fprintf(f, "const %s = %s - %s\n", diff, asize, bsize)
		fmt.Fprintf(f, "const %s = %s - %s * ((%s>>(%s.UintSize-1))&1)\n", max, asize, diff, diff, c.importUnique["bits"])
		return c.codegenMaxSize(f, max, variants[2:], topLevel)
	} else if len(variants) == 0 {
		return max
	} else {
		curr := variants[0]
		size := c.checker.FreshName("size").Name.Data
		diff := c.checker.FreshName("diff").Name.Data
		newmax := c.checker.FreshName("max").Name.Data
		fmt.Fprintf(f, "const %s = int64(%s.Sizeof(*new(%s)))\n", size, c.importUnique["unsafe"], curr.Tag.Name.Data)
		fmt.Fprintf(f, "const %s = %s - %s\n", diff, size, max)
		fmt.Fprintf(f, "const %s = %s - %s * ((%s>>(%s.UintSize-1))&1)\n", newmax, size, diff, diff, c.importUnique["bits"])
		return c.codegenMaxSize(f, newmax, variants[1:], topLevel)
	}
}

func (c *packageCodegen) codegenExpr(f fsx.WriteableFile, x ast.Node, topLevel bool) []string {
	switch x := x.(type) {
	case *ast.Block:
		tblk := c.checker.GetType(x)
		tmp := c.checker.FreshName("").Name.Data             // TODO: also add to env
		fmt.Fprintf(f, "var %s %s\n", tmp, typeString(tblk)) // TODO: print go type
		// blocks are weird, cause they can be used in expression position.
		if topLevel {
			fmt.Fprintf(f, "var _ struct{} = func() struct{} {\n")
		} else {
			fmt.Fprintf(f, "{\n")
		}
		for i, elem := range x.Body {
			vars := c.codegenExpr(f, elem, false)
			if i == len(x.Body)-1 {
				if _, isStmt := elem.(*ast.Stmt); !isStmt {
					fmt.Fprintf(f, "%s = %s\n", tmp, vars[0])
				} else {
					fmt.Fprintf(f, "%s = struct{}{}\n", tmp) // is this necessary?
				}
			}
		}
		fmt.Fprintf(f, "_ = %s\n", tmp)
		if topLevel {
			fmt.Fprintf(f, "return struct{}{}\n")
			fmt.Fprintf(f, "}()\n")
		} else {
			fmt.Fprintf(f, "}\n")
		}
		// fmt.Fprintf(f, "}\n")
		return []string{tmp}
	case *ast.Stmt:
		vars := c.codegenExpr(f, x.Stmt, topLevel)
		// fmt.Fprintf(f, "\n")
		fmt.Fprintf(f, "_ = %s\n", vars[0])
		return []string{"_"}
	case *ast.CallExpr:
		if tX, ok := c.checker.GetType(x).(types2.Named); ok {
			if sum, ok := tX.Type.(types2.Sum); ok {
				if caller, ok := x.Elements[0].(*ast.SelectorExpr); ok {
					if _, _, ok := c.checker.CheckTypeSel(caller.X); ok {
						i := slices.IndexFunc(sum.Variants, func(v types2.Variant) bool { return v.Tag.Name.Data == caller.Name.Name.Data })
						variant := sum.Variants[i]
						// actually generate code for the constructor
						nvar := c.checker.FreshName("").Name.Data
						// argType := c.checker.GetType(x.Elements[1])
						v := c.checkCodegenPromote(f, x.Elements[1], variant, topLevel)
						// v := c.checkCodegenBinExp(f, x.Elements[1], variant, topLevel)
						// v = c.promoteCodegenTuple(f, variant, argType, v)
						fmt.Fprintf(f, "var %s %s = %s\n", nvar, variant.ConsName, v)
						return []string{nvar}
						// return []string{c.checkCodegenBinExp(f, x.Elements[1], variant, topLevel)}
					}
				}
			}
		}
	case *ast.LetDecl:
		tlet := c.checker.GetType(x.Destructure)
		goTLet := typeString(tlet)
		rhs := c.checkCodegenPromote(f, x.Rhs, tlet, topLevel)
		// switch r := x.Rhs.(type) {
		// case *ast.Number:
		// 	rhs = r.Lit.Data
		// case *ast.BasicString:
		// 	rhs = r.Lit.Data
		// case *ast.Tuple:
		// 	ttup := c.checker.GetType(r)
		// 	vars := c.codegenExpr(f, x.Rhs, topLevel)
		// 	if c.checker.GoConvertible(tlet, ttup) { // TODO: equivalent
		// 		rhs = vars[0]
		// 	} else {
		// 		rhs = fmt.Sprintf("*(*%s)(%s.Pointer(&%s))", goTLet, c.unsafeImport, vars[0])
		// 	}
		// default:
		// 	rhs = c.checkCodegenTuple(f, x.Rhs, tlet, topLevel)
		// }
		switch des := x.Destructure.(type) {
		case *ast.Ident:
			fmt.Fprintf(f, "var %s %s = %s\n", des.Name.Data, goTLet, rhs)
		case *ast.TypeAnnotation:
			if varname, ok := des.Destructure.(*ast.Ident); ok {
				fmt.Fprintf(f, "var %s %s = %s\n", varname.Name.Data, goTLet, rhs)
			}
		}
		return []string{"_"}
	case *ast.TypeDecl:
		tname := x.Name.Name.Data
		t := c.checker.GetType(x)
		if t, ok := t.(types2.Named); ok {
			if sum, ok := t.Type.(types2.Sum); ok {
				ifaceMethodName := c.checker.FreshName("").Name.Data
				fmt.Fprintf(f, "type %s interface { %s() }\n", tname, ifaceMethodName)
				for _, variant := range sum.Variants {
					fmt.Fprintf(f, "type %s %s\n", variant.ConsName, typeString(variant.Type))
					fmt.Fprintf(f, "func (%s) %s() {}\n", variant.ConsName, ifaceMethodName)
				}
				// if len(sum.Variants) == 1 {
				// 	fmt.Fprintf(f, "type %s struct { storage %s }\n", tname, sum.Variants[0].Tag.Name.Data)
				// } else {
				// 	maxVar := c.codegenMaxSize(f, "", sum.Variants, topLevel)
				// 	tagType := tagSizeType(len(sum.Variants))
				// 	fmt.Fprintf(f, "type %s struct { tag %s; storage [%s]byte }\n", tname, tagType, maxVar)
				// }
				// // generate each variant type now
				// // TODO: recursion will require unsafe.Pointer
				// // self-reference will also require fresh names to be valid
				// for _, variant := range sum.Variants {
				// 	fmt.Fprintf(f, "type %s %s\n", variant.Tag.Name.Data, typeString(variant.Type))
				// }
			} else {
				fmt.Fprintf(f, "type %s %s\n", tname, typeString(t.Type))
			}
			return nil
		}
		// if e, ok := c.checker.GetEnv(x); ok {
		// 	if b, ok := e.LookupLocal(tname); ok {
		// 		if b, ok := b.(types2.TypeBind); ok {
		// 			if t, ok := b.ReifiedType.(types2.Named); ok {
		// 				fmt.Fprintf(f, "type %s %s\n", tname, typeString(t.Type))
		// 				return nil
		// 			}
		// 		}
		// 	}
		// }
		panic("unreachable")
	case *ast.IndexExpr:
		expr := c.codegenExpr(f, x.X, topLevel)[0]
		tX := c.checker.GetType(x.X)
		switch tX := tX.(type) {
		case types2.Tuple:
			// why are we computing this twice?
			i, err := strconv.Atoi(x.IndexElements[0].(*ast.CommaElement).X.(*ast.Number).Lit.Data)
			if err != nil {
				panic(err)
			}
			// find the name of the ith element of the tuple
			fd := tX.Fields[i]
			var fname string
			if fd.Name == nil {
				fname = fmt.Sprintf("F%d", i)
			} else {
				fname = fd.Name.Name.Data
			}
			// if fd.Name != nil {
			// 	panic("named tuples not implemented yet")
			// }
			nvar := c.checker.FreshName("").Name.Data
			tR := c.checker.GetType(x)
			fmt.Fprintf(f, "var %s %s = %s.%s\n", nvar, typeString(tR), expr, fname)
			return []string{nvar}
		default:
			panic("unhandled indexable")
		}
	case *ast.PrefixExpr:
		expr := c.codegenExpr(f, x.X, topLevel)[0]
		tprefix := c.checker.GetType(x)
		goTPrefix := typeString(tprefix)
		pvar := c.checker.FreshName("").Name.Data
		fmt.Fprintf(f, "var %s %s = %s%s\n", pvar, goTPrefix, opString(x.Op.Type), expr)
		return []string{pvar}
	case *ast.BinaryExpr:
		// if left or right is a constant, we don't need temporaries
		var left string
		left = c.checkCodegenPromote(f, x.Left, c.checker.GetType(x.Left), topLevel)
		// switch l := x.Left.(type) {
		// case *ast.Number:
		// 	left = l.Lit.Data
		// case *ast.BasicString:
		// 	left = l.Lit.Data
		// default:
		// 	left = c.codegenExpr(f, x.Left, topLevel)[0]
		// }
		var right string
		right = c.checkCodegenPromote(f, x.Right, c.checker.GetType(x.Right), topLevel)
		// switch r := x.Right.(type) {
		// case *ast.Number:
		// 	right = r.Lit.Data
		// case *ast.BasicString:
		// 	right = r.Lit.Data
		// default:
		// 	right = c.codegenExpr(f, x.Right, topLevel)[0]
		// }
		tbin := c.checker.GetType(x)
		goTBin := typeString(tbin)
		bvar := c.checker.FreshName("")
		fmt.Fprintf(f, "var %s %s = %s %s %s\n", bvar.Name.Data, goTBin, left, opString(x.Op.Type), right)
		return []string{bvar.Name.Data}
	case *ast.SelectorExpr:
		// check if from package
		if pkgname, _, ok := c.checker.CheckPackageDef(x); ok {
			// get name of package
			return []string{fmt.Sprintf("%s.%s", c.importUnique[pkgname], x.Name.Name.Data)}
			// def.
			// _, _ = pkgname, def
		}
		v := c.checkCodegenPromote(f, x.X, c.checker.GetType(x.X), topLevel)
		tn := c.checker.GetType(x)
		nvar := c.checker.FreshName("").Name.Data
		fmt.Fprintf(f, "var %s %s = %s.%s\n", nvar, tn, v, x.Name.Name.Data)
		return []string{nvar}
		// otherwise from record
	case *ast.Number:
		nvar := c.checker.FreshName("")
		fmt.Fprintf(f, "var %s %s = %s\n", nvar.Name.Data, typeString(c.checker.GetType(x)), x.Lit.Data)
		return []string{nvar.Name.Data}
	case *ast.BasicString:
		nvar := c.checker.FreshName("")
		fmt.Fprintf(f, "var %s %s = %s\n", nvar.Name.Data, typeString(c.checker.GetType(x)), x.Lit.Data)
		return []string{nvar.Name.Data}
	case *ast.Ident:
		return []string{x.Name.Data}
	case *ast.IfElse:
		cvar := c.codegenExpr(f, x.IfHeader.Cond, topLevel)
		tif := c.checker.GetType(x)
		goTIf := typeString(tif)
		ifvar := c.checker.FreshName("").Name.Data
		fmt.Fprintf(f, "var %s %s\n", ifvar, goTIf)
		if topLevel {
			fmt.Fprintf(f, "var _ struct{} = func() struct{} {\n")
		}
		fmt.Fprintf(f, "if %s {\n", cvar[0])
		bodyVar := c.checkCodegenTuple(f, x.Body, tif, false)
		fmt.Fprintf(f, "%s = %s\n", ifvar, bodyVar)
		fmt.Fprintf(f, "} else {\n")
		elseVar := c.checkCodegenTuple(f, x.ElseBody, tif, false)
		fmt.Fprintf(f, "%s = %s\n", ifvar, elseVar)
		fmt.Fprintf(f, "}\n")
		if topLevel {
			fmt.Fprintf(f, "return struct{}{}\n")
			fmt.Fprintf(f, "}()\n")
		}
		return []string{ifvar}
	case *ast.If:
		cvar := c.codegenExpr(f, x.IfHeader.Cond, topLevel)
		tif := c.checker.GetType(x)
		goTIf := typeString(tif) // this is just unit
		ifvar := c.checker.FreshName("").Name.Data
		fmt.Fprintf(f, "var %s %s\n", ifvar, goTIf)
		if topLevel {
			fmt.Fprintf(f, "var _ struct{} = func() struct{} {\n")
		}
		fmt.Fprintf(f, "if %s {\n", cvar[0])
		bodyVar := c.codegenExpr(f, x.Body, false)
		fmt.Fprintf(f, "%s = %s\n", ifvar, bodyVar[0])
		fmt.Fprintf(f, "}\n")
		if topLevel {
			fmt.Fprintf(f, "return struct{}{}\n")
			fmt.Fprintf(f, "}()\n")
		}
		return []string{ifvar}
	case *ast.CommaElement:
		return c.codegenExpr(f, x.X, topLevel)
	case *ast.Tuple:
		// Just handling the 1-tuple with no trailing comma case for now
		if elem, ok := c.checker.CheckTupleParam(x); ok {
			if binExp, ok := elem.X.(*ast.BinaryExpr); ok && binExp.Op.Type == lexer.Assign {
				if _, isIdent := binExp.Left.(*ast.Ident); isIdent {
					goto inner
				}
			}
			return c.codegenExpr(f, elem.X, topLevel)
		}
	inner:
		var vars []string
		for _, elem := range x.Elements {
			if assignExp, ok := c.checker.CheckAssignElem(elem); ok {
				elem := assignExp.Right
				vars = append(vars, c.checkCodegenPromote(f, elem, c.checker.GetType(elem), topLevel))
			} else {
				vars = append(vars, c.checkCodegenPromote(f, elem, c.checker.GetType(elem), topLevel))
			}
		}
		if len(vars) != len(x.Elements) {
			panic("unequal number of elements")
		}
		nvar := c.checker.FreshName("")
		tstr := typeString(c.checker.GetType(x))
		fmt.Fprintf(f, "var %s = %s{", nvar.Name.Data, tstr)
		for _, v := range vars {
			fmt.Fprintf(f, "%s,", v)
		}
		fmt.Fprint(f, "}\n")
		return []string{nvar.Name.Data}

	// this is probably context-dependent
	// panic("codegen: unhandled tuple")
	// if expr == nil {
	// 	panic("codegen: unhandled tuple")
	// }
	// return c.codegenExpr(f, expr, topLevel)
	case *ast.ImportDecl:
		// I don't think i need to do anything here?
		return nil
	}
	panic(fmt.Sprintf("unhandled node: %T", x))
}

func checkUnderlyingTuple(t types2.Type) (res types2.Tuple, b bool) {
	switch t := t.(type) {
	case types2.Named:
		return checkUnderlyingTuple(t.Type)
	case types2.Variant:
		return checkUnderlyingTuple(t.Type)
	case types2.Tuple:
		return t, true
	default:
		return
	}
}

func (c *packageCodegen) promoteCodegenTuple(f fsx.WriteableFile, dst, src types2.Type, pvar string) string {
	// check that it needs to be promoted a rank
	if c.checker.RankPromotable(dst, src) {
		nvar := c.checker.FreshName("").Name.Data
		fmt.Fprintf(f, "var %s = %s{%s}\n", nvar, typeString(dst), pvar)
		return nvar
	}
	return pvar
	// guessing that src needs to be promoted one rank
	// src
}

func opString(t lexer.TokenType) string {
	switch t {
	case lexer.Plus:
		return "+"
	case lexer.Minus:
		return "-"
	case lexer.Times:
		return "*"
	case lexer.Divide:
		return "/"
	case lexer.Remainder:
		return "%"
	case lexer.LeftShift:
		return "<<"
	case lexer.RightShift:
		return ">>"
	case lexer.And:
		return "&"
	case lexer.Or:
		return "|"
	case lexer.Caret:
		return "^"
	case lexer.LogicalAnd:
		return "&&"
	case lexer.LogicalOr:
		return "||"
	case lexer.LogicalEquals:
		return "=="
	case lexer.NotEquals:
		return "!="
	case lexer.LessThan:
		return "<"
	case lexer.LessThanEquals:
		return "<="
	case lexer.GreaterThan:
		return ">"
	case lexer.GreaterThanEquals:
		return ">="
	case lexer.Not:
		return "!"
	case lexer.Assign, lexer.DotDot, lexer.LeftArrow, lexer.Exponentiation, lexer.Colon, lexer.Pipe, lexer.QuestionPipe, lexer.Tilde, lexer.QuestionMark:
		panic(fmt.Sprintf("unhandled binary op: %s", t))
	default:
		panic(fmt.Sprintf("unhandled binary op: %s", t))
	}
}

func typeString(t types2.Type) string {
	switch t := t.(type) {
	case types2.Base:
		return t.String()
	case types2.Named:
		return t.Name.Name.Data
	case types2.TypeVar:
		if t.Ref.Bound {
			return typeString(t.Ref.Type)
		}
	case types2.Variant:
		return t.ConsName
	case types2.Tuple:
		buf := new(strings.Builder)
		buf.WriteString("struct{")
		for i, f := range t.Fields {
			var fname string
			if f.Name == nil {
				fname = fmt.Sprintf("F%d", i)
			} else {
				fname = f.Name.Name.Data
			}
			fmt.Fprintf(buf, "%s %s;", fname, typeString(f.Type))
		}
		buf.WriteByte('}')
		return buf.String()
	}
	panic(fmt.Sprintf("TODO: typeString: %T", t))
}
