package types

import "github.com/smasher164/gflat/parser"

type visitorRec func(parser.Node) (parser.Node, bool)
type visitorFunc func(parser.Node, visitorRec) (parser.Node, bool)

func visit(n parser.Node, f visitorFunc) parser.Node {
	rec := func(x parser.Node) (parser.Node, bool) { return visit1(x, f) }
	n, _ = f(n, rec)
	return n
}

func visit1(n parser.Node, f visitorFunc) (parser.Node, bool) {
	rec := func(x parser.Node) (parser.Node, bool) { return visit1(x, f) }
	return visit2(n, func(x parser.Node) (parser.Node, bool) {
		return f(x, rec)
	})
}

func visit2(n parser.Node, f visitorRec) (parser.Node, bool) {
	var quit bool
	switch n := n.(type) {
	case parser.File:
		if n.PackageName, quit = f(n.PackageName); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body)
		return n, quit
	case parser.ImportDecl:
		n.Package, quit = f(n.Package)
		return n, quit
	case parser.ImportDeclPackage:
		if n.Binding, quit = f(n.Binding); quit {
			return n, quit
		}
		n.Path, quit = f(n.Path)
		return n, quit
	case parser.Package:
		for i := range n.PackageFiles {
			if n.PackageFiles[i], quit = f(n.PackageFiles[i]); quit {
				return n, quit
			}
		}
		for i := range n.ScriptFiles {
			if n.ScriptFiles[i], quit = f(n.ScriptFiles[i]); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.ImplDecl:
		if n.Name, quit = f(n.Name); quit {
			return n, quit
		}
		if n.Args, quit = f(n.Args); quit {
			return n, quit
		}
		if n.Clause, quit = f(n.Clause); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body)
		return n, quit
	case parser.ArrayType:
		n.Type, quit = f(n.Type)
		return n, quit
	case parser.NillableType:
		n.Type, quit = f(n.Type)
		return n, quit
	case parser.BinaryExpr:
		if n.Left, quit = f(n.Left); quit {
			return n, quit
		}
		n.Right, quit = f(n.Right)
		return n, quit
	case parser.Block:
		for i := range n.Body {
			if n.Body[i], quit = f(n.Body[i]); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.Tuple:
		for i := range n.Elements {
			if n.Elements[i], quit = f(n.Elements[i]); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.CommaElement:
		n.X, quit = f(n.X)
		return n, quit
	case parser.CallExpr:
		for i := range n.Elements {
			if n.Elements[i], quit = f(n.Elements[i]); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.LetFunction:
		for i := range n.TypeParams {
			if n.TypeParams[i], quit = f(n.TypeParams[i]); quit {
				return n, quit
			}
		}
		if n.Name, quit = f(n.Name); quit {
			return n, quit
		}
		if n.Signature, quit = f(n.Signature); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body)
		return n, quit
	case parser.Function:
		for i := range n.TypeParams {
			if n.TypeParams[i], quit = f(n.TypeParams[i]); quit {
				return n, quit
			}
		}
		if n.Name, quit = f(n.Name); quit {
			return n, quit
		}
		if n.Signature, quit = f(n.Signature); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body)
		return n, quit
	case parser.FunctionSignature:
		if n.Param, quit = f(n.Param); quit {
			return n, quit
		}
		for i := range n.Arrows {
			if n.Arrows[i], quit = f(n.Arrows[i]); quit {
				return n, quit
			}
		}
		n.Clause, quit = f(n.Clause)
		return n, quit
	case parser.Arrow:
		n.Type, quit = f(n.Type)
		return n, quit
	case parser.ForallType:
		if n.TypeArg, quit = f(n.TypeArg); quit {
			return n, quit
		}
		n.Type, quit = f(n.Type)
		return n, quit
	case parser.FunctionType:
		if n.Param, quit = f(n.Param); quit {
			return n, quit
		}
		for i := range n.Arrows {
			if n.Arrows[i], quit = f(n.Arrows[i]); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.IndexExpr:
		if n.X, quit = f(n.X); quit {
			return n, quit
		}
		for i := range n.IndexElements {
			if n.IndexElements[i], quit = f(n.IndexElements[i]); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.Array:
		for i := range n.Elements {
			if n.Elements[i], quit = f(n.Elements[i]); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.LetDecl:
		if n.Destructure, quit = f(n.Destructure); quit {
			return n, quit
		}
		n.Rhs, quit = f(n.Rhs)
		return n, quit
	case parser.VarDecl:
		if n.Destructure, quit = f(n.Destructure); quit {
			return n, quit
		}
		n.Rhs, quit = f(n.Rhs)
		return n, quit
	case parser.Stmt:
		n.Stmt, quit = f(n.Stmt)
		return n, quit
	case parser.Illegal:
		n.Node, quit = f(n.Node)
		return n, quit
	case parser.PrefixExpr:
		n.X, quit = f(n.X)
		return n, quit
	case parser.PostfixExpr:
		n.X, quit = f(n.X)
		return n, quit
	case parser.IfHeader:
		n.Cond, quit = f(n.Cond)
		return n, quit
	case parser.If:
		if n.IfHeader, quit = f(n.IfHeader); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body)
		return n, quit
	case parser.IfElse:
		if n.IfHeader, quit = f(n.IfHeader); quit {
			return n, quit
		}
		if n.Body, quit = f(n.Body); quit {
			return n, quit
		}
		n.ElseBody, quit = f(n.ElseBody)
		return n, quit
	case parser.IfMatch:
		if n.IfHeader, quit = f(n.IfHeader); quit {
			return n, quit
		}
		for i := range n.Cases {
			if n.Cases[i], quit = f(n.Cases[i]); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.PatternCase:
		if n.Pattern, quit = f(n.Pattern); quit {
			return n, quit
		}
		if n.Guard, quit = f(n.Guard); quit {
			return n, quit
		}
		n.Expr, quit = f(n.Expr)
		return n, quit
	case parser.TypeDecl:
		if n.Name, quit = f(n.Name); quit {
			return n, quit
		}
		for i := range n.TypeParams {
			if n.TypeParams[i], quit = f(n.TypeParams[i]); quit {
				return n, quit
			}
		}
		if n.Clause, quit = f(n.Clause); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body)
		return n, quit
	case parser.SumType:
		for i := range n.Elements {
			if n.Elements[i], quit = f(n.Elements[i]); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.SumTypeElement:
		if n.Name, quit = f(n.Name); quit {
			return n, quit
		}
		n.Type, quit = f(n.Type)
		return n, quit
	case parser.SelectorExpr:
		if n.X, quit = f(n.X); quit {
			return n, quit
		}
		n.Name, quit = f(n.Name)
		return n, quit
	case parser.Field:
		if n.Name, quit = f(n.Name); quit {
			return n, quit
		}
		if n.Type, quit = f(n.Type); quit {
			return n, quit
		}
		n.Default, quit = f(n.Default)
		return n, quit
	case parser.TypeAnnotation:
		if n.Destructure, quit = f(n.Destructure); quit {
			return n, quit
		}
		n.Type, quit = f(n.Type)
		return n, quit
	case Var:
		n.OriginalIdent, quit = f(n.OriginalIdent)
		return n, quit
	case TypeName:
		n.OriginalIdent, quit = f(n.OriginalIdent)
		return n, quit
	case UnresolvedIdent:
		n.OriginalIdent, quit = f(n.OriginalIdent)
		return n, quit
	case PackageName:
		n.OriginalIdent, quit = f(n.OriginalIdent)
		return n, quit
	case Cons:
		n.OriginalIdent, quit = f(n.OriginalIdent)
		return n, quit
	case ResolvedTypeArg:
		n.OriginalTypeVar, quit = f(n.OriginalTypeVar)
		return n, quit
	case UnresolvedTypeArg:
		n.OriginalTypeVar, quit = f(n.OriginalTypeVar)
		return n, quit
	default:
		return n, quit
		// return f(n)
	}
}
