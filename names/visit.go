package names

import "github.com/smasher164/gflat/parser"

// i may not need this after all
func isLeaf(n parser.Node) bool {
	switch n.(type) {
	case nil, parser.Ident, parser.EmptyExpr, parser.Number, parser.NamedTypeArgument, parser.StringPart, parser.String:
		return true
	}
	return false
}

type visitorRec func(parser.Node) (parser.Node, bool)
type visitorFunc func(parser.Node, visitorRec) (parser.Node, bool)

func visit(n parser.Node, f visitorFunc) parser.Node {
	rec := func(x parser.Node) (parser.Node, bool) { return visit1(x, f) }
	n, _ = f(n, rec)
	return n
}

func visit1(n parser.Node, f visitorFunc) (parser.Node, bool) {
	rec := func(x parser.Node) (parser.Node, bool) { return visit1(x, f) }
	var quit bool
	switch n := n.(type) {
	case parser.File:
		if n.PackageName, quit = f(n.PackageName, rec); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body, rec)
		return n, quit
	case parser.ImportDecl:
		n.Package, quit = f(n.Package, rec)
		return n, quit
	case parser.ImportDeclPackage:
		if n.Binding, quit = f(n.Binding, rec); quit {
			return n, quit
		}
		n.Path, quit = f(n.Path, rec)
		return n, quit
	case parser.Package:
		for i := range n.PackageFiles {
			if n.PackageFiles[i], quit = f(n.PackageFiles[i], rec); quit {
				return n, quit
			}
		}
		for i := range n.ScriptFiles {
			if n.ScriptFiles[i], quit = f(n.ScriptFiles[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.ImplDecl:
		if n.Name, quit = f(n.Name, rec); quit {
			return n, quit
		}
		if n.Args, quit = f(n.Args, rec); quit {
			return n, quit
		}
		if n.Clause, quit = f(n.Clause, rec); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body, rec)
		return n, quit
	case parser.ArrayType:
		n.Type, quit = f(n.Type, rec)
		return n, quit
	case parser.NillableType:
		n.Type, quit = f(n.Type, rec)
		return n, quit
	case parser.BinaryExpr:
		if n.Left, quit = f(n.Left, rec); quit {
			return n, quit
		}
		n.Right, quit = f(n.Right, rec)
		return n, quit
	case parser.Block:
		for i := range n.Body {
			if n.Body[i], quit = f(n.Body[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.Tuple:
		for i := range n.Elements {
			if n.Elements[i], quit = f(n.Elements[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.CommaElement:
		n.X, quit = f(n.X, rec)
		return n, quit
	case parser.CallExpr:
		for i := range n.Elements {
			if n.Elements[i], quit = f(n.Elements[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.LetFunction:
		if n.Name, quit = f(n.Name, rec); quit {
			return n, quit
		}
		if n.Signature, quit = f(n.Signature, rec); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body, rec)
		return n, quit
	case parser.Function:
		if n.Name, quit = f(n.Name, rec); quit {
			return n, quit
		}
		if n.Signature, quit = f(n.Signature, rec); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body, rec)
		return n, quit
	case parser.FunctionSignature:
		if n.Param, quit = f(n.Param, rec); quit {
			return n, quit
		}
		for i := range n.Arrows {
			if n.Arrows[i], quit = f(n.Arrows[i], rec); quit {
				return n, quit
			}
		}
		n.Clause, quit = f(n.Clause, rec)
		return n, quit
	case parser.Arrow:
		n.Type, quit = f(n.Type, rec)
		return n, quit
	case parser.ForallType:
		if n.TypeArg, quit = f(n.TypeArg, rec); quit {
			return n, quit
		}
		n.Type, quit = f(n.Type, rec)
		return n, quit
	case parser.FunctionType:
		if n.Param, quit = f(n.Param, rec); quit {
			return n, quit
		}
		for i := range n.Arrows {
			if n.Arrows[i], quit = f(n.Arrows[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.IndexExpr:
		if n.X, quit = f(n.X, rec); quit {
			return n, quit
		}
		for i := range n.IndexElements {
			if n.IndexElements[i], quit = f(n.IndexElements[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.LetDecl:
		if n.Destructure, quit = f(n.Destructure, rec); quit {
			return n, quit
		}
		n.Rhs, quit = f(n.Rhs, rec)
		return n, quit
	case parser.VarDecl:
		if n.Destructure, quit = f(n.Destructure, rec); quit {
			return n, quit
		}
		n.Rhs, quit = f(n.Rhs, rec)
		return n, quit
	case parser.Stmt:
		n.Stmt, quit = f(n.Stmt, rec)
		return n, quit
	case parser.Illegal:
		n.Node, quit = f(n.Node, rec)
		return n, quit
	case parser.PrefixExpr:
		n.X, quit = f(n.X, rec)
		return n, quit
	case parser.PostfixExpr:
		n.X, quit = f(n.X, rec)
		return n, quit
	case parser.IfHeader:
		n.Cond, quit = f(n.Cond, rec)
		return n, quit
	case parser.If:
		if n.IfHeader, quit = f(n.IfHeader, rec); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body, rec)
		return n, quit
	case parser.IfElse:
		if n.IfHeader, quit = f(n.IfHeader, rec); quit {
			return n, quit
		}
		if n.Body, quit = f(n.Body, rec); quit {
			return n, quit
		}
		n.ElseBody, quit = f(n.ElseBody, rec)
		return n, quit
	case parser.IfMatch:
		if n.IfHeader, quit = f(n.IfHeader, rec); quit {
			return n, quit
		}
		for i := range n.Cases {
			if n.Cases[i], quit = f(n.Cases[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.PatternCase:
		if n.Pattern, quit = f(n.Pattern, rec); quit {
			return n, quit
		}
		if n.Guard, quit = f(n.Guard, rec); quit {
			return n, quit
		}
		n.Expr, quit = f(n.Expr, rec)
		return n, quit
	case parser.TypeDecl:
		if n.Name, quit = f(n.Name, rec); quit {
			return n, quit
		}
		for i := range n.TypeParams {
			if n.TypeParams[i], quit = f(n.TypeParams[i], rec); quit {
				return n, quit
			}
		}
		if n.Clause, quit = f(n.Clause, rec); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body, rec)
		return n, quit
	case parser.SumType:
		for i := range n.Elements {
			if n.Elements[i], quit = f(n.Elements[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.SumTypeElement:
		if n.Name, quit = f(n.Name, rec); quit {
			return n, quit
		}
		n.Type, quit = f(n.Type, rec)
		return n, quit
	case parser.SelectorExpr:
		if n.X, quit = f(n.X, rec); quit {
			return n, quit
		}
		n.Name, quit = f(n.Name, rec)
		return n, quit
	case parser.Field:
		if n.Name, quit = f(n.Name, rec); quit {
			return n, quit
		}
		if n.Type, quit = f(n.Type, rec); quit {
			return n, quit
		}
		n.Default, quit = f(n.Default, rec)
		return n, quit
	case parser.TypeAnnotation:
		if n.Destructure, quit = f(n.Destructure, rec); quit {
			return n, quit
		}
		n.Type, quit = f(n.Type, rec)
		return n, quit
	case Var:
		n.OriginalIdent, quit = f(n.OriginalIdent, rec)
		return n, quit
	case TypeName:
		n.OriginalIdent, quit = f(n.OriginalIdent, rec)
		return n, quit
	case UnresolvedIdent:
		n.OriginalIdent, quit = f(n.OriginalIdent, rec)
		return n, quit
	case PackageName:
		n.OriginalIdent, quit = f(n.OriginalIdent, rec)
		return n, quit
	case Cons:
		n.OriginalIdent, quit = f(n.OriginalIdent, rec)
		return n, quit
	case TypeVar:
		n.OriginalTypeVar, quit = f(n.OriginalTypeVar, rec)
		return n, quit
	case UnresolvedTypeVar:
		n.OriginalTypeVar, quit = f(n.OriginalTypeVar, rec)
		return n, quit
	default:
		return n, quit
		// return f(n, rec)
	}
}
