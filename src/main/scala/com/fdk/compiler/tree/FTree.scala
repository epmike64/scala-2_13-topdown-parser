package com.fdk.compiler.tree



import com.fdk.compiler.util.FName

import scala.collection.mutable.ArrayBuffer

enum FTreeTag:
	/** 
	 * For methods that return an invalid tag if a given condition is not met
	 */
	case NO_TAG
	
	/** 
	 * Toplevel nodes, of type TopLevel, representing entire source files.
	 */
	case TOPLEVEL

	/** 
	 * Package level definitions.
	 */
	case PACKAGEDEF


	/** Import clauses, of type Import.
	 */
	case IMPORT

	/** Class definitions, of type ClassDef.
	 */
	case CLASSDEF
	
	
	/** Method definitions, of type MethodDef.
	 */
	case METHODDEF
	
	
	/** Variable definitions, of type VarDef.
	 */
	case VARDEF

	
	/** The no-op statement ";", of type Skip
	 */
	case SKIP


	/** Blocks, of type Block.
	 */
	case BLOCK


	/** Do-while loops, of type DoLoop.
	 */
	case DOLOOP


	/** While-loops, of type WhileLoop.
	 */
	case WHILELOOP


	/** For-loops, of type ForLoop.
	 */
	case FORLOOP

	/** Foreach-loops, of type ForeachLoop.
	 */
	case FOREACHLOOP


	/** Labelled statements, of type Labelled.
	 */
	case LABELLED

	/** Case parts in switch statements, of type Case.
	 */
	case CASE

	/** Synchronized statements, of type Synchonized.
	 */
	case SYNCHRONIZED


	/** Try statements, of type Try.
	 */
	case TRY


	/** Catch clauses in try statements, of type Catch.
	 */
	case CATCH


	/** Conditional expressions, of type Conditional.
	 */
	case CONDEXPR

	/** Conditional statements, of type If.
	 */
	case IF

	/** Expression statements, of type Exec.
	 */
	case EXEC


	/** Break statements, of type Break.
	 */
	case BREAK


	/** Continue statements, of type Continue.
	 */
	case CONTINUE


	/** Return statements, of type Return.
	 */
	case RETURN
	

	/** Throw statements of type Throw.
	 */
	case THROW


	/** Assert statements, of type Assert.
	 */
	case ASSERT


	/** Method invocation expressions of type Apply.
	 */
	case APPLY


	/** Class instance creation expressions of type NewClass.
	 */
	case NEWCLASS


	/** Array creation expressions of type NewArray.
	 */
	case NEWARRAY


	/** Lambda expression of type Lambda.
	 */
	case LAMBDA


	/** Parenthesized subexpressions of type Parens.
	 */
	case PARENS


	/** Assignment expressions of type Assign.
	 */
	case ASSIGN


	/** Type cast expressions of type TypeCast.
	 */
	case TYPECAST


	/** Type test expressions of type TypeTest.
	 */
	case TYPETEST


	/** Indexed array expressions of type Indexed.
	 */
	case INDEXED


	/** Selections of type Select.
	 */
	case SELECT


	/** Member references of type Reference.
	 */
	case REFERENCE


	/** Simple identifiers of type Ident.
	 */
	case IDENT


	/** Literals of type Literal.
	 */
	case LITERAL


	/** Basic type identifiers of type TypeIdent.
	 */
	case TYPEIDENT


	/** Array types of type TypeArray.
	 */
	case TYPEARRAY


	/** Parameterized types of type TypeApply.
	 */
	case TYPEAPPLY


	/** Union types of type TypeUnion.
	 */
	case TYPEUNION


	/** Intersection types of type TypeIntersection.
	 */
	case TYPEINTERSECTION


	/** Formal type parameters of type TypeParameter.
	 */
	case TYPEPARAMETER


	/** Type argument.
	 */
	case WILDCARD


	/** Bound kind: extends super exact or unbound
	 */
	case TYPEBOUNDKIND


	/** metadata: Annotation.
	 */
	case ANNOTATION


	/** metadata: Type annotation.
	 */
	case TYPE_ANNOTATION


	/** metadata: Modifiers
	 */
	case MODIFIERS


	/** An annotated type tree.
	 */
	case ANNOTATED_TYPE


	/** Error trees of type Erroneous.
	 */
	case ERRONEOUS


	/** Unary operators of type Unary.
	 */
	case POS // +                             // +// +
	case NEG // -                             // -// -
	case NOT // !                             // !// !
	case COMPL // ~                           // ~// ~
	case PREINC // ++ _                          // ++ _// ++ _
	case PREDEC // -- _                          // -- _// -- _
	case POSTINC // _ ++                         // _ ++// _ ++
	case POSTDEC // _ --                         // _ --// _ --

	/** unary operator for null reference checks, only used internally.
	 */
	case NULLCHK

	/** Binary operators, of type Binary.
	 */
	case OR // ||                              // ||// ||
	case AND // &&                             // &&// &&
	case BITOR // |                           // |// |
	case BITXOR // ^                          // ^// ^
	case BITAND // &                          // &// &
	case EQ // ==                              // ==// ==
	case NE // !=                              // !=// !=
	case LT // <                              // <// <
	case GT // >                              // >// >
	case LE // <=                              // <=// <=
	case GE // >=                              // >=// >=
	case SL // <<                              // <<// <<
	case SR // >>                              // >>// >>
	case USR // >>>                             // >>>// >>>
	case PLUS // +                            // +// +
	case MINUS // -                           // -// -
	case MUL // *                             // *// *
	case DIV // /                             // /// /
	case MOD // %                             // %// %

	/** Assignment operators, of type Assignop.
	 */
	case BITOR_ASG // |=                // |=// |=
	case BITXOR_ASG // ^=              // ^=// ^=
	case BITAND_ASG // &=              // &=// &=

	case SL_ASG // <<=                      // <<=// <<=
	case SR_ASG // >>=                      // >>=// >>=
	case USR_ASG // >>>=                    // >>>=// >>>=
	case PLUS_ASG // +=                  // +=// +=
	case MINUS_ASG // -=                // -=// -=
	case MUL_ASG // *=                    // *=// *=
	case DIV_ASG // /=                    // /=// /=
	case MOD_ASG // %=                    // %=// %=

	case MODULEDEF

	case EXPORTS

	case OPENS

	case PROVIDES

	case REQUIRES
	case USES


trait FTree {

	var pos: Int = -1
	def getTag: FTreeTag
	def accept(visitor: IFVisitor): Unit
}







class FAnnotatedType extends FTree {
	val annotations = ArrayBuffer[FAnnotation]()
	val underlying = null
}
class FAnnotation extends FTree {
	val annotationType = null
	val args = ArrayBuffer[FExpression]()
}
class FArrayAccess extends FTree {
	val indexed = null
	val index = null
}

class FArrayTypeTree extends FTree {
	val elemtype = null
}
class FAssert extends FTree {
	val cond = null
	val detail = null
}
class FAssign extends FTree {
	val lhs = null
	val rhs = null
}
class FAssignOp extends FTree {
	val lhs = null
	val op = null
	val rhs = null
}
class FBinary extends FTree {
	val lhs = null
	val op = null
	val rhs = null
}
class FBlock 		extends FTree {
	val stats = ArrayBuffer[FTree]()
}
class FBreak 		extends FTree
class FCase 		extends FTree {
	val pat = null
	val stats = ArrayBuffer[FTree]()
}
class FCatch 		extends FTree {
	val param = null
	val body = null
}

class FClassDecl extends FTree {
	val mods = new FModifiers
	val name = null
	val tparams = ArrayBuffer[FTypeParameter]()
	val impl = null
	val stats = ArrayBuffer[FTree]()
}

class FConditional extends FTree {
	val cond = null
	val thenp = null
	val elsep = null
}
class FContinue 	extends FTree
class FDirective 	extends FTree
class FDoWhileLoop extends FTree {
	val cond = null
	val body = null
}
class FEnhancedForLoop extends FTree {
	val varDecl = null
	val expr = null
	val body = null
}
class FErroneous 	extends FTree
class FExports 	extends FTree {
	val qual = null
	val mods = new FModifiers
	val exports = ArrayBuffer[FIdent]()
}
class FExpression extends FTree {
	val typetag = 0

}
class FExpressionStatement 	extends FTree {
	val expr = null
}

class FFieldAccess(val selected: FExpression, name: FName, symbol: Symbol) extends FExpression {

	override def accept(visitor: IFVisitor): Unit = {
		visitor.visitSelect(this)
	}

	override def getTag: FTreeTag = FTreeTag.SE
}
class FForLoop extends FTree {
	val init = ArrayBuffer[FTree]()
	val cond = null
	val step = ArrayBuffer[FTree]()
	val body = null
}
class FFunctionalExpression extends FTree
class FIdent(val name: FName) extends FExpression {
}

class FIf extends FTree {
	val cond = null
	val thenp = null
	val elsep = null
}
case class FImport(qualId: FTree) extends FTree {
	override def accept(visitor: IFVisitor): Unit = {
		visitor.visitImport(this)
	}
}

class FInstanceOf extends FTree {
	val expr = null
	val clazz = null
}
class FLabeledStatement 
class FLambda extends FTree {
	val params = ArrayBuffer[FVariableDecl]()
	val body = null
}
class FLiteral extends FTree {
	val value = null
}
class FMemberReference extends FTree {
	val expr = null
	val name = null
}
class FMethodDecl extends FTree {
	val mods = new FModifiers
	val name = null
	val tparams = ArrayBuffer[FTypeParameter]()
	val params = ArrayBuffer[FVariableDecl]()
	val restype = null
	val body = null
}
class FMethodInvocation extends FTree {
	val meth = null
	val targs = ArrayBuffer[FExpression]()
	val args = ArrayBuffer[FExpression]()
}
class FModifiers extends FTree {
	val flags = 0
}
class FModuleDecl extends FTree {
	val mods = new FModifiers
	val name = null
	val directives = ArrayBuffer[FDirective]()
}
class FNewArray extends FTree {
	val elemtype = null
	val dims = ArrayBuffer[FExpression]()
	val elems = ArrayBuffer[FExpression]()
}
class FNewClass extends FTree {
	val encl = null
	val typeArgs = ArrayBuffer[FExpression]()
	val clazz = null
	val args = ArrayBuffer[FExpression]()
	val body = null
}

class FOpens extends FTree {
	val qual = null
	val mods = new FModifiers
	val opens = ArrayBuffer[FIdent]()
}
class FOperatorExpression extends FTree

/**
 * A package declaration.
 * @param pid
 */
class FPackageDecl(val pid: FExpression) extends FTree {
	override def accept(visitor: IFVisitor): Unit = {
		visitor.visitPackageDef(this)
	}
	override def getTag: FTreeTag = FTreeTag.PACKAGEDEF
}
class FParens extends FTree {
	val expr = null
}
class FPolyExpression extends FTree
class FPrimitiveTypeTree extends FTree {
	val typetag = 0
}
class FProvides extends FTree {
	val qual = null
	val mods = new FModifiers
	val service = null
	val impls = ArrayBuffer[FIdent]()
}
class FRequires extends FTree {
	val qual = null
	val mods = new FModifiers
	val req = null
}
class FReturn extends FTree {
	val expr = null
}
class FSkip extends FTree


class FSwitch extends FTree {
	val selector = null
	val cases = ArrayBuffer[FCase]()
}

class FCompilationUnit extends FTree {
	val pid = null
	val imports = ArrayBuffer[FImport]()
	/** All definitions in this file (ClassDef, Import, and Skip) */
	val defs = ArrayBuffer[FTree]()

	override def accept(visitor: IFVisitor): Unit = {
		visitor.visitTopLevel(this)
	}
}