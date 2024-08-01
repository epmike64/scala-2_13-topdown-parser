package com.fdk.compiler.parser

import com.fdk.compiler.FToken
import com.fdk.compiler.parser.FTokenKind.*
import com.fdk.compiler.tree.{FExpression, FImport, FModifiers, FPackageDecl, FTree, FTreeMaker}
import com.fdk.compiler.util.FName

import scala.collection.mutable.ArrayBuffer

class FParser(lexer: IFLexer) extends IFParser {
	{
		lexer.nextToken()
	}
	private[this] var token: FToken = lexer.token
	private[this] val F: FTreeMaker = new FTreeMaker()
	private[this] val endPosTable: scala.collection.mutable.Map[FTree, Int] = scala.collection.mutable.Map()

	private def nextTk(): Unit = {
		token = lexer.nextToken()
	}

	def toP[T <: FTree](t: T): T = {
		t
	}

	/** If next input token matches given token, skip it, otherwise report
	 * an error.
	 */
	private def accept(tk: FTokenKind): Unit = {
		if token.kind == tk then nextTk()
		else {
			setErrorEndPos(token.pos)
			reportSyntaxError(token.pos, "expected", tk)
		}
	}

	def setErrorEndPos(errPos: Int): Unit = {
		//endPosTable.setErrorEndPos(errPos)
	}

	def reportSyntaxError(pos: Int, key: String, args: Any): Unit = {
		//reporter.syntaxError(token.offset, msg)
	}

	def ident(): FName = {
		val name = token.name
		accept(IDENTIFIER)
		name
	}

	/**
	 * Qualident = Ident { DOT [Annotations] Ident }
	 */
	def qualId(): FExpression = {
		var t: FExpression = toP(F.at(token.pos).ident(ident()))
		while (token.kind == DOT) {
			val pos = token.pos
			nextTk()
			t = toP(F.at(token.pos).select(t, ident()))
		}
		t
	}

	def packageDecl(): FPackageDecl = {
		val startPos = token.pos
		nextTk()
		val pid = qualId()
		val pd = F.at(startPos).packageDecl(pid)
		endPosTable(pd) = token.pos
		pd
	}

	def importDecl(): FImport = {
		val startPos = token.pos
		nextTk()
		val id = qualId()
		val imp = F.at(startPos).makeImport(id)
		endPosTable(imp) = token.pos
		imp
	}

	def paramType(): Unit = {
		token.kind match {
			case FAT_ARROW => {
				nextTk()
				_type()
			}
			case _ => {
				_type()
				nextTk()
				token.kind match {
					case STAR => nextTk()
				}
			}
		}
	}
	/*
	Type ::= FunctionArgTypes ‘=>’ Type  | InfixType [ExistentialClause]
	FunctionArgTypes ::= InfixType   | ‘(’ [ ParamType {‘,’ ParamType } ] ‘)’
	 */
	def _type(): Unit = {
		token.kind match {
			case LPAREN =>
				nextTk()
				if (token.kind != RPAREN) {
					paramType()
					while (token.kind == COMMA) {
						nextTk()
						paramType()
					}
				}
				accept(RPAREN)
				if (token.kind == FAT_ARROW) {
					nextTk()
					_type()
				}
			case _ => infixType()
		}
	}

	def typeParam(): Unit = {
		token.kind match {
			case UNDERSCORE => nextTk()
			case IDENTIFIER => ident()
		}
		if(token.kind == LBRACKET){
			typeParamClause()
		}
		if (token.kind == LOWER_BOUND) {
			nextTk()
			_type()
		}
		if (token.kind == UPPER_BOUND) {
			nextTk()
			_type()
		}
		if (token.kind == COLON) {
			nextTk() //Context bound
			_type()
		}
	}
	
	def classQualifier(): Unit = {
		accept(LBRACKET)
		ident()
		accept(RBRACKET)
	}

	def stableId(): Unit = {
		token.kind match {
			case IDENTIFIER => {
				ident()
				if (token.kind == DOT) {
					nextTk()
					stableId()
				}
			}

			case THIS => {
				nextTk()
				//Some(F.at(token.pos).ident(FName("this")))
			}
			case SUPER => {
				nextTk() 
				if (token.kind == LBRACKET) {
					classQualifier()
				}
				accept(DOT)
				ident()
			}
		}
	}

	def variantTypeParam(): Unit = {
		token.kind match {
			case PLUS | SUB => nextTk()
		}
		typeParam()
	}

	def typeParamClause(): Option[FTree] = {
		accept(LBRACKET)
		variantTypeParam()
		while (token.kind == COMMA) {
			nextTk()
			variantTypeParam()
		}
		accept(RBRACKET)
		None
	}

	def classDef(isCase: Boolean): FTree = {
		val startPos = token.pos
		accept(CLASS)
		val name = ident()
		if (token.kind == LBRACKET) {
			typeParamClause()
		}
		val cd = F.at(startPos).makeClassDecl()
		endPosTable(cd) = token.pos
		cd
	}

	def objectDef(isCase: Boolean): FTree = {
		val startPos = token.pos
		accept(OBJECT)
		val name = ident()
		val od = F.at(startPos).makeClassDecl()
		endPosTable(od) = token.pos
		od
	}

	def traitDef(): FTree = {
		val startPos = token.pos
		accept(TRAIT)
		val name = ident()
		val td = F.at(startPos).makeTraitDecl()
		endPosTable(td) = token.pos
		td
	}

	def accessQualifier(): Unit = {
		accept(LBRACKET)
		token.kind match {
			case IDENTIFIER => ident()
			case THIS => nextTk()
		}
		accept(RBRACKET)
	}
	
	def modifier(): Option[Int] = {

		token.kind match

			case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY | OVERRIDE =>
				nextTk()
				Some(1)

			case PRIVATE | PROTECTED =>
				nextTk()
				if (token.kind == LBRACKET) {
					accessQualifier()
				}
				Some(2)

			case _ =>
				None
	}

	def modifiersOpt(): FModifiers = {
		val mods = new FModifiers()
		while (token.kind != EOF) {
			val mod: Option[Int] = modifier()
			mod match {
				case Some(n) => ??? // mods.addFlag(FModifiers.ABSTRACT)
				case _ => return mods
			}
		}
		mods
	}


	def tmplDef(mods: FModifiers): FTree = {
		token.kind match {
			case CASE =>
				nextTk()
				token.kind match {
					case CLASS => classDef(true)
					case OBJECT => objectDef(true)
					case _ => {
						reportSyntaxError(token.pos, "expected", CLASS)
						F.makeSkip()
					}
				}
			case CLASS => classDef(false)
			case OBJECT => objectDef(false)
			case TRAIT => traitDef()
			case _ => {
				reportSyntaxError(token.pos, "expected", CLASS)
				F.makeSkip()
			}
		}
	}

	def topStatement(): FTree = {
		token.kind match {
			case IMPORT => importDecl()
			case _ =>
				val mods = modifiersOpt()
				tmplDef(mods)
		}
	}

	def topStatementSeq(): ArrayBuffer[FTree] = {
		val defs: ArrayBuffer[FTree] = ArrayBuffer()
		while (token.kind != EOF) {
			defs += topStatement()
		}
		defs
	}

	def compilationUnit(): ArrayBuffer[FTree] = {

		val defs: ArrayBuffer[FTree] = ArrayBuffer()

		while (token.kind == PACKAGE) {
			defs += packageDecl()
		}

		defs ++= topStatementSeq()
		defs
	}
}

