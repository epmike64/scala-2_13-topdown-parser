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

	/*
		paramType:   type_  | '=>' type_  | type_ '*'
	 */
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

	def infixType(): Unit = {
		token.kind match {
			case LPAREN => {
				nextTk()
				_type()
				while (token.kind != RPAREN) {
					_type()
					if (token.kind == COMMA) {
						nextTk()
						_type()
					}
				}
				accept(RPAREN)
			}
			case _ => {
				qualId()

			}
		}
	}

	/*
		type_ : functionArgTypes '=>' type_ | infixType existentialClause?
	 */
	def _type(): Unit = {
		token.kind match {
			case LPAREN =>
				nextTk()
				while (token.kind != RPAREN) {
					paramType()
					if (token.kind == COMMA) {
						nextTk()
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
			case IDENTIFIER => ident()
			case UNDERSCORE => nextTk()
		}
		if (token.kind == LBRACKET) {
			nextTk()
			if (token.kind != RBRACKET) {
				variantTypeParam()
				while (token.kind == COMMA) {
					nextTk()
					variantTypeParam()
				}
			}
			accept(RBRACKET)
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
	
	/*
	simplePattern
				: '_'
				| Varid
				| literal
				| stableId ('(' patterns? ')')?
				| stableId '(' (patterns ',')? (Id '@')? '_' '*' ')'
				| '(' patterns? ')'
	 */
	def simplePattern(): Unit = {
		token.kind match {
			case UNDERSCORE => nextTk()
			case LITERAL => nextTk()
			case IDENTIFIER => {
				stableId()
				if (token.kind == LPAREN) {
					nextTk()
					if (token.kind != RPAREN) {
						patterns()
					}
					accept(RPAREN)
				}
			}
		}
	}
	/*
		enumerators : generator+
    	generator: pattern1 '<-' expr (guard_ | pattern1 '=' expr)*
    	pattern1: (BoundVarid | '_' | Id) ':' typePat | pattern2
    	pattern2: Id ('@' pattern3)?| pattern3
    	pattern3: simplePattern| simplePattern (Id NL? simplePattern)*
    	simplePattern
						 : '_'
						 | Varid
						 | literal
						 | stableId ('(' patterns? ')')?
						 | stableId '(' (patterns ',')? (Id '@')? '_' '*' ')'
						 | '(' patterns? ')'
						 
	 */
	def generator(): Unit = {
		token.kind match {
			case IDENTIFIER => {
				ident()
				token.kind match {
					case COLON => {
						nextTk()
						_type()
					}
					case AT => {
						nextTk()
						simplePattern()
					}
					case _ => simplePattern()
				}
			}
			case UNDERSCORE /*|BoundVarid*/ => {
				nextTk()
				accept(COLON)
				_type()
			}
		}
	}
	def enumerators(): Unit = {
		while (token.kind != RPAREN || token.kind != RBRACE) {
			generator()
		}
	}
	/*
	expr1
			 : 'if' '(' expr ')' NL* expr ('else' expr)?
			 | 'while' '(' expr ')' NL* expr
			 | 'try' expr ('catch' expr)? ('finally' expr)?
			 | 'do' expr 'while' '(' expr ')'
			 | 'for' ('(' enumerators ')' | '{' enumerators '}') 'yield'? expr
			 | 'throw' expr
			 | 'return' expr?
			 | ((simpleExpr | simpleExpr1 '_'?) '.')? Id '=' expr
			 | simpleExpr1 argumentExprs '=' expr
			 | postfixExpr ascription?
			 | postfixExpr 'match' '{' caseClauses '}'
	 */
	def expr1(): Unit = {
		token.kind match {
			case IF => {
				accept(LPAREN)
				expr()
				accept(RBRACE)
				expr()
				if(token.kind == ELSE) {
					nextTk()
					expr()
				}
			}
			case WHILE => {
				accept(LPAREN)
				expr()
				accept(RBRACE)
				expr()
			}
			case TRY => {
				expr()
				if(token.kind == CATCH) {
					nextTk()
					expr()
				}
				if(token.kind == FINALLY) {
					nextTk()
					expr()
				}
			}
			case DO => {
				expr()
				accept(WHILE)
				accept(LPAREN)
				expr()
				accept(RPAREN)
			}
			case FOR => {
				if(token.kind == LPAREN) {
					nextTk()
					enumerators()
					accept(RPAREN)
				}
				else {
					accept(LBRACE)
					enumerators()
					accept(RBRACE)
				}
				if(token.kind == YIELD) {
					nextTk()
				}
				expr()
			}
			case THROW => expr()
			case RETURN => ???
		}
	}
	/*  
   	expr: (bindings | 'implicit'? Id | '_') '=>' expr | expr1
			bindings: '(' binding (',' binding)* ')'
    			binding: (Id | '_') (':' type_)?
   	expr1:
			 'if' '(' expr ')' NL* expr ('else' expr)?
			 | 'while' '(' expr ')' NL* expr
			 | 'try' expr ('catch' expr)? ('finally' expr)?
			 | 'do' expr 'while' '(' expr ')'
			 | 'for' ('(' enumerators ')' | '{' enumerators '}') 'yield'? expr
			 | 'throw' expr
			 | 'return' expr?
			 | ((simpleExpr | simpleExpr1 '_'?) '.')? Id '=' expr
			 | simpleExpr1 argumentExprs '=' expr
			 | postfixExpr ascription?
			 | postfixExpr 'match' '{' caseClauses '}'
	*/

	def expr(): Unit = {
		while(token.kind == IDENTIFIER || token.kind == UNDERSCORE) {
			nextTk()
			if (token.kind == COLON) {
				nextTk()
				_type()
			}
		}
		if(token.kind == FAT_ARROW){
			nextTk()
			expr()
		}
		else {
			expr1()
		}
	}
	/*
		classParam: annotation* modifier* ('val' | 'var')? Id ':' paramType ('=' expr)?
	 */
	def classParam(): Unit = {
		val mods = modifiersOpt()
		token.kind match {
			case VAR | VAL => nextTk()
		}
		ident()
		accept(COLON)
		paramType()
		if (token.kind == EQ) {
			nextTk()
			expr()
		}
	}

	/**
	 * ClassDef ::= id [TypeParamClause] {Annotation} [AccessModifier] ClassParamClauses classTemplateOpt
	 */
	def classDef(isCase: Boolean): FTree = {
		val startPos = token.pos
		accept(CLASS)
		val name = ident()
		/*
		typeParamClause: '[' variantTypeParam (',' variantTypeParam)* ']'
		 */
		if (token.kind == LBRACKET) {
			nextTk()
			variantTypeParam()
			while (token.kind == COMMA) {
				nextTk()
				variantTypeParam()
			}
			accept(RBRACKET)
		}
		/*
		accessModifier: ('private' | 'protected') accessQualifier?
		 */
		token.kind match {
			case PRIVATE | PROTECTED =>
				nextTk()
				if (token.kind == LBRACKET) {
					accessQualifier()
				}
		}
		/*
			classParamClauses: classParamClause* (NL? '(' 'implicit' classParams ')')?
			classParamClause: NL? '(' classParams? ')'
			classParams: classParam (',' classParam)*
			classParam: annotation* modifier* ('val' | 'var')? Id ':' paramType ('=' expr)?
		 */
		if (token.kind == LPAREN) {
			nextTk()
			while (token.kind != RPAREN) {
				classParam()
				if (token.kind == COMMA) {
					nextTk()
					classParam()
				}
			}
			accept(RPAREN)
		}

		if (token.kind == EXTENDS) {
			nextTk()
			if (token.kind == LBRACE) {
				nextTk()
				//earlyDefs()
				accept(RBRACE)
				accept(WITH)
			}
		}
		if (token.kind == LPAREN) {//templateBody
			nextTk()
			
			accept(RPAREN)
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

