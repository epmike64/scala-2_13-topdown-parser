package com.fdk.compiler.parser

import com.fdk.compiler.FToken
import com.fdk.compiler.parser.FTokenKind.*
import com.fdk.compiler.tree.{FExpression, FImport, FModifiers, FPackageDecl, FTree, FTreeMaker}
import com.fdk.compiler.util.FName

import scala.collection.mutable.ArrayBuffer

class FParser(lexer: IFLexer) extends IFParser {

	private[this] var token: FToken = lexer.next

	def next(): Unit = {
		token = lexer.next
	}
	
	def skip(n: Int): Unit = {
		if(n == 1) next()
		else if (n > 1) token = lexer.skip(n)
		else throw new IllegalArgumentException("n must be positive")
	}
	
	def lookAhead(n: Int): FToken = {
		if(n == 0) token
		else if(n > 0) lexer.lookAhead(n)
		else throw new IllegalArgumentException("n must be positive")
	}
	/** If next input token matches given token, skip it, otherwise report
	 * an error.
	 */
	def accept(tk: FTokenKind): Unit = {
		if(token.kind == tk) next()
		else {
			setErrorEndPos(token.pos)
			reportSyntaxError(token.pos, "expected", tk)
		}
	}

	def acceptOneOf(kinds: FTokenKind*): Unit = {
		if(kinds.contains(token.kind)) next()
		else {
			setErrorEndPos(token.pos)
			reportSyntaxError(token.pos, "expected", kinds: _*)
		}
	}
	
	def isTokenLa(kinds: FTokenKind*): Boolean = {
		for(i <- 0 until kinds.length){
			if(lexer.lookAhead(i).kind != kinds(i)) return false
		}
		true
	}
	

	def isTokenLaOneOf(n: Int, kinds: FTokenKind*): Boolean = {
		kinds.contains(lookAhead(n))
	}
	
	def isToken(kind: FTokenKind): Boolean = {
		token.kind == kind
	}

	def eatUp(kind: FTokenKind): Int = {
		var n = 0
		while(token.kind == kind){
			n += 1 
			next()
		}
		n
	}
	
	def eatUpCount(kind: FTokenKind, count: Int): Unit = {
		for(i <- 0 until count){
			if(token.kind == kind) next()
			else {
				setErrorEndPos(token.pos)
				reportSyntaxError(token.pos, "expected", kind)
			}
		}
	}
	
	def isTokenOneOf(kinds: FTokenKind*): Boolean = {
		kinds.contains(token.kind)
	}
	
	def setErrorEndPos(errPos: Int): Unit = {
		//endPosTable.setErrorEndPos(errPos)
	}

	def reportSyntaxError(pos: Int, key: String, kinds: FTokenKind*): Unit = {
		//reporter.syntaxError(token.offset, msg)
	}

	def ident(): FName = {
		val name = token.name
		accept(ID)
		name
	}
	
	def qualId(): Unit = {
		ident()
		while(token.kind == DOT){
			next()
			ident()
		}
	}

	def _package(): Unit = {
		accept(PACKAGE)
		val pid = qualId()
	}

	def _import(): Unit = {
		accept(IMPORT)
		stableId()
		if(isToken(DOT)){
			token.kind match
				case ID | UNDERSCORE => {
					next()
				}
				case LBRACE => {
					next()
					if(isToken(ID)){
						ident()
						if (isToken(FAT_ARROW)) {
							next()
							acceptOneOf(ID, UNDERSCORE)
						}
						while (isToken(COMMA)) {
							next()
							ident()
							if (isToken(FAT_ARROW)) {
								next()
								acceptOneOf(ID, UNDERSCORE)
							}
						}
					} else if(isToken(UNDERSCORE)){
						next()
					} else {
						reportSyntaxError(token.pos, "expected", ID, UNDERSCORE)
					}
					
					accept(RBRACE)
				}
		}
	}



	def annotType():Unit = {
		simpleType()
		while (token.kind == AT) {
			next()
			simpleType()
		}
	}

	def compoundType(): Unit = {
		annotType()
		while (token.kind == WITH) {
			next()
			annotType()
		}
	}

	def infixType(): Unit = {
		compoundType()
		while (token.kind == ID) {
			next()
			compoundType()
		}
	}

	def simpleType(): Unit = {
		val leftPar = eatUp(LPAREN)
		stableId()
		eatUpCount(RPAREN, leftPar)
	}
	
	def _type():Unit = {
		val leftPar = eatUp(LPAREN)
		simpleType()
		if(isToken(FAT_ARROW)){
			next()
			_type()
		}
		while(isToken(COMMA)){
			next()
			simpleType()
			if (isToken(FAT_ARROW)) {
				next()
				_type()
			}
		}
		eatUpCount(RPAREN, leftPar)
	}

	def paramType(): Unit = {
		if (isToken(FAT_ARROW)) {
			next()
			_type()
		} else {
			_type()
			if (isToken(STAR)) next()
		}
	}

	def classParam(): Unit = {
		modifiers()
		if (isTokenOneOf(VAR, VAL)) next()
		ident()
		accept(COLON)
		paramType()
		if (token.kind == EQ) {
			next()
			expr()
		}
	}
	
	
	
	def typeParam(): Unit = {
		token.kind match {
			case ID => ident()
			case UNDERSCORE => next()
		}
		if (token.kind == LBRACKET) {
			next()
			if (token.kind != RBRACKET) {
				variantTypeParam()
				while (token.kind == COMMA) {
					next()
					variantTypeParam()
				}
			}
			accept(RBRACKET)
		}
		if (token.kind == LOWER_BOUND) {
			next()
			_type()
		}
		if (token.kind == UPPER_BOUND) {
			next()
			_type()
		}
		if (token.kind == COLON) {
			next() //Context bound
			_type()
		}
	}

	def types():Unit = {
		_type()
		while (token.kind == COMMA) {
			next()
			_type()
		}
	}



	def classQualifier(): Unit = {
		accept(LBRACKET)
		ident()
		accept(RBRACKET)
	}
	
	def stableId2(): Unit = {
		if (token.kind == LBRACKET) {
			classQualifier()
		}
		accept(DOT)
		accept(ID)
	}

	def stableIdRest(): Unit = {
		while (token.kind == DOT && isTokenLaOneOf(1, ID)) {
			next()
			ident()
		}
	}
	
	def stableId(): Unit = {
		if(isToken(ID)){
			ident()
			if (token.kind == DOT) {
				if(isTokenLaOneOf(1, THIS, SUPER)){
					skip(2)
					stableId2()
				} 
			}
			stableIdRest()
			
		} else if(isTokenOneOf(THIS, SUPER)){
			next()
			stableId2()
			stableIdRest()
		} else {
			reportSyntaxError(token.pos, "expected", ID, THIS, SUPER)
		}
	}

	def variantTypeParam(): Unit = {
		token.kind match {
			case PLUS | SUB => next()
		}
		typeParam()
	}

	def typeParamClause(): Option[FTree] = {
		accept(LBRACKET)
		variantTypeParam()
		while (token.kind == COMMA) {
			next()
			variantTypeParam()
		}
		accept(RBRACKET)
		None
	}

	def pattern(): Unit = {
		//pattern1()
	}
	def patterns(): Unit = {
		if (token.kind == UNDERSCORE) {
			next()
			accept(STAR)
		}
		else {
			pattern()
			while (token.kind == COMMA) {
				next()
				patterns()
			}
		}
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
			case UNDERSCORE => next()
			case LITERAL => next()
			case ID => {
				stableId()
				if (token.kind == LPAREN) {
					next()
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
			case ID => {
				ident()
				token.kind match {
					case COLON => {
						next()
						_type()
					}
					case AT => {
						next()
						simplePattern()
					}
					case _ => simplePattern()
				}
			}
			case UNDERSCORE /*|BoundVarid*/ => {
				next()
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
				if (token.kind == ELSE) {
					next()
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
				if (token.kind == CATCH) {
					next()
					expr()
				}
				if (token.kind == FINALLY) {
					next()
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
				if (token.kind == LPAREN) {
					next()
					enumerators()
					accept(RPAREN)
				}
				else {
					accept(LBRACE)
					enumerators()
					accept(RBRACE)
				}
				if (token.kind == YIELD) {
					next()
				}
				expr()
			}
			case THROW => expr()
			case RETURN => ???
//	    | ((simpleExpr | simpleExpr1 '_'?) '.')? Id '=' expr
//		| simpleExpr1 argumentExprs '=' expr
//		| postfixExpr ascription?
//		| postfixExpr 'match' '{' caseClauses '}'
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
		while (token.kind == ID || token.kind == UNDERSCORE) {
			next()
			if (token.kind == COLON) {
				next()
				_type()
			}
		}
		if (token.kind == FAT_ARROW) {
			next()
			expr()
		}
		else {
			expr1()
		}
	}
	def argumentExprs():Unit = {
		if (isTokenOneOf(LPAREN, LBRACE)) {
			val left = token
			if (isTokenOneOf(ID, UNDERSCORE)) {
				next()
				if (isToken(COLON)) {
					next()
					_type()
				}
				while (isToken(COMMA)) {
					next()
					if (isToken(COLON)) {
						next()
						_type()
					}
				}
				accept(FAT_ARROW)
				expr()
			} else if(isToken(IMPLICIT)) {
				acceptOneOf(ID, UNDERSCORE)
				accept(FAT_ARROW)
				expr()
			} else {
				expr1()
			}
			accept(if (left.kind == LPAREN) RPAREN else RBRACE)
		}
	}
	
	def classDef(isCase: Boolean): Unit = {

		accept(CLASS)
		val name = ident()

		if (isToken(LBRACKET)) {
			next()
			variantTypeParam()
			while (isToken(COMMA)) {
				next()
				variantTypeParam()
			}
			accept(RBRACKET)
		}
	
		token.kind match {
			case PRIVATE | PROTECTED =>
				next()
				if (token.kind == LBRACKET) {
					accessQualifier()
				}
		}

		if (isToken(LPAREN)) {
			next()
			if(!isToken(RPAREN)){
				next()
				classParam()
				while(isToken(COMMA)){
					next()
					classParam()
				}
			}
			accept(RPAREN)
		}

		if (token.kind == EXTENDS) {
			next()
			if (token.kind == LBRACE) {
				next()
				//earlyDefs()
				accept(RBRACE)
				accept(WITH)
			}
			simpleType()
			argumentExprs()
			
		}
	}

	def objectDef(isCase: Boolean): Unit = {
		val startPos = token.pos
		accept(OBJECT)
		val name = ident()
//		val od = F.at(startPos).makeClassDecl()
//		endPosTable(od) = token.pos
//		od
	}

	def traitDef(): Unit = {
		val startPos = token.pos
		accept(TRAIT)
		val name = ident()

	}

	def accessQualifier(): Unit = {
		accept(LBRACKET)
		token.kind match {
			case ID => ident()
			case THIS => next()
		}
		accept(RBRACKET)
	}

	def modifiers(): Unit = {
		while (token.kind == ABSTRACT || token.kind == FINAL || token.kind == SEALED || token.kind == IMPLICIT || token.kind == LAZY || token.kind == OVERRIDE || token.kind == PRIVATE || token.kind == PROTECTED) {
			//modifier()
			next()
		}
	}


	def tmplDef(): Unit = {
		token.kind match {
			case CASE =>
				next()
				token.kind match {
					case CLASS => classDef(true)
					case OBJECT => objectDef(true)
					case _ => {
						reportSyntaxError(token.pos, "expected", CLASS)
					}
				}
			case CLASS => classDef(false)
			case OBJECT => objectDef(false)
			case TRAIT => traitDef()
			case _ => {
				reportSyntaxError(token.pos, "expected", CLASS)
			}
		}
	}

	def topStatement(): Unit = {
		if(isToken(IMPORT)) _import()
		else {
			modifiers()
			tmplDef()
		}
	}

	def topStatements(): Unit = {

		while (token.kind != EOF) {
			topStatement()
		}
	}

	def compilationUnit(): Unit = {

		while (token.kind == PACKAGE) {
			_package()
		}

		topStatements()
	}
}

