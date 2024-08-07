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
	def accept(kind: FTokenKind): Unit = {
		if(token.kind == kind) next()
		else {
			setErrorEndPos(token.pos)
			reportSyntaxError(token.pos, "expected", kind)
		}
	}

	def acceptOneOf(kinds: FTokenKind*): Unit = {
		if(kinds.contains(token.kind)) next()
		else {
			setErrorEndPos(token.pos)
			reportSyntaxError(token.pos, "expected", kinds: _*)
		}
	}
	

	def isTokenLaOneOf(n: Int, kinds: FTokenKind*): Boolean = {
		kinds.contains(lookAhead(n))
	}

	def eqTokenPrefix(prefix: FTokenKind*): Boolean = {
		for (i <- 0 until prefix.length) {
			if (lookAhead(i) != prefix(i)) return false
		}
		true
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
				case LCURL => {
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
					
					accept(RCURL)
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
	
	def typeParamClause():Unit = {
		accept(LBRACKET)
		typeParam()
		while (token.kind == COMMA) {
			next()
			typeParam()
		}
		accept(RBRACKET)
	}
	
	def typeParam(): Unit = {
		if(isTokenOneOf(ID, UNDERSCORE)){
			next()
		}
		if(isToken(LBRACKET)){
			typeParamClause()
		}
		
		if (isToken(LOWER_BOUND)) {
			next()
			_type()
		}
		if (isToken(UPPER_BOUND)) {
			next()
			_type()
		}
		if (isToken(COLON)) {
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
		while (eqTokenPrefix(DOT, ID)) {
			next()
			accept(ID)
		}
	}
	
	def stableId(): Boolean = {
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
			return false
		}
		true
	}

	def variantTypeParam(): Unit = {
		if(isTokenOneOf(PLUS, SUB)){
			next()
		}
		typeParam()
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
		while (token.kind != RPAREN || token.kind != RCURL) {
			generator()
		}
	}

	def simpleExpr(): Boolean = {
		if(isToken(NEW)){

			return true
		} else if(isToken(LCURL)){

			return true
		}
		false
	}
	def simpleExpr1():Unit = {
		if(literal() || stableId() || isToken(UNDERSCORE)){
			next()
			simpleExpr1Rest()
		} else if(isToken(LPAREN)){
			while({
				next()
				expr()
				isToken(COMMA)
			})()
			accept(RPAREN)
			simpleExpr1Rest()
		} else if(simpleExpr()){
			if(isToken(DOT)){
				next()
				accept(ID)
			} else if(isToken(LBRACKET)){
				types()
				accept(RBRACKET)
			}
			simpleExpr1Rest()
		}
	}

	def simpleExpr1Rest(): Unit = {
		if(isToken(UNDERSCORE)){
			next()
			simpleExpr1Rest2()
		} else if(isTokenOneOf(DOT, LBRACKET)){
			simpleExpr1Rest2()
		} else if(isTokenOneOf(LPAREN, LCURL)){
			argumentExprs()
		}
	}

	def simpleExpr1Rest2(): Unit = {
		if (isToken(DOT)) {
			next()
			ident()
		} else if (isToken(LBRACKET)) {
			next()
			types()
			accept(RBRACKET)
		}
	}
	def expr1(): Unit = {
		token.kind match {
			case IF => {
				accept(LPAREN)
				expr()
				accept(RCURL)
				expr()
				if (token.kind == ELSE) {
					next()
					expr()
				}
			}
			case WHILE => {
				accept(LPAREN)
				expr()
				accept(RCURL)
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
					accept(LCURL)
					enumerators()
					accept(RCURL)
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

	def args():Unit = {
		expr() // postfixExpr (':' | '_' | '*')?
	}

	def bindings(): Boolean = {
		if(isToken(LPAREN)) {
			while ({
				next()
				acceptOneOf(ID, UNDERSCORE)
				if(isToken(COLON)){
					next()
					_type()
				}
				isToken(COMMA)
			}) {}
			return true
		}
		false
	}
	
	def expr(): Boolean = {
		if(bindings()){
			accept(FAT_ARROW)
			expr()
		} else if (isToken(IMPLICIT)) {
			acceptOneOf(ID, UNDERSCORE)
			accept(FAT_ARROW)
			expr()
		} else if(isTokenOneOf(ID, UNDERSCORE) && isTokenLaOneOf(1, FAT_ARROW)){
			next()
			accept(FAT_ARROW)
			expr()
		} else {
			expr1()
		}
	}
	
	def argumentExprs():Unit = {
		if (isTokenOneOf(LPAREN, LCURL)) {
			val left = token
			next()
			args() // blockExpr()
			accept(if (left.kind == LPAREN) RPAREN else RCURL)
		}
	}
	
	def constr():Unit = {
		simpleType()
		argumentExprs()//*
	}
	
	def classParents():Unit = {
		constr()
		while(isToken(WITH)){
			next()
			simpleType()
		}
	}
	def patVarDef():Unit = {
		if(isToken(VAL)){
			//patDef
		}else if(isToken(VAR)){
			//varDef
		} else {
			reportSyntaxError(token.pos, "expected", VAL, VAR)
		}
	}
	def earlyDefs():Unit = {
		accept(LCURL)
		modifiers()
		patVarDef()
		accept(RCURL)
		accept(WITH)
	}

	def funDef(): Unit = {
		accept(DEF)
		ident()
		typeParamClause()
		paramType()
		if (isToken(COLON)) {
			next()
			_type()
		}
		accept(EQ)
		expr()
	}

	def typeDcl():Unit = {

	}

	def defDcl():Unit = {
		//def_
		//dcl
		if(isTokenOneOf(VAL, VAR)){
			patVarDef()
		} else if(isToken(DEF)){
			funDef()
		} else if(isTokenOneOf(CASE, CLASS, OBJECT, TRAIT)){
			tmplDef()
		} else if(isToken(TYPE)){
			typeDcl()
		}
		else {
			reportSyntaxError(token.pos, "expected", VAL, VAR, DEF, CASE, CLASS, OBJECT, TRAIT, TYPE)
		}
	}

	def valDefDcl(): Unit = {
		accept(VAL)
	}

	def defDclV2():Unit = {
		token.kind match
			case VAL | VAR => ???
			case DEF => ???
			case TYPE => ???
			case CASE | CLASS | OBJECT | TRAIT => ???
			case _ => {
				reportSyntaxError(token.pos, "expected", VAL, VAR, DEF, CASE, CLASS, OBJECT, TRAIT, TYPE)
			}
	}

	def templateStat():Unit = {
		if(isToken(IMPORT)){
			_import()
		} else if(modifiers()){
			defDcl()
		} else {
			expr()
		}
	}

	def literal():Boolean = {
		if(isToken(SUB) && isTokenLaOneOf(1, INT_LTR, FLOAT_LTR)){
			skip(2)
			return true
		} else if(isTokenOneOf(INT_LTR, FLOAT_LTR, STR_LTR, CHR_LTR, BOOL_LTR, NULL)){
			next()
			return true
		}
		false
	}
	/*
	State 332
	 */
	def templateStatV2():Unit = {
		if(literal()){
		}
		else if(stableId()){
		}
		else if(bindings()){
		}









		token.kind match {
			case IMPORT => _import()
			case VAL | VAR => valDefDcl()
			case DEF => funDef()
			case TYPE => typeDcl()
			case CASE | CLASS | OBJECT | TRAIT => tmplDef()
			case _ => {
				reportSyntaxError(token.pos, "expected", IMPORT, VAL, VAR, DEF, CASE, CLASS, OBJECT, TRAIT, TYPE)
			}
		}
	}
	/*
	State 196
	 */
	def templateBody(): Unit = {
		accept(LCURL)
		if(isTokenOneOf(ID, THIS)){
			//selfType
			next()
		}
		while (!isToken(RCURL)) {
			next()
			templateStat() //+
		}
		next()
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

//		if (isToken(EXTENDS)) {
//			if(isToken(LCURL)){
//				earlyDefs()
//			}
//			classParents()
//		}
		templateBody()
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

	def modifiers(): Boolean = {
		var isModifier = false
		while (token.kind == ABSTRACT || token.kind == FINAL || token.kind == SEALED || token.kind == IMPLICIT || token.kind == LAZY || token.kind == OVERRIDE || token.kind == PRIVATE || token.kind == PROTECTED) {
			isModifier = true
			next()
		}
		isModifier
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

