package com.fdk.compiler.parser

import com.fdk.compiler.FToken
import com.fdk.compiler.parser.FTokenKind.*
//import com.fdk.compiler.tree.{FExpression, FImport, FModifiers, FPackageDecl, FTree, FTreeMaker}
import com.fdk.compiler.util.FName

import scala.collection.mutable.ArrayBuffer

class FParser(lexer: IFLexer) { //extends IFParser {

	private[this] var token: FToken = lexer.next

	def next(): Unit = {
		token = lexer.next
	}

	def skip(n: Int): Unit = {
		if (n == 1) next()
		else if (n > 1) token = lexer.skip(n)
		else throw new IllegalArgumentException("n must be positive")
	}

	def lookAhead(n: Int): FToken = {
		if (n == 0) token
		else if (n > 0) lexer.lookAhead(n)
		else throw new IllegalArgumentException("n must be positive")
	}

	/** If next input token matches given token, skip it, otherwise report
	 * an error.
	 */
	def accept(kind: FTokenKind): Unit = {
		if (token.kind == kind) next()
		else {
			setErrorEndPos(token.pos)
			reportSyntaxError(token.pos, "expected", kind)
		}
	}

	def acceptOneOf(kinds: FTokenKind*): Unit = {
		if (kinds.contains(token.kind)) next()
		else {
			setErrorEndPos(token.pos)
			reportSyntaxError(token.pos, "expected", kinds: _*)
		}
	}


	def isTokenLaOneOf(n: Int, kinds: FTokenKind*): Boolean = {
		kinds.contains(lookAhead(n))
	}

	def isTokenPrefix(prefix: FTokenKind*): Boolean = {
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
		while (token.kind == kind) {
			n += 1
			next()
		}
		n
	}

	def eatUpCount(kind: FTokenKind, count: Int): Unit = {
		for (i <- 0 until count) {
			if (token.kind == kind) next()
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
		while (token.kind == DOT) {
			next()
			ident()
		}
	}

	def _package(): Unit = {
		accept(PACKAGE)
		val pid = qualId()
	}

	def _import(): Boolean = {
		if (isToken(IMPORT)) {
			next()
			stableId()
			if (isToken(DOT)) {
				token.kind match
					case ID | UNDERSCORE => {
						next()
					}
					case LCURL => {
						next()
						if (isToken(ID)) {
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
						} else if (isToken(UNDERSCORE)) {
							next()
						} else {
							reportSyntaxError(token.pos, "expected", ID, UNDERSCORE)
						}

						accept(RCURL)
					}
			}
			return true
		}
		false
	}


	def annotType(): Unit = {
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

	def _type(): Unit = {
		val leftPar = eatUp(LPAREN)
		simpleType()
		if (isToken(FAT_ARROW)) {
			next()
			_type()
		}
		while (isToken(COMMA)) {
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

	def typeParamClause(): Unit = {
		accept(LBRACKET)
		typeParam()
		while (token.kind == COMMA) {
			next()
			typeParam()
		}
		accept(RBRACKET)
	}

	def typeParam(): Unit = {
		if (isTokenOneOf(ID, UNDERSCORE)) {
			next()
		}
		if (isToken(LBRACKET)) {
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

	def types(): Unit = {
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
		while (isTokenPrefix(DOT, ID)) {
			next()
			accept(ID)
		}
	}

	def stableId(): Boolean = {
		if (isToken(ID)) {
			ident()
			if (token.kind == DOT) {
				if (isTokenLaOneOf(1, THIS, SUPER)) {
					skip(2)
					stableId2()
				}
			}
			stableIdRest()
		} else if (isTokenOneOf(THIS, SUPER)) {
			next()
			stableId2()
			stableIdRest()
		} else {
			return false
		}
		true
	}

	def variantTypeParam(): Unit = {
		if (isTokenOneOf(PLUS, SUB)) {
			next()
		}
		typeParam()
	}

	def pattern2(): Boolean = {
		if (isTokenPrefix(ID, AT)) {
			ident()
			next()
			pattern3()
		} else {
			pattern3()
		}
	}
	
	def pattern1(): Unit = {
		if(isTokenOneOf(UNDERSCORE, ID) && isTokenLaOneOf(1, COLON)){
			skip(2)
			_type()
		} else {
			pattern2()
		}
	}
	
	def pattern(): Unit = {
		pattern1()
		while (isToken(PIPE)) {
			next()
			pattern1()
		}
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

	def simplePatternRest(): Boolean = {
		if (isToken(LPAREN)) {
			next()
			if (token.kind != RPAREN) {
				patterns()
			}
			accept(RPAREN)
			return true
		}
		false
	}
	
	def simplePattern(): Boolean = {
		if(isToken(UNDERSCORE) || literal()){
			return true
		}
		if(stableId()){
			simplePatternRest()
			return true
		}
		simplePatternRest()
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
		if (isToken(NEW)) {
			//(classTemplate | templateBody)
			return true
		} else if (isToken(LCURL)) {
			//blockExpr
			return true
		}
		false
	}

	def simpleExpr1(): Boolean = {
		if (literal() || stableId() || isToken(UNDERSCORE)) {
			next()
			simpleExpr1Rest()
			return true
		} else if (isToken(LPAREN)) {
			while ( {
				next()
				expr()
				isToken(COMMA)
			}) ()
			accept(RPAREN)
			simpleExpr1Rest()
			return true
		} else if (simpleExpr()) {
			if (isToken(DOT)) {
				next()
				accept(ID)
			} else if (isToken(LBRACKET)) {
				types()
				accept(RBRACKET)
			}
			simpleExpr1Rest()
			return true
		}
		false
	}

	def simpleExpr1Rest(): Unit = {
		if (isToken(UNDERSCORE)) {
			next()
			simpleExpr1Rest2()
		} else if (isTokenOneOf(DOT, LBRACKET)) {
			simpleExpr1Rest2()
		} else if (isTokenOneOf(LPAREN, LCURL)) {
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

	def prefixExpr(): Boolean = {
		if (isTokenOneOf(SUB, PLUS, BANG, TILDE)) {
			next()
		}
		if (simpleExpr()) {
			return true
		} else if (simpleExpr1()) {
			if (isToken(UNDERSCORE)) {
				next()
			}
			return true
		}
		false
	}

	def infixExpr(): Boolean = {
		if (prefixExpr()) {
			if (isToken(ID)) {
				while ( {
					next()
					prefixExpr()
					isToken(ID)
				}) {}
			}
			return true
		}
		false
	}

	def postfixExpr(): Boolean = {
		// Grammar is ambiguous here
		if (infixExpr()) {
			return true
		}
		false
	}


	def caseCause(): Boolean = {
		if (isToken(CASE)) {
			next()
			pattern()
			if (isToken(IF)) {
				next()
				postfixExpr()
			}
			accept(FAT_ARROW)
			block()
			return true
		}
		false
	}

	def expr1(): Unit = {
		if (isToken(IF)) {
			accept(LPAREN)
			expr()
			accept(RCURL)
			expr()
			if (token.kind == ELSE) {
				next()
				expr()
			}
		} else if (isToken(WHILE)) {
			accept(LPAREN)
			expr()
			accept(RCURL)
			expr()
		} else if (isToken(TRY)) {
			expr()
			if (token.kind == CATCH) {
				next()
				expr()
			}
			if (token.kind == FINALLY) {
				next()
				expr()
			}
		} else if (isToken(DO)) {
			expr()
			accept(WHILE)
			accept(LPAREN)
			expr()
			accept(RPAREN)
		} else if (isToken(FOR)) {
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
		} else if (isToken(THROW)) {
			expr()
		} else if (isToken(RETURN)) {
			//expr()
		} else if (simpleExpr1()) {
			if (argumentExprs()) {
				accept(EQ)
				expr()
			} else {
				if (isToken(UNDERSCORE)) {
					next()
				}
				if (isToken(DOT)) {
					next()
					ident()
				}
				accept(EQ)
				expr()
			}

		} else if (postfixExpr()) {
			if (isToken(MATCH)) {
				next()
				accept(LCURL)
				while (token.kind != RCURL) {
					next()
					caseClause()
				}
				next()
			}
		}
	}

	def args(): Unit = {
		expr() // postfixExpr (':' | '_' | '*')?
	}

	def bindings(): Boolean = {
		if (isToken(LPAREN)) {
			while ( {
				next()
				acceptOneOf(ID, UNDERSCORE)
				if (isToken(COLON)) {
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
		if (bindings()) {
			accept(FAT_ARROW)
			expr()
		} else if (isToken(IMPLICIT)) {
			acceptOneOf(ID, UNDERSCORE)
			accept(FAT_ARROW)
			expr()
		} else if (isTokenOneOf(ID, UNDERSCORE) && isTokenLaOneOf(1, FAT_ARROW)) {
			next()
			accept(FAT_ARROW)
			expr()
		} else {
			expr1()
		}
	}

	def argumentExprs(): Boolean = {
		if (isTokenOneOf(LPAREN, LCURL)) {
			val left = token
			next()
			args() // blockExpr()
			accept(if (left.kind == LPAREN) RPAREN else RCURL)
			return true
		}
		false
	}

	def constr(): Unit = {
		simpleType()
		argumentExprs() //*
	}

	def classParents(): Unit = {
		constr()
		while (isToken(WITH)) {
			next()
			simpleType()
		}
	}

	def pattern3(): Boolean = {
		simplePattern()
		while (isToken(ID)) {
			ident()
			simplePattern()
		}
		true
	}
	

	
	def patDef(): Boolean = {
		pattern2()
	}
	
	def patVarDef(): Boolean = {
		if (isToken(VAL)) {
			//patDef
		} else if (isToken(VAR)) {
			//varDef
		} else {
			return false
		}
		true
	}

	def earlyDefs(): Unit = {
		accept(LCURL)
		modifiers()
		patVarDef()
		accept(RCURL)
		accept(WITH)
	}

	def funDef(): Boolean = {
		if(isToken(DEF)) {
			next()
			ident()
			typeParamClause()
			paramType()
			if (isToken(COLON)) {
				next()
				_type()
			}
			accept(EQ)
			expr()
			return true
		}
		false
	}
	
	def typeDef(): Boolean = {
		if (isToken(TYPE)) {
			next()
			ident()
			typeParamClause()
			accept(EQ)
			_type()
			return true
		}
		false
	}

	def defDcl(): Boolean = {

		if (patVarDef()) {

		} else if (funDef()) {

		} else if (isTokenOneOf(CASE, CLASS, OBJECT, TRAIT)) {
			tmplDef()
		} else if (isToken(TYPE)) {
			typeDcl()
		} else {
			return false
		}
		true
	}
	
	def valVarDcl():Boolean = {
		if(isTokenOneOf(VAL, VAR)){
			while({
				next()
				ident()
				isToken(COMMA)
			}){}
			accept(COLON)
			_type()
			return true
		}
		false
	}
	
	def typeDcl(): Boolean = {
		if(isToken(TYPE)){
			next()
			ident()
			typeParamClause()
			if(isTokenOneOf(LOWER_BOUND, UPPER_BOUND)) {
				next()
				_type()
			}
			return true
		}
		false
	}
	
	def dcl(): Boolean = {
		if(valVarDcl()){
		} else if(typeDcl()){
		} else {
			return false
		}
		true
	}
	
	def _def(): Boolean = {
		if (patVarDef()) {
		} else if (funDef()) {
		} else if (typeDef()) {
		} else if (tmplDef()) {
		} else {
			return false
		}
		true
	}
	
	def valDefDcl(): Unit = {
		accept(VAL)
	}

	def defDclV2(): Unit = {
		token.kind match
			case VAL | VAR => ???
			case DEF => ???
			case TYPE => ???
			case CASE | CLASS | OBJECT | TRAIT => ???
			case _ => {
				reportSyntaxError(token.pos, "expected", VAL, VAR, DEF, CASE, CLASS, OBJECT, TRAIT, TYPE)
			}
	}

	def resultExpr(): Unit = {
	}

	def block(): Unit = {
		blockStats()
		resultExpr()
	}
	
	def modifiers(): Boolean = {
		var isModifier: Boolean = false
		while(modifier()){
			isModifier = true
			next()
		}
		isModifier
	}
	
	def modifier(): Boolean = {
		if (localModifier()  || accessModifier()) {
			return true
		}
		if (isToken(OVERRIDE)) {
			next()
			return true
		}
		false
	}

	def localModifier(): Boolean = {
		if (isTokenOneOf(ABSTRACT, FINAL, SEALED, IMPLICIT, LAZY)) {
			next()
			return true
		}
		false
	}
	
	def accessModifier(): Boolean = {
		if (isTokenOneOf(PRIVATE, PROTECTED)) {
			next()
			if (isToken(LBRACKET)) {
				accessQualifier()
			}
			return true
		}
		false
	}
	
	def blockStats(): Unit = {
		if(_import()){
		} else if(localModifier()){
			tmplDef()
		} else if(isTokenOneOf(IMPLICIT, LAZY)){
			_def()
		} else {
			expr1()
		}
	}
	
	def templateStat(): Unit = {
		if (_import()) {
		} else if (modifiers()) {
			defDcl()
		}
		if (defDcl()) {
			
		} else {
			expr()
		}
	}

	def literal(): Boolean = {
		if (isToken(SUB) && isTokenLaOneOf(1, INT_LTR, FLOAT_LTR)) {
			skip(2)
			return true
		} else if (isTokenOneOf(INT_LTR, FLOAT_LTR, STR_LTR, CHR_LTR, BOOL_LTR, NULL)) {
			next()
			return true
		}
		false
	}

	/*
	State 332
	 */
	def templateStatV2(): Unit = {
		if (literal()) {
		}
		else if (stableId()) {
		}
		else if (bindings()) {
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
		if (isTokenOneOf(ID, THIS)) {
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
			if (!isToken(RPAREN)) {
				next()
				classParam()
				while (isToken(COMMA)) {
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

	def tmplDef(): Boolean = {
		var isCase = false
		if(isToken(CASE)){
			isCase = true
			next()
		}
		if(isToken(TRAIT)){
			if(isCase){
				reportSyntaxError(token.pos, "expected", CLASS, OBJECT)
			}
			traitDef()
		} else if(isToken(OBJECT)){
			objectDef(isCase)
		} else if(isToken(CLASS)){
			classDef(isCase)
		} else {
			return false
		}
		true
	}

	def topStatement(): Unit = {
		if (isToken(IMPORT)) _import()
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

