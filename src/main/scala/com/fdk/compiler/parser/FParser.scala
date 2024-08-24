package com.fdk.compiler.parser

import com.fdk.compiler.parser.FParser.assrt
import com.fdk.compiler.parser.FToken.FTokenKind
import com.fdk.compiler.parser.FToken.FTokenKind.*
import com.fdk.compiler.tree.{FRefinement, *}

import scala.collection.mutable.ArrayBuffer
import com.fdk.compiler.tree.BFlags.*

object FParser {
	def assrt(cond: Boolean, msg: String = ""): Unit = if (!cond) throw new AssertionError(msg)

	def apply(lexer: IFLexer): FParser = new FParser(lexer)
}


class FParser(lexer: IFLexer) {
	private var token: FToken = lexer.nextToken()

	def next(): Unit = {
		token = lexer.nextToken()
		println(s"Next=[${token}]")
	}

	def pushState(): Unit = {
		lexer.pushState()
	}

	def popState(stateId: Int, discard: Boolean): Unit = {
		lexer.popState(stateId, discard)
	}

	def skip(n: Int): Unit = {
		if (n == 1) next()
		else if (n > 1) token = lexer.skip(n)
		else throw new IllegalArgumentException("n must be positive")
	}

	def slide(kind: FTokenKind): Int = {
		var n = 0
		while (token.kind == kind) {
			n += 1
			next()
		}
		n
	}

	def acceptCount(kind: FTokenKind, count: Int): Unit = {
		for (i <- 0 until count) {
			if (token.kind == kind) next()
			else {
				setErrorEndPos(token.pos)
				reportSyntaxError(token.pos, "expected", kind)
			}
		}
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
			throw new IllegalArgumentException(s"Expected token $kind, but got ${token.kind}")
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
		kinds.contains(lookAhead(n).kind)
	}

	def isTokenPrefix(prefix: FTokenKind*): Boolean = {
		for (i <- 0 until prefix.length) {
			val t = lookAhead(i)
			if (t.kind != prefix(i)) {
				return false
			}
		}
		true
	}

	def isToken(kind: FTokenKind): Boolean = {
		token.kind == kind
	}


	def isTokenOneOf(kinds: FTokenKind*): Boolean = {
		kinds.contains(token.kind)
	}

	def setErrorEndPos(errPos: Int): Unit = {
		//endPosTable.setErrorEndPos(errPos)
	}

	def reportSyntaxError(pos: Int, key: String, kinds: FTokenKind*): Unit = {
		throw new IllegalArgumentException(s"Syntax error at $pos: $key ${kinds.mkString(" or ")}")
	}

	def ident(): FIdent = {
		token.kind match
			case UNDERSCORE => next(); FIdent(BFlags.Idents.UNDERSCORE)
			case THIS => next(); FIdent(BFlags.Idents.THIS)
			case SUPER => next(); FIdent(BFlags.Idents.SUPER)
			case ID => val name = token.name; next(); FIdent(BFlags.Idents.IDENTIFIER, name)
			case _ => throw new IllegalArgumentException(s"Expected [ID,UNDERSCORE,THIS,SUPER] but got unexpected token ${token.kind}")
	}

	def qualId(): FQualId = {
		if (isToken(ID)) {
			val defs = ArrayBuffer[FTree]()
			defs.append(ident())
			while (isToken(DOT)) {
				next()
				defs.append(ident())
			}
			return FQualId(defs.toList)
		}
		null
	}

	def _package(): FPackage = {
		if (isToken(PACKAGE)) {
			next()
			val qs = qualId()
			if (qs == null) {
				reportSyntaxError(token.pos, "expected", ID)
			}
			return FPackage(qs)
		}
		null
	}

	def _import(): FImport = {
		if (isToken(IMPORT)) {
			next()
			val defs = ArrayBuffer[FImportExpr]()
			var ie = importExpr()
			while (ie != null) {
				defs.append(ie)
				if (isToken(COMMA)) {
					next()
					ie = importExpr()
				} else {
					ie = null
				}
			}
			if (defs.isEmpty) {
				reportSyntaxError(token.pos, "expected", ID, UNDERSCORE)
			}
			return FImport(defs.toList)
		}
		null
	}

	def importExpr(): FImportExpr = {
		val sid = stableId()
		if (sid != null) {
			val ie = FImportExpr(sid)
			
			if (isToken(DOT)) {
				next()
				if(isTokenOneOf(UNDERSCORE, ID)){
					ie.selectors = FImportSelector(ident())::Nil
					return ie
				} 
					
				val is = importSelectors()
				assert(is != Nil)
				ie.selectors = is
			}
			return ie
		}
		null
	}

	def importSelectors(): List[FImportSelector] = {
		if (isToken(LCURL)) {
			next()
			if (isToken(UNDERSCORE)) {
				val id = ident()
				accept(RCURL)
				return FImportSelector(id) :: Nil
			}
			var is = importSelector()
			val defs = ArrayBuffer[FImportSelector]()
			while (is != null) {
				defs.append(is)
				if (isToken(COMMA)) {
					next()
					is = importSelector()
				} else {
					is = null
				}
			}
			accept(RCURL)
			if(defs.size > 0) {
				return defs.toList
			}
		}
		Nil
	}

	def importSelector(): FImportSelector = {
		if (isToken(ID)) {
			val is = FImportSelector(ident())
			if (isToken(FAT_ARROW)) {
				next()
				is.alias = ident()
			}
		}
		null
	}

	def annotType(): FTree = {
		val t = simpleType()
		if (t.isInstanceOf[FSimpleType]) {
			val st = t.asInstanceOf[FSimpleType]
			st.ann = annotations()
		}
		t
	}

	def refineStat(): FRefineStat = {
		var t: FTree = dcl()
		if (t != null) {
			return FRefineStat()
		} else if (isToken(TYPE)) {
			next()
			t = typeDef()
			if (t != null) {
				return FRefineStat()
			}
		}
		null
	}

	def refinement(): FRefinement = {
		if (isToken(LCURL)) {
			val defs = ArrayBuffer[FRefineStat]()
			while(!isToken(RCURL)){
				defs.append(refineStat())
			}
			assert(defs.size > 0)
			accept(RCURL)
			return FRefinement(defs.toList)
		}
		null
	}

	def compoundType(): Unit = {
		annotType()
		while (token.kind == WITH) {
			next()
			annotType()
		}
	}

	def infixType(): FTree = {
		compoundType()
		while (token.kind == ID) {
			next()
			compoundType()
		}
		FTree()
	}

	def simpleType(): FSimpleType = {
		if (isToken(LPAREN)) {
			next()
			val tys = types()
			accept(RPAREN)
			return simpleType12(tys)
		}

		val sid = stableId()
		if(sid neq null){
			var dotTy = false
			if (isTokenPrefix(DOT, TYPE)) {
				skip(2)
				dotTy = true
			}
			return simpleType12(FSimpleType(sid, dotTy))
		}
		null
	}

	def simpleType12(ty: FSimpleType): FSimpleType = {
		var loop = true
		while(loop){
			val tas = typeArgs()
			if (tas != null) {
				ty.tyArgs = tas
			} else if (isToken(POUND)) {
				next()
				ty.poundId = ident()
			} else {
				loop = false
			}
		}
		ty
	}

	def _type(): FSimpleType = {
		var t = null
		if ((t = functionArgTypes()) neq null) {
			if (isToken(FAT_ARROW)) {
				next()
				val t = _type()
			}
			return FType()
		}
		null
	}

	def typeArgs(): FSimpleType = {
		if (isToken(LBRACKET)) {
			next()
			val tys = types()
			accept(RBRACKET)
			return tys
		}
		null
	}

	def functionArgTypes(): FTree = {
		var t = null
		if ((t = infixType()) != null) {
			return FFunctionArgTypes()
		}
		if (isToken(LPAREN)) {
			next()
			if (!isToken(RPAREN)) {
				paramType()
				while (isToken(COMMA)) {
					next()
					paramType()
				}
			}
			accept(RPAREN)
			return FFunctionArgTypes()
		}
		null
	}

	def paramType(): FTree = {
		var t = null
		if (isToken(FAT_ARROW)) {
			next()
			t = _type()
		} else {
			val t = _type()
			if (isToken(ID)) ident() //STAR
		}
		FParamType(t)
	}

	def classParam(): FTree = {
		val mods = modifiers()
		if (isTokenOneOf(VAR, VAL) && isTokenLaOneOf(1, ID)) {
			val varVal = token.kind
			next()
			val id = ident()
			accept(COLON)
			val pt = paramType()
			var exp = null
			if (token.kind == EQ) {
				next()
				exp = expr()
			}
			return FClassParam(mods, varVal, id, pt, exp)
		}
		null
	}

	def typeParamClause(): FTree = {
		if (isToken(LBRACKET)) {
			next()
			val defs = ArrayBuffer[FTree]()
			defs.append(variantTypeParam())
			while (token.kind == COMMA) {
				next()
				defs.append(variantTypeParam())
			}
			accept(RBRACKET)
			return FTypeParamClause(defs.toList)
		}
		null
	}

	def typeParam(): FTree = {
		if (isTokenOneOf(ID, UNDERSCORE)) {
			val tp = FTypeParam(ident())
			tp.tpc = typeParamClause()
			if (isToken(LOWER_BOUND)) {
				next()
				tp.lowerB = _type()
			}
			if (isToken(UPPER_BOUND)) {
				next()
				tp.upperB = _type()
			}
			if (isToken(LESS_PERCENT)) {
				next()
				tp.ctxB = _type()
			}
			if (isToken(COLON)) {
				next() //Context bound
				tp.parType = _type()
			}
			return tp
		}
		null
	}

	def types(): FSimpleType = {
		val list = _type()
		var nod = list
		while (token.kind == COMMA) {
			next()
			nod.next = _type()
			nod = nod.next
		}
		list
	}

	def classQualifier(): FIdent = {
		if(isToken(LBRACKET)){
			next()
			val id = ident(); id.bFlag = BFlags.Idents.CLASS_QLFR
			accept(RBRACKET)
			return id
		}
		null
	}

	def stableId32(sid: FStableId): Unit = {
		val cq = classQualifier()
		if(cq != null){
			sid.addId(cq)
		}
		accept(DOT)
		sid.addId(ident())
	}

	def stableId2(sid: FStableId): FStableId = {
		while (isTokenPrefix(DOT, ID)) {
			next()
			sid.addId(ident())
		}
		sid
	}

	def stableId(): FStableId = {
		if (isToken(ID)) {
			val sid = FStableId()
			sid.addId(ident())
			if (token.kind == DOT) {
				if (isTokenLaOneOf(1, THIS, SUPER)) {
					next()
					return stableId31(sid)
				}
				return stableId2(sid)
			}
			return sid
		}

		if (isTokenOneOf(THIS, SUPER)) {
			val sid = FStableId()
			return stableId31(sid)
		}
		null
	}

	def stableId31(sid: FStableId): FStableId = {
		val tk = token.kind
		assert(tk == THIS || tk == SUPER)
		sid.addId(ident())
		if (tk == SUPER) {
			stableId32(sid)
		}
		sid
	}

	def variantTypeParam(): FTree = {
		var plusMinus: FIdent = null
		if (isToken(ID)) { //OneOf(PLUS, SUB))
			plusMinus = ident()
		}
		val tp = typeParam()
		if (tp != null) {
			return FVariantTypeParam(plusMinus, tp)
		}
		null
	}

	def pattern2(): FTree = {
		if (isToken(ID) && isTokenLaOneOf(1, AT)) {
			val id = ident()
			next()
			val p3 = pattern3()
			return FPattern2(id, p3)
		}
		val p3 = pattern3()
		FPattern2(null, p3)
	}

	def pattern1(): FTree = {
		if (isTokenOneOf(UNDERSCORE, ID) && isTokenLaOneOf(1, COLON)) {
			skip(2)
			assrt(_type())
		} else if (pattern2()) {
		} else {
			return null
		}
		FTree()
	}

	def pattern(): FTree = {
		if (pattern1()) {
			while (isToken(ID)) { //PIPE
				next()
				pattern1()
			}
		} else {
			return null
		}
		FTree()
	}

	def patterns(): Unit = {
		if (token.kind == UNDERSCORE) {
			next()
			accept(ID) //STAR
		}
		else {
			pattern()
			while (token.kind == COMMA) {
				next()
				patterns()
			}
		}
	}

	def simplePatternRest(): FTree = {
		if (isToken(LPAREN)) {
			next()
			if (token.kind != RPAREN) {
				patterns()
			}
			accept(RPAREN)
			return FTree()
		}
		null
	}

	def simplePattern(): FTree = {
		if (isToken(UNDERSCORE) || literal()) {
			next()
			return FTree()
		}
		if (stableId()) {
			simplePatternRest()
			return FTree()
		}
		simplePatternRest()
	}

	def guard(): FTree = {
		if (isToken(IF)) {
			next()
			postfixExpr()
			return FTree()
		}
		null
	}

	def generatorRest(): FTree = {
		if (guard()) {
		} else if (pattern1()) {
			accept(EQ)
			expr()
		} else {
			return null
		}
		FTree()
	}

	def generator(): FTree = {
		if (pattern1()) {
			accept(LEFT_ARROW)
			expr()
			while (generatorRest()) {}
		} else {
			return null
		}
		FTree()
	}

	def enumerators(): FTree = {
		if (generator()) {
			while (generator()) {}
		} else {
			return null
		}
		FTree()
	}


	def simpleExpr(): FTree = {
		if (isToken(NEW)) {
			next()
			if (templateBody() || classTemplate()) return FTree()
		} else if (blockExpr()) {
			return FTree()
		}
		null
	}

	def caseClauses(): FTree = {
		if (caseClause()) {
			while (caseClause()) {}
			return FTree()
		}
		null
	}

	def blockExpr(): FTree = {
		if (isToken(LCURL)) {
			next()
			if (caseClauses()) {
			} else {
				assrt(block())
			}
			accept(RCURL)
			return FTree()
		}
		null
	}

	def simpleExpr1(): FTree = {
		if (literal() || stableId()) {
			simpleExpr1Rest()
			return FTree()
		}
		if (isToken(UNDERSCORE)) {
			next()
			simpleExpr1Rest()
			return FTree()
		}
		if (isToken(LPAREN)) {
			next()
			if (!isToken(RPAREN)) {
				exprs()
			}
			accept(RPAREN)
			simpleExpr1Rest()
			return FTree()
		}
		if (simpleExpr()) {
			if (isToken(DOT)) {
				next()
				ident()
				simpleExpr1Rest()
				return FTree()
			}
			assrt(typeArgs())
			simpleExpr1Rest()
			return FTree()
		}
		null
	}

	def simpleExpr1Rest(): FTree = {
		if (isTokenPrefix(UNDERSCORE, DOT)) {
			skip(2)
			ident()
			simpleExpr1Rest()
			return FTree()
		}
		if (isTokenPrefix(UNDERSCORE, LBRACKET)) {
			next()
			typeArgs()
			simpleExpr1Rest()
			return FTree()
		}
		if (isToken(DOT)) {
			next()
			ident()
			simpleExpr1Rest()
			return FTree()
		}
		if (isToken(LBRACKET)) {
			typeArgs()
			simpleExpr1Rest()
			return FTree()
		}
		if (argumentExprs()) {
			simpleExpr1Rest()
			return FTree()
		}
		null
	}

	def simpleExpr1Rest_OLD(): FTree = {
		if (isToken(UNDERSCORE)) {
			next()
			simpleExpr1Rest2()
			return FTree()
		}
		if (simpleExpr1Rest2()) {
			return FTree()
		}
		argumentExprs()
	}

	def simpleExpr1Rest2(): FTree = {
		if (isToken(DOT)) {
			next()
			ident()
			return FTree()
		}
		if (typeArgs()) {
			return FTree()
		}
		null
	}

	def prefixExpr(): FTree = {
		if (isToken(ID)) { //SUB, PLUS, BANG, TILDE
			next()
		}
		if (simpleExpr()) {
			return FTree()
		} else if (simpleExpr1()) {
			if (isToken(UNDERSCORE)) {
				next()
			}
			return FTree()
		}
		null
	}

	def infixExpr(): FTree = {
		if (prefixExpr()) {
			if (isToken(ID)) {
				while ( {
					next()
					prefixExpr()
					isToken(ID)
				}) {}
			}
			return FTree()
		}
		null
	}

	def prefixDef(): FTree = {
		if (isToken(ID)) { //SUB, PLUS, TILDE, BANG
			next()
			return FTree()
		}
		null
	}

	def postfixExpr(): FTree = {
		if (infixExpr()) {
			if (isToken(ID)) {
				ident()
			}
			while (prefixDef()) {
				simpleExpr1()
			}
			return FTree()
		}
		null
	}


	def caseClause(): FTree = {
		if (isToken(CASE)) {
			next()
			pattern()
			guard()
			accept(FAT_ARROW)
			assrt(block())
			return FTree()
		}
		null
	}

	def expr1(): FTree = {
		if (isToken(IF)) {
			accept(LPAREN)
			expr()
			accept(RCURL)
			expr()
			if (token.kind == ELSE) {
				next()
				expr()
			}
			return FTree()
		}

		if (isToken(WHILE)) {
			accept(LPAREN)
			expr()
			accept(RCURL)
			expr()
			return FTree()
		}

		if (isToken(TRY)) {
			expr()
			if (token.kind == CATCH) {
				next()
				expr()
			}
			if (token.kind == FINALLY) {
				next()
				expr()
			}
			return FTree()
		}

		if (isToken(DO)) {
			expr()
			accept(WHILE)
			accept(LPAREN)
			expr()
			accept(RPAREN)
			return FTree()
		}

		if (isToken(FOR)) {
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
			return FTree()
		}

		if (isToken(THROW)) {
			assrt(expr())
			return FTree()
		}

		if (isToken(RETURN)) {
			expr()
			return FTree()
		}

		if (simpleExpr()) {
			ident()
			accept(EQ)
			expr()
			return FTree()
		}

		if (simpleExpr1()) {
			var suffix = false
			if (isToken(UNDERSCORE)) {
				next()
				suffix = true
			}
			if (isToken(DOT)) {
				next()
				suffix = true
			}
			if (isToken(ID)) {
				ident()
				accept(EQ)
				expr()
				return FTree()
			}

			if (!suffix && argumentExprs()) {
				accept(EQ)
				expr()
				return FTree()
			}
			return FTree()
		}

		if (postfixExpr()) {
			if (isToken(MATCH)) {
				next()
				accept(LCURL)
				caseClauses()
				accept(RCURL)
				return FTree()
			}

			if (ascription()) {
				return FTree()
			}
		}
		null
	}

	//	/*
	//		SimpleExpr
	//	 */
	//		if (isTokenOneOf(NEW, LCURL)) {
	//			simpleExpr()
	//		}
	//
	//		if(isTokenPrefix(ID, EQ)){
	//			ident()
	//			accept(EQ)
	//			expr()
	//			return FTree()
	//		}
	//
	//	/*
	//			SimpleExpr1
	//	 */
	//		if (isTokenOneOf(UNDERSCORE, LPAREN)) {
	//			simpleExpr1()
	//		}
	//
	//		/*
	//
	//		 */
	//		if(isTokenOneOf(SUB, PLUS, BANG, TILDE)){
	//			postfixExpr()
	//		}
	//
	//		throw new IllegalArgumentException(s"Unexpected Token ${token.kind}")


	//		if(postfixExpr()){
	//
	//			if (isToken(MATCH)) {
	//				next()
	//				accept(LCURL)
	//				while (token.kind != RCURL) {
	//					next()
	//					caseClause()
	//				}
	//				next()
	//				return FTree()
	//			}
	//
	//			if(ascription()){
	//				return FTree()
	//			}
	//
	//			if (argumentExprs()) {
	//				accept(EQ)
	//				expr()
	//				return FTree()
	//			}
	//
	//			if(isToken(ID)){
	//				ident()
	//				accept(EQ)
	//				expr()
	//				return FTree()
	//			}
	//
	//			if(isToken(EQ)){
	//				next()
	//				expr()
	//				return FTree()
	//			}
	//
	//			return FTree()
	//		}

	def annotations(): FTree = {
		if (isToken(AT)) {
			while ( {
				next()
				simpleType()
				argumentExprs()
				isToken(AT)
			}) {}
			return FTree()
		}
		null
	}

	def ascription(): FTree = {
		if (isToken(COLON)) {
			if (isTokenPrefix(UNDERSCORE, ID)) { // STAR
				skip(2)
				return FTree()
			} else if (annotations() || infixType()) {
				return FTree()
			}
		}
		null
	}

	def args(): FTree = {
		var isExpr = false
		if (exprs()) {
			isExpr = true
			while (isToken(COMMA)) {
				next()
				exprs()
			}
		}

		if (postfixExpr()) {
			if (isTokenOneOf(COLON, UNDERSCORE, ID)) { //STAR
				next()
			}
			return FTree()
		}
		isExpr
	}

	def bindings(): FTree = {
		if (isToken(LPAREN)) {
			while ( {
				next()
				acceptOneOf(ID, UNDERSCORE)
				if (isToken(COLON)) {
					next()
					assrt(_type())
				}
				isToken(COMMA)
			}) {}
			return FTree()
		}
		null
	}

	def exprs(): FTree = {
		assrt(expr())
		while (isToken(COMMA)) {
			next()
			assrt(expr())
		}
		FTree()
	}

	def expr(): FTree = {
		if (bindings()) {
			accept(FAT_ARROW)
			return expr()
		}

		if (isToken(IMPLICIT)) {
			acceptOneOf(ID, UNDERSCORE)
			accept(FAT_ARROW)
			return expr()
		}

		if (isTokenOneOf(ID, UNDERSCORE) && isTokenLaOneOf(1, FAT_ARROW)) {
			next()
			accept(FAT_ARROW)
			return expr()
		}

		expr1()
	}

	def resultExprRest(): FTree = {
		accept(FAT_ARROW)
		block()
		FTree()
	}

	def resultExpr(): FTree = {
		if (bindings()) {
			resultExprRest()
		} else if (isTokenPrefix(IMPLICIT, ID) || isToken(ID) || isToken(UNDERSCORE)) {
			if (isToken(IMPLICIT)) next()
			next()
			accept(COLON)
			compoundType()
			resultExprRest()
		} else if (expr1()) {
		} else {
			return null
		}
		FTree()
	}

	def argumentExprs(): FTree = {
		if (blockExpr()) {
			return FTree()
		}
		if (isTokenOneOf(LPAREN, LCURL)) {
			val t = if (token.kind == LPAREN) RPAREN else RCURL
			next()
			args()
			accept(t)
			return FTree()
		}

		null
	}

	def constr(): Unit = {
		simpleType()
		while (argumentExprs()) {}
	}

	def classParents(): Unit = {
		constr()
		while (isToken(WITH)) {
			next()
			simpleType()
		}
	}

	def pattern3(): FTree = {
		val sp = simplePattern()
		while (isToken(ID)) {
			ident()
			simplePattern()
		}
		FTree()
	}


	def patDef(): FTree = {
		var t = null
		if ((t = pattern2()) != null) {
			while (isToken(COMMA)) {
				next()
				pattern2()
			}
			if (isToken(COLON)) {
				next()
				assrt(_type())
			}
			accept(EQ)
			expr()
		}
		null
	}

	def ids(): List[FIdent] = {
		if (isToken(ID)) {
			val defs = ArrayBuffer[FIdent]()
			defs.append(ident())
			while (isToken(COMMA)) {
				next()
				defs.append(ident())
			}
			return defs.toList
		}
		null
	}

	def varDef(): FTree = {
		if (patDef()) {
		} else if (ids()) {
			accept(COLON)
			assrt(_type())
			accept(EQ)
			accept(UNDERSCORE)
		} else {
			return null
		}
		FTree()
	}

	def patVarDef(): FTree = {
		if (isToken(VAL)) {
			next()
			return patDef()
		} else if (isToken(VAR)) {
			next()
			return varDef()
		}
		null
	}

	def selfInvocation(): FTree = {
		if (isToken(THIS)) {
			next()
			argumentExprs()
			return FTree()
		}
		null
	}

	def constrBlock(): FTree = {
		if (isToken(LCURL)) {
			selfInvocation()
			while (blockStat()) {}
			accept(RCURL)
			return FTree()
		}
		null
	}

	def constrExpr(): FTree = {
		if (selfInvocation()) {
		} else if (constrBlock()) {
		} else {
			return null
		}
		FTree()
	}

	def defDcl(): FTree = {
		if (isToken(VAL)) {
			if (valDcl() || patVarDef()) {
				return FTree()
			}
		} else if (isToken(VAR)) {
			if (varDcl() || patVarDef()) {
				return FTree()
			}
		} else if (isToken(DEF)) {
			if (funDclDef()) {
				return FTree()
			}
		} else if (isToken(TYPE)) {
			if (typeDclDef()) {
				return FTree()
			}
		}
		null
	}

	def typeDcl(): FTree = {
		if (isToken(ID)) {
			val id = ident()
			val tpc = typeParamClause()
			var lowerB = null
			if (isToken(LOWER_BOUND)) {
				next()
				lowerB = _type()
			}
			var upperB = null
			if (isToken(UPPER_BOUND)) {
				next()
				upperB = _type()
			}
			return FTypeDcl(id, tpc, lowerB, upperB)
		}
		null
	}

	def typeDef(): FTree = {
		if (isToken(ID)) {
			val id = ident()
			val tpc = typeParamClause()
			accept(EQ)
			val t = _type()
			return FTypeDef(id, tpc, t)
		}
		null
	}

	def typeDclDef(): FTree = {
		if (isToken(TYPE)) {
			next()
			ident()
			typeParamClause()
			if (isToken(EQ)) {
				next()
				assrt(_type())
				return FTree()
			}
			if (isToken(LOWER_BOUND)) {
				next()
				assrt(_type())
			}
			if (isToken(UPPER_BOUND)) {
				next()
				assrt(_type())
			}
			return FTree()
		}
		null
	}


	def valDcl(): FValDcl = {
		val is = ids()
		if (is != null) {
			accept(EQ)
			val t = _type()
			return FValDcl(is, t)
		}
		null
	}

	def varDcl(): FTree = {
		val is = ids()
		if (is != null) {
			accept(EQ)
			val t = _type()
			return FVarDcl(is, t)
		}
		null
	}

	def funTypeParamClause(): FTree = {
		if (isToken(LBRACKET)) {
			next()
			typeParam()
			while (isToken(COMMA)) {
				next()
				typeParam()
			}
			accept(RBRACKET)
			return FTree()
		}
		null
	}

	def param(): FTree = {
		if (isTokenOneOf(ID)) {
			ident()
			if (isToken(COLON)) {
				next()
				paramType()
			} else if (isToken(EQ)) {
				next()
				expr()
			}
			return FTree()
		}
		null
	}

	def params(): FTree = {
		if (param()) {
			while (isToken(COMMA)) {
				next()
				param()
			}
			return FTree()
		}
		null
	}

	def paramClausesRest(): FTree = {
		if (isToken(LPAREN) && isTokenLaOneOf(1, IMPLICIT)) {
			skip(2)
			params()
			accept(RPAREN)
			return FTree()
		}
		null
	}

	def paramClause(): FTree = {
		if (isToken(LPAREN)) {
			next()
			params()
			accept(RPAREN)
			return FTree()
		}
		null
	}

	def paramClauses(): FTree = {
		if (paramClausesRest()) {
			return FTree()
		}
		else if (paramClause()) {
			paramClausesRest()
			return FTree()
		}
		null
	}

	def funSig(): FTree = {
		if (isToken(ID)) {
			ident()
			funTypeParamClause()
			paramClauses()
			return FTree()
		}
		null
	}


	def funDclDef(): FTree = {
		if (isToken(DEF)) {
			next()
			if (funSig()) {
				if (isToken(COLON)) {
					next()
					assrt(_type())
					if (isToken(EQ)) {
						next()
						expr()
					}
				} else if (isToken(LCURL)) {
					block()
					accept(RCURL)
				} else if (isToken(EQ)) {
					next()
					expr()
				}
				return FTree()
			} else if (isToken(THIS)) {
				next()
				paramClause()
				paramClauses()
				if (isToken(EQ)) {
					next()
					constrExpr()
				} else if (constrBlock()) {
				}
				return FTree()
			}
		}
		null
	}

	def dcl(): FDcl = {
		if (isToken(VAL)) {
			next()
			return valDcl()
		}

		if (isToken(VAR)) {
			next()
			return varDcl()
		}

		if (isToken(DEF)) {
			next()
			return funDcl()
		}

		if (isToken(TYPE)) {
			next()
			return typeDcl()
		}
		null
	}

	def funDcl(): FTree = {
		val f = funSig()
		if (f != null) {
			var t = null
			if (isToken(EQ)) {
				accept()
				t = _type()
			}
			return FFunDcl(f, t)
		}
		null
	}

	def funDef(): FTree = {
		if (isToken(THIS)) {
			next()
			val pc = paramClause()
			val pcs = paramClauses()
			if (isToken(EQ)) {
				val ce = constrExpr()
				return FFunDef(pc, pcs, ce)
			}
			val cb = constrBlock()
			return FFunDef(pc, pcs, cb)
		}

		val fs = funSig()
		if (fs) {
			if (isToken(LCURL)) {
				val b = block()
				accept(RCURL)
				return FFunDef(fs, b)
			}

			var ty = null
			if (isToken(COLON)) {
				next()
				ty = _type()
			}

			accept(EQ)
			val ex = expr()
			return FFunDef(fs, ty, ex)
		}

		null
	}

	def _def(): FTree = {
		var t = null
		if ((t = patVarDef()) != null) {
			return t
		}

		if (isToken(DEF)) {
			next()
			funDef()
		}

		//		if ((t=funDclDef())!=null) {
		//			return t
		//		}

		if ((t = typeDclDef()) != null) {
			return t
		}

		if ((t = tmplDef()) != null) {
			return t
		}
		null
	}

	def block(): FTree = {
		val defs = ArrayBuffer[FTree]()
		var b = blockStat()
		while (b != null) {
			defs.append(b)
			b = blockStat()
		}
		if (defs.size > 0) {
			val re = resultExpr()
			return FBlock(defs.toList, re)
		}
		null
	}

	def modifiers(): FModifiers = {
		val ms = 0
		var m = modifier()
		while (m != 0) {
			ms |= m
			m = modifier()
		}
		if (ms > 0) {
			return FModifiers(ms)
		}
		null
	}

	def modifier(): Int = {
		var m = localModifier()
		if(m == 0){
			m = accessModifier()
		}
		if(m == 0 && isToken(OVERRIDE)){
			next()
			m = BFlags.Modifiers.OVERRIDE
		}
		m
	}

	def localModifier(): Int = {
		val m = token.kind match {
			case ABSTRACT => BFlags.Modifiers.ABSTRACT
			case FINAL => BFlags.Modifiers.FINAL
			case SEALED => BFlags.Modifiers.SEALED
			case IMPLICIT => BFlags.Modifiers.IMPLICIT
			case LAZY => BFlags.Modifiers.LAZY
			case _ => 0
		}
		if(m > 0)	next()
		m
	}

	def accessModifier(): Int = {
		val m = token.kind match {
			case PRIVATE => BFlags.Modifiers.PRIVATE
			case PROTECTED => BFlags.Modifiers.PROTECTED
			case _ => 0
		}
		if(m > 0)	{
			next()
			m |= accessQualifier()
		}
		m
	}

	def blockStat(): FTree = {
		var t = _import()
		if (t != null) {
			return t
		}

		if (localModifier()) {
			return tmplDef()
		}

		if (isTokenOneOf(IMPLICIT, LAZY)) {
			next()
			return _def()
		}

		t = _def()
		if (t != null) {
			return t
		}

		expr1()
	}

	def templateStat(): FTree = {
		var t: FTree = null
		if ((t = _import()) != null) {
			return t
		}
		if ((t = modifiers()) != null) {
			return defDcl()
		}
		if ((t = defDcl()) != null) {
			return t
		}
		if ((t = expr()) != null) {
			return t
		}
		null
	}

	def literal(): FTree = {
		if (isToken(ID) && isTokenLaOneOf(1, INTLITERAL, FLOATLITERAL)) {
			skip(2)
			return FLiteral()
		} else if (isTokenOneOf(INTLITERAL, FLOATLITERAL, STRINGLITERAL, CHARLITERAL, BOOLEANLITERAL, NULL)) {
			next()
			return FLiteral()
		}
		null
	}

	def selfType(): FTree = {
		if (isToken(ID)) {
			if (isToken(COLON)) {
				next()
				assrt(_type())
			}
		} else if (isToken(THIS)) {
			accept(COLON)
			assrt(_type())
		} else {
			return null
		}
		accept(FAT_ARROW)
		FTree()
	}

	def classParamClause(): FTree = {
		if (isToken(LPAREN)) {
			next()
			classParams()
			accept(RPAREN)
			return FClassParamClause()
		}
		null
	}

	def classParams(): FTree = {
		val t = classParam()
		if (t != null) {
			val defs = ArrayBuffer[FTree]()
			defs.append(t)
			while (isToken(COMMA)) {
				next()
				defs.append(classParam())
			}
			return FClassParams(defs.toList)
		}
		null
	}

	def classParamClausesRest(): FTree = {
		if (isTokenPrefix(LPAREN, IMPLICIT)) {
			skip(2)
			classParams()
			return FTree()
		}
		null
	}

	def classParamClauses(): FTree = {
		if (classParamClausesRest()) {
			return FClassParamClauses()
		}
		if (classParamClause()) {
			while (classParamClause()) {}
			classParamClausesRest()
			return FClassParamClauses()
		}
		null
	}



	def templateBody(): FTree = {
		if (isToken(LCURL)) {
			next()
			selfType()
			while (!isToken(RCURL)) {
				templateStat() //+
			}
			accept(RCURL)
			return FTemplateBody()
		}
		null
	}

	def earlyDef(): FTree = {
		val ms = modifiers()
		val pvd = patVarDef()
		if (pvd != null) {
			return FEarlyDef(ms, pvd)
		}
		null
	}

	def earlyDefs(): FTree = {
		if(isToken(LCURL)){
			next()
			val defs = ArrayBuffer[FTree]()
			while(!isToken(RCURL)){
				defs.append(earlyDef())
			}
			accept(RCURL)
			accept(WITH)
			FEarlyDefs(defs.toList)
		}
		null
	}

	def classTemplate(): FTree = {
		val ed = earlyDefs()
		classParents()
		templateBody()
	}


	def classTemplateOpt(): FTree = {
		var ct = null
		if (isToken(EXTENDS)) {
			next()
			ct = classTemplate()
			if(ct == null){
				ct = templateBody()
			}
		} else {
			ct = templateBody()
		}
		ct
	}

	def traitTemplateOpt(): FTree = {
		if (isToken(EXTENDS)) {
			next()
			classParents()
		}
		templateBody()
	}

	def classDef(isCase: Boolean): FTree = {
		if (isToken(CLASS)) {
			next()
			val id = ident()
			val tpc = typeParamClause()
			val am = accessModifier()
			val tpcs = classParamClauses()
			val ct = classTemplateOpt()
			return FClassDef()
		}
		null
	}

	def objectDef(isCase: Boolean): FTree = {
		if (isToken(OBJECT)) {
			next()
			ident()
			classTemplateOpt()
			return FObjectDef()
		}
		null
	}

	def traitDef(): FTraitDef = {
		if (isToken(TRAIT)) {
			next()
			ident()
			typeParamClause()
			traitTemplateOpt()
			return FTraitDef()
		}
		null
	}

	def accessQualifier(): Int = {
		if (isToken(LBRACKET)) {
			next()
			val aq = token.kind match {
				case ID => BFlags.Modifiers.ACCSSQUAL_ID
				case THIS => BFlags.Modifiers.ACCSSQUAL_THIS
				case _ => 0
			}
			if(aq > 0) next()
			accept(RBRACKET)
			return aq
		}
		0
	}

	def tmplDef(): FTopStmt = {

		val mdfs = modifiers()
		var isCase = false
		if (isToken(CASE)) {
			if (!isTokenLaOneOf(1, CLASS, OBJECT)) {
				throw new IllegalArgumentException("Expected CLASS or OBJECT")
			}
			isCase = true
			next()
		}

		var t: FTopStmt = null
		if ((t = traitDef()) neq null) {
			return t
		}

		if ((t = objectDef(isCase)) neq null) {
			return t
		}

		if ((t = classDef(isCase)) neq null) {
			return t
		}

		null
	}

	def topStatement(): FTopStmt = {
		val i = _import()
		if (i != null) {
			return i
		}
		tmplDef()
		//val mdfs = modifiers()
//		val td = tmplDef()
//		FTopStmt(mdfs, td)
	}

	def topStatements(): List[FTopStmt] = {
		val defs = ArrayBuffer[FTopStmt]()
		while (token.kind != EOF) {
			defs.append(topStatement())
		}
		defs.toList
	}

	def compilationUnit(): FCompilationUnit = {
		val pkgs = ArrayBuffer[FPackage]()
		var p = _package()
		while (p != null) {
			pkgs.append(p)
			p = _package()
		}
		val stmts = topStatements()
		FCompilationUnit(pkgs.toList, stmts)
	}
}

