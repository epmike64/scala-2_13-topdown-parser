package com.fdk.compiler.parser

import com.fdk.compiler.FToken
import scala.collection.mutable.ArrayBuffer

class FScanner(val tokenizer: FTokenizer) extends IFLexer {

	private[this] val _savedTokens = ArrayBuffer[FToken]()
	private[this] var _token: FToken = FToken.DUMMY
	private[this] var _prevToken: FToken = FToken.DUMMY
	
	override def prevToken: FToken = _prevToken
	override def token: FToken = _token
	
	override def nextToken(): Unit = {
		_prevToken = _token
		if (_savedTokens.isEmpty) 
			_token = tokenizer.readToken()
		else {
			_token = _savedTokens.remove(0)
		}
	}
	
	override def token(lookAhead: Int): FToken = {
		if lookAhead == 0 then _token
		else {
			ensureLookAhead(lookAhead)
			_savedTokens(lookAhead - 1)
		}
	}

	private def ensureLookAhead(lookAhead: Int):Unit = {
		for (i <- _savedTokens.size until _savedTokens.size + lookAhead) {
			_savedTokens += tokenizer.readToken()
		}
	}

}
