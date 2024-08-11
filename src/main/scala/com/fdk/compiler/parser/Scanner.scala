package com.fdk.compiler.parser

import com.fdk.compiler.parser.FToken.FTokenKind


class Scanner(val tokenizer: FTokenizer) extends IFLexer {

	var token: FToken = null

	override def next(): FToken = {
		token = tokenizer.readToken()
		token
	}

	override def skip(n: Int): FToken = {
		for(_ <- 0 until n) {
			token = tokenizer.readToken()
		}
		token
	}

	override def slide(kind: FTokenKind): FToken = {
		while(token.kind == kind) {
			token = tokenizer.readToken()
		}
		token
	}

	override def lookAhead(n: Int): FToken = {
		var t = token
		for(_ <- 0 until n) {
			t = tokenizer.readToken()
		}
		t
	}
}
