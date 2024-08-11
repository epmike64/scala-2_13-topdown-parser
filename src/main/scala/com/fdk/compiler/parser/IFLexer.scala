package com.fdk.compiler.parser

import com.fdk.compiler.parser.FToken.FTokenKind

trait IFLexer {

	def next(): FToken
	
	def skip(n: Int): FToken

	def slide(kind: FTokenKind): FToken

	def lookAhead(n: Int): FToken
}
