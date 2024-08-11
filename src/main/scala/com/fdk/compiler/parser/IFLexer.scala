package com.fdk.compiler.parser

import com.fdk.compiler.parser.FToken.FTokenKind


trait IFLexer {

	def next: FToken
	
	def skip(n: Int): FToken
	/**
	 * Return current token.
	 */
	def token: FToken
	
	def slide(kind: FTokenKind): FToken

	def lookAhead(n: Int): FToken
	
	def prev: FToken
	
	def errPos: Int
	
	def errPos(pos: Int): Unit
}
