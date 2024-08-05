package com.fdk.compiler.parser

import com.fdk.compiler.FToken

trait IFLexer {

	def next: FToken
	
	def skip(n: Int): FToken
	/**
	 * Return current token.
	 */
	def token: FToken
	
	/**
	 * Return token with given lookahead.
	 */
	def lookAhead(n: Int): FToken

	/**
	 * Return the last character position of the previous token.
	 */
	def prev: FToken

	/**
	 * Return the position where a lexical error occurred;
	 */
	def errPos: Int

	/**
	 * Set the position where a lexical error occurred;
	 */
	def errPos(pos: Int): Unit
}
