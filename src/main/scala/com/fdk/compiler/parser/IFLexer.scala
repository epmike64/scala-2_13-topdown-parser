package com.fdk.compiler.parser

import com.fdk.compiler.FToken

trait IFLexer {
	/**
	 * Consume the next token.
	 */
	def nextToken(): FToken

	/**
	 * Return current token.
	 */
	def token: FToken

	/**
	 * Return the last character position of the previous token.
	 */
	def prevToken: FToken

	/**
	 * Return token with given lookahead.
	 */
	def token(lookAhead: Int): FToken

	/**
	 * Return the position where a lexical error occurred;
	 */
	def errPos: Int

	/**
	 * Set the position where a lexical error occurred;
	 */
	def errPos(pos: Int): Unit
}
