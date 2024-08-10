package com.fdk.compiler.parser

import com.fdk.compiler.parser.FToken

import java.nio.CharBuffer
import scala.collection.mutable.ArrayBuffer


object FScanner {
	def apply(buf: CharBuffer): FScanner = {
		new FScanner(FTokenizer(UnicodeReader(buf)))
	}
}

class FScanner(val tokenizer: FTokenizer) extends IFLexer {

	var token = FToken.DUMMY
	var prevToken = FToken.DUMMY
	val savedTokens = ArrayBuffer[FToken]()

	override def next: FToken = {
		prevToken = token
		if (!savedTokens.isEmpty) {
			token = savedTokens.remove(0)
		}
		else token = tokenizer.readToken()
	}

	override def slide(kind: FTokenKind): FToken = ???

	override def lookAhead(n: Int): FToken = ???

	override def prev: FToken = ???

	override def errPos: Int = ???

	override def errPos(pos: Int): Unit = ???
}
