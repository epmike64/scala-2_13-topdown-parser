package com.fdk.compiler.parser

import com.fdk.compiler.parser.LayoutChars.*
import com.fdk.compiler.util.FArrUtil

import scala.collection.mutable.Stack

object UnicodeReader {
	var stateId = -1
	lazy val surrogatesSupported = {
		try {
			Character.isHighSurrogate('a')
			true
		} catch {
			case _ => false
		}
	}
	def apply(buf: Array[Char]): UnicodeReader = new UnicodeReader(buf)
}

class UnicodeReader(var buf: Array[Char]) {

	/** The buffer index of the last converted unicode character
	 */
	var unicodeConversionBp = -1
	/** A character buffer for saved chars.
	 */
	var sbuf = new Array[Char](128)
	var sp = 0
	
	if(buf.length > 0 && Character.isWhitespace(buf(buf.length - 1))) {
		buf(buf.length - 1) = EOI
	} else {
		buf  = buf :+ EOI
	}

	var ch = EOI
	var bp = -1
	val buflen = buf.length - 1

	case class State(stateId: Int, bp: Int, ch: Char, unicodeConversionBp: Int, sbuf: Array[Char], sp: Int)
	val stateStack = Stack[State]()
	
	scanChar()
	
	import UnicodeReader.*
	def pushState(): Int = {
		stateId += 1
		stateStack.push(State(stateId, bp, ch, unicodeConversionBp, sbuf.clone(), sp))
		stateId
	}
	
	def popState(stateId: Int, discard: Boolean): Unit = {
		val state = stateStack.pop()
		assert(state.stateId == stateId, "popState: wrong state id")
		if(!discard) {
			bp = state.bp
			ch = state.ch
			unicodeConversionBp = state.unicodeConversionBp
			sbuf = state.sbuf
			sp = state.sp
		}
	}
	
	def scanChar(): Unit = {
		if(bp < buflen) {
			bp += 1
			ch = buf(bp)
			if (ch == '\\') convertUnicode()
		}
	}

	/** Read next character in comment, skipping over double '\' characters.
	 */
	def scanCommentChar(): Unit = {
		scanChar()
		if (ch == '\\') if (peekChar == '\\' && !isUnicode) skipChar()
		else convertUnicode()
	}

	/** Append a character to sbuf.
	 */
	def putChar(ch: Char, scan: Boolean): Unit = {
		sbuf = FArrUtil.ensureCapacity(sbuf, sp)
		sbuf(sp) = ch
		sp += 1
		if (scan) scanChar()
	}

	def putChar(ch: Char): Unit = {
		putChar(ch, false)
	}

	def putChar(scan: Boolean): Unit = {
		putChar(ch, scan)
	}

	/** Convert unicode escape; bp points to initial '\' character
	 * (Spec 3.3).
	 */
	def convertUnicode(): Unit = {
		if (ch == '\\' && unicodeConversionBp != bp) {
			bp += 1
			ch = buf(bp)
			if (ch == 'u') {
				while({
					bp += 1
					ch = buf(bp)
					ch == 'u'
				}){}
				val limit = bp + 3
				if (limit < buflen) {
					var d = digit(bp, 16)
					var code = d
					while (bp < limit && d >= 0) {
						bp += 1
						ch = buf(bp)
						d = digit(bp, 16)
						code = (code << 4) + d
					}
					if (d >= 0) {
						ch = code.toChar
						unicodeConversionBp = bp
						return
					}
				}
			}
			else {
				bp -= 1
				ch = '\\'
			}
		}
	}

	/** Convert an ASCII digit from its base (8, 10, or 16)
	 * to its value.
	 */
	def digit(pos: Int, base: Int): Int = {
		val c = ch
		if ('0' <= c && c <= '9') return Character.digit(c, base) //a fast common case
		val codePoint = peekSurrogates()
		val result = if (codePoint >= 0) Character.digit(codePoint, base)
		else Character.digit(c, base)
		if (result >= 0 && c > 0x7f) {
			//            log.error(pos + 1, "illegal.nonascii.digit");
			if (codePoint >= 0) scanChar()
			ch = "0123456789abcdef".charAt(result)
		}
		result
	}

	/** Scan surrogate pairs.  If 'ch' is a high surrogate and
	 * the next character is a low surrogate, returns the code point
	 * constructed from these surrogates. Otherwise, returns -1.
	 * This method will not consume any of the characters.
	 */
	def peekSurrogates(): Int = {
		if (surrogatesSupported && Character.isHighSurrogate(ch)) {
			val high = ch
			val prevBP = bp
			scanChar()
			val low = ch
			ch = high
			bp = prevBP
			if (Character.isLowSurrogate(low)) return Character.toCodePoint(high, low)
		}
		-1
	}

	def isUnicode: Boolean = unicodeConversionBp == bp

	def skipChar(): Unit = {
		bp += 1
	}

	def peekChar: Char = buf(bp + 1)

	def name(): String = {
		chars
	}
	def chars = new String(sbuf, 0, sp)
}
