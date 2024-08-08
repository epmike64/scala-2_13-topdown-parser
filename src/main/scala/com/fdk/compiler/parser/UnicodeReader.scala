package com.fdk.compiler.parser

import com.fdk.compiler.util.LayoutCharacters
import com.fdk.compiler.util.ArrayUtils

import java.nio.CharBuffer
import java.util
import java.util.Arrays

object UnicodeReader {
	lazy val surrogatesSupported = {
		try {
			Character.isHighSurrogate('a')
			true
		} catch {
			case ex: NoSuchMethodError =>
				false
		}
	}

	def apply(input: CharBuffer): UnicodeReader = {
		require(input != null, "input must not be null")
		create(input, input.limit)
	}
	
	private def create(input: Array[Char], inputLen: Int): UnicodeReader = {
		if (inputLen == input.length) {
			if (input.length > 0 && Character.isWhitespace(input.last)) {
				return new UnicodeReader(input, inputLen - 1)
			} else {
				return new UnicodeReader(Array.copyOf(input, inputLen + 1), inputLen)
			}
		}
		new UnicodeReader(input, inputLen)
	}
}

class UnicodeReader private (val buf: Array[Char], val bufLen: Int) {
	buf(bufLen) = LayoutCharacters.EOI
	var bp = -1
	var unicodeConversionBp = -1
	var ch: Char = _
	var sbuf = Array[Char](128)
	var sp = 0

	def scanChar(): Unit = {
		if (bp < bufLen) {
			bp += 1
			ch = buf(bp)
			if (ch == '\\') {
				convertUnicode()
			}
		}
	}

	def scanCommentChar(): Unit = {
		scanChar()
		if (ch == '\\') {
			if (peekChar == '\\' && !isUnicode) {
				skipChar()
			}
			else {
				convertUnicode()
			}
		}
	}


	def putChar(ch: Char, scan: Boolean): Unit = {
		sbuf = ArrayUtils.ensureCapacity(sbuf, sp)
		sbuf(sp) = ch
		sp += 1
		if (scan) scanChar()
	}

	def putChar(ch: Char): Unit = putChar(ch, false)

	def putChar(scan: Boolean): Unit = putChar(ch, scan)

	def chars = new String(sbuf, 0, sp)

	def convertUnicode(): Unit = {
		if (ch == '\\' && unicodeConversionBp != bp) {
			bp += 1
			ch = buf(bp)
			if (ch == 'u') {
				while ( {
					bp += 1
					ch = buf(bp)
					ch == 'u'
				}) {}
				val limit = bp + 3
				if (limit < bufLen) {
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
					}
				}
			} else {
				bp -= 1
				ch = '\\'
			}
		}
	}

	def digit(pos: Int, base: Int): Int = {
		val c = ch
		if ('0' <= c && c <= '9') {
			return Character.digit(c, base)
		}
		val codePoint = peekSurrogates()
		val result = if (codePoint >= 0) Character.digit(codePoint, base) else Character.digit(c, base)
		if (result >= 0 && c > 0x7f) {
			if (codePoint >= 0) scanChar()
			ch = "0123456789abcdef".charAt(result)
		}
		result
	}

	def peekSurrogates(): Int = {
		if (UnicodeReader.surrogatesSupported && Character.isHighSurrogate(ch)) {
			val high = ch
			val prevBP = bp
			scanChar()
			val low = ch
			ch = high
			bp = prevBP
			if (Character.isLowSurrogate(low)) {
				return Character.toCodePoint(high, low)
			}
		}
		-1
	}

	def isUnicode: Boolean = unicodeConversionBp == bp

	def skipChar(): Unit = {
		bp += 1
	}

	def peekChar: Char = buf(bp + 1)

	/**
	 * Returns a copy of the input buffer, up to its inputLength.
	 * Unicode escape sequences are not translated.
	 */
	def getRawCharacters: Array[Char] = {
		val chars = new Array[Char](bufLen)
		System.arraycopy(buf, 0, chars, 0, bufLen)
		chars
	}

	/**
	 * Returns a copy of a character array subset of the input buffer.
	 * The returned array begins at the {@code beginIndex} and
	 * extends to the character at index {@code endIndex - 1}.
	 * Thus the length of the substring is {@code endIndex-beginIndex}.
	 * This behavior is like
	 * {@code String.substring(beginIndex, endIndex)}.
	 * Unicode escape sequences are not translated.
	 *
	 * @param beginIndex the beginning index, inclusive.
	 * @param endIndex   the ending index, exclusive.
	 * @throws ArrayIndexOutOfBoundsException if either offset is outside of the
	 *                                        array bounds
	 */
	def getRawCharacters(beginIndex: Int, endIndex: Int): Array[Char] = {
		val length = endIndex - beginIndex
		val chars = new Array[Char](length)
		System.arraycopy(buf, beginIndex, chars, 0, length)
		chars
	}

}
