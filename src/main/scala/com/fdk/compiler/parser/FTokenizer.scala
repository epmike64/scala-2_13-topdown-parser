package com.fdk.compiler.parser

import com.fdk.compiler.parser.FTokenizer.hexFloatsWork
import com.fdk.compiler.parser.LayoutChars.{CR, EOI, LF}
import com.fdk.compiler.parser.FToken.FTokenKind
import com.fdk.compiler.parser.FToken.FTokenKind.{DO, ID}
import com.sun.tools.javac.util.Assert

object FTokenizer {

	lazy val hexFloatsWork = try {
		java.lang.Float.valueOf("0x1.0p1")
		true
	} catch {
		case ex: NumberFormatException =>
			false
	}

	def apply(filePath: String): FTokenizer = {
		val source = io.Source.fromFile(filePath)
		val text = try source.mkString finally source.close()
		new FTokenizer(UnicodeReader(text.toCharArray))
	}
}

class FTokenizer private(val reader: UnicodeReader) {

	/** Allow binary literals.
	 */
	private var allowBinaryLiterals = false

	/** Allow underscores in literals.
	 */
	private var allowUnderscoresInLiterals = false

	var radix = -1
	var tk: FTokenKind = null
	var errPos = -1
	var name: String = null

	def pushState(): Int = {
		reader.pushState()
	}

	def popState(stateId: Int, discard: Boolean): Unit = {
		reader.popState(stateId, discard)
	}

	def readToken(): FToken = {
		reader.sp = 0
		name = null
		radix = 0
		var pos = 0
		var endPos = 0
		var isLoop = true

		while (isLoop) {
			pos = reader.bp
			reader.ch match {
				case EOI =>
					tk = FTokenKind.EOF
					isLoop = false
					
				case ' ' | '\t' | '\n' | '\r' | '\f' =>
					while ( {
						reader.scanChar()
						reader.ch == ' ' || reader.ch == '\t' || reader.ch == '\n' || reader.ch == '\r' || reader.ch == '\f'
					}) {}

				case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'
					  | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'
					  | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n'
					  | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | '$' =>
					scanIdent()
					isLoop = false

				case '0' =>
					reader.scanChar()
					if (reader.ch == 'x' || reader.ch == 'X') {
						reader.scanChar()
						skipIllegalUnderscores()
						scanNumber(pos, 16)
					}
					else if (reader.ch == 'b' || reader.ch == 'B') {
						if (!allowBinaryLiterals) {
							lexError(pos, "unsupported.binary.lit")
							allowBinaryLiterals = true
						}
						reader.scanChar()
						skipIllegalUnderscores()
						scanNumber(pos, 2)
					}
					else {
						reader.putChar('0')
						if (reader.ch == '_') {
							val savePos = reader.bp
							while ( {
								reader.scanChar()
								reader.ch == '_'
							}) {}

							if (reader.digit(pos, 10) < 0) lexError(savePos, "illegal.underscore")
						}
						scanNumber(pos, 8)
					}
					isLoop = false // todo: label break is not supported

				case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
					scanNumber(pos, 10)
					isLoop = false // todo: label break is not supported

				case '.' =>
					reader.scanChar()
					if (reader.digit(pos, 10) >= 0) {
						reader.putChar('.')
						scanFractionAndSuffix(pos)
					}
					else {
						tk = FTokenKind.DOT
					}
					isLoop = false // todo: label break is not supported

				case ',' =>
					reader.scanChar()
					tk = FTokenKind.COMMA
					isLoop = false // todo: label break is not supported

				case ';' =>
					reader.scanChar()
					tk = FTokenKind.SEMI
					isLoop = false // todo: label break is not supported

				case '(' =>
					reader.scanChar()
					tk = FTokenKind.LPAREN
					isLoop = false // todo: label break is not supported

				case ')' =>
					reader.scanChar()
					tk = FTokenKind.RPAREN
					isLoop = false // todo: label break is not supported

				case '[' =>
					reader.scanChar()
					tk = FTokenKind.LBRACKET
					isLoop = false // todo: label break is not supported

				case ']' =>
					reader.scanChar()
					tk = FTokenKind.RBRACKET
					isLoop = false // todo: label break is not supported

				case '{' =>
					reader.scanChar()
					tk = FTokenKind.LCURL
					isLoop = false // todo: label break is not supported

				case '}' =>
					reader.scanChar()
					tk = FTokenKind.RCURL
					isLoop = false // todo: label break is not supported

				case '_' =>
					putChar(true)
					if (!isSpec) {
						tk = FTokenKind.UNDERSCORE
					} else {
						scanIdent()
					}
					isLoop = false

				case ':' =>
					putChar(true)
					if (!isSpec) {
						tk = FTokenKind.COLON
					} else {
						scanOpIdent()
					}
					isLoop = false

				case '#' =>
					putChar(true)
					if (!isSpec) {
						tk = FTokenKind.POUND
					} else {
						scanOpIdent()
					}
					isLoop = false

				case '@' =>
					putChar(true)
					if (!isSpec) {
						tk = FTokenKind.AT
					} else {
						scanOpIdent()
					}
					isLoop = false

				case '=' =>
					putChar(true)
					if (reader.ch == '>') {
						reader.scanChar()
						tk = FTokenKind.FAT_ARROW
					} else {
						scanOpIdent()
					}
					isLoop = false

				case '<' =>
					putChar(true)
					if (reader.ch == '-') {
						reader.scanChar()
						tk = FTokenKind.LEFT_ARROW
					}
					else if (reader.ch == ':') {
						reader.scanChar()
						tk = FTokenKind.UPPER_BOUND
					}
					else if (reader.ch == '%') {
						reader.scanChar()
						tk = FTokenKind.LESS_PERCENT
					}
					else {
						scanOpIdent()
					}
					isLoop = false

				case '/' =>

					if (reader.peekChar == '/') {
						scanChar()
						while ( {
							scanChar()
							reader.ch != '\n' && reader.ch != '\r' && reader.ch != EOI
						}) {}

					} else if (reader.peekChar == '*') {
						scanChar()
						var wl = true
						while(wl){
							scanChar()
							if (reader.ch == '*' && reader.peekChar == '/') {
								scanChar(2)
								wl = false
							} else if(reader.ch == EOI){
								lexError(pos, "unclosed.comment")
								wl = false
							}
						}
					}
					else {
						putChar(true)
						scanOpIdent()
						isLoop = false
					}

				case '\'' =>
					scanChar()
					if (reader.ch == '\'') {
						lexError(pos, "empty.char.lit")
						reader.scanChar()
					}
					else {
						if (reader.ch == CR || reader.ch == LF) lexError(pos, "illegal.line.end.in.char.lit")
						scanLitChar(pos)
						if (reader.ch == '\'') {
							reader.scanChar()
							tk = FTokenKind.CHARLITERAL
						}
						else lexError(pos, "unclosed.char.lit")
					}
					isLoop = false // todo: label break is not supported

				case '\"' =>
					scanChar()
					while (reader.ch != '\"' && reader.ch != CR && reader.ch != LF && reader.bp < reader.buflen) scanLitChar(pos)
					if (reader.ch == '\"') {
						tk = FTokenKind.STRINGLITERAL
						reader.scanChar()
					}
					else lexError(pos, "unclosed.str.lit")
					isLoop = false // todo: label break is not supported

				case _ =>
					if(isSpec){
						putChar(true)
						scanOpIdent()
						isLoop = false
					} else {
						throw new IllegalArgumentException(s"Unexpected character: ${reader.ch}")
//						lexError(pos, "illegal.char")
//						isLoop = false
					}
			}
		}

		endPos = reader.bp
		import FToken.FTokenTag.*

		tk.tag match {
			case DEFAULT =>
				new FToken(tk, pos, endPos)
			case NAMED =>
				new NamedToken(tk, pos, endPos, name)
			case STRING =>
				new StringToken(tk, pos, endPos, reader.chars)
			case NUMERIC =>
				new NumericToken(tk, pos, endPos, reader.chars, radix)
			case _ =>
				throw new AssertionError
		}
	}

	def scanChar(times: Int = 1): Unit = {
		for(_ <- 0 until times)
			reader.scanChar()
	}
	
	def putChar(b: Boolean): Unit = {
		reader.putChar(b)
	}

	def scanOpIdent(): Unit = {
		while(isSpec){
			reader.putChar(true)
		}
		name = reader.name()
		tk = FToken.lookupKind(name)
	}

	def isSpec: Boolean = {
		isSpecial(reader.ch)
	}

	def isSpecial(ch: Char): Boolean = {
		// No underscore
		ch match {
			case '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~' =>
				true
			case _ =>
				false
		}
	}

	private def scanIdent(): Unit = {

		var isLoop = true
		var specAllow = false

		while (isLoop) {
			val prev = reader.ch
			reader.putChar(true)

			reader.ch match {
				case '_' =>
					specAllow = true

				case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L'
					  | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X'
					  | 'Y' | 'Z' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k'
					  | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w'
					  | 'x' | 'y' | 'z' | '$'
					  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
					if (prev == '_') {
						specAllow = false
					} else if (specAllow) {
						isLoop = false
					}

				case _ =>
					if (!specAllow || !isSpec) {
						isLoop = false
					}
			}
		}
		name = reader.name()
		tk = FToken.lookupKind(name)
	}

	private def scanIdent_OLD(): Unit = {
		var isJavaIdentifierPart = false
		val high = 0
		reader.putChar(true)
		var isCont = false
		while (true) {
			reader.ch match {
				case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L'
					  | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X'
					  | 'Y' | 'Z' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k'
					  | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w'
					  | 'x' | 'y' | 'z' | '$' | '_'
					  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>

				case '\u0000' | '\u0001' | '\u0002' | '\u0003' | '\u0004'
					  | '\u0005' | '\u0006' | '\u0007' | '\u0008' | '\u000E'
					  | '\u000F' | '\u0010' | '\u0011' | '\u0012' | '\u0013'
					  | '\u0014' | '\u0015' | '\u0016' | '\u0017' | '\u0018'
					  | '\u0019' | '\u001B' | '\u007F' =>
					reader.scanChar()
					isCont = true

				case '\u001A' => // EOI is also a legal identifier part

					if (reader.bp >= reader.buflen) {
						name = reader.name()
						tk = FToken.lookupKind(name)
						return
					}
					reader.scanChar()
					isCont = true

				case _ =>
					if (reader.ch < '\u0080') {
						// all ASCII range chars already handled, above
						isJavaIdentifierPart = false
					}
					else if (Character.isIdentifierIgnorable(reader.ch)) {
						reader.scanChar()
						isCont = true
					}
					else {
						val codePoint = reader.peekSurrogates()
						if (codePoint >= 0) {
							isJavaIdentifierPart = Character.isJavaIdentifierPart(codePoint)
							if (isJavaIdentifierPart) {
								reader.putChar(true)
							}
						}
						else isJavaIdentifierPart = Character.isJavaIdentifierPart(reader.ch)
					}
					if (!isJavaIdentifierPart) {
						name = reader.name()
						tk = FToken.lookupKind(name)
						return
					}
			}
			if (isCont) {
				isCont = false
			} else {
				reader.putChar(true)
			}
		}
	}

	/** Report an error at the given position using the provided arguments.
	 */
	protected def lexError(pos: Int, key: String, args: AnyRef*): Unit = {
		tk = FTokenKind.ERROR
		errPos = pos
	}

	/** Read next character in character or string literal and copy into sbuf.
	 */
	private def scanLitChar(pos: Int): Unit = {
		if (reader.ch == '\\') if (reader.peekChar == '\\' && !reader.isUnicode) {
			reader.skipChar()
			reader.putChar('\\', true)
		}
		else {
			reader.scanChar()
			reader.ch match {
				case '0' =>
				case '1' =>
				case '2' =>
				case '3' =>
				case '4' =>
				case '5' =>
				case '6' =>
				case '7' =>
					val leadch = reader.ch
					var oct = reader.digit(pos, 8)
					reader.scanChar()
					if ('0' <= reader.ch && reader.ch <= '7') {
						oct = oct * 8 + reader.digit(pos, 8)
						reader.scanChar()
						if (leadch <= '3' && '0' <= reader.ch && reader.ch <= '7') {
							oct = oct * 8 + reader.digit(pos, 8)
							reader.scanChar()
						}
					}
					reader.putChar(oct.toChar)

				case 'b' =>
					reader.putChar('\b', true)

				case 't' =>
					reader.putChar('\t', true)

				case 'n' =>
					reader.putChar('\n', true)

				case 'f' =>
					reader.putChar('\f', true)

				case 'r' =>
					reader.putChar('\r', true)

				case '\'' =>
					reader.putChar('\'', true)

				case '\"' =>
					reader.putChar('\"', true)

				case '\\' =>
					reader.putChar('\\', true)

				case _ =>
					lexError(reader.bp, "illegal.esc.char")
			}
		}
		else if (reader.bp != reader.buflen) reader.putChar(true)
	}

	private def scanDigits(pos: Int, digitRadix: Int): Unit = {
		var saveCh = 0
		var savePos = 0
		while ( {
			if (reader.ch != '_') reader.putChar(false)
			else if (!allowUnderscoresInLiterals) {
				lexError(pos, "unsupported.underscore.lit")
				allowUnderscoresInLiterals = true
			}
			saveCh = reader.ch
			savePos = reader.bp
			reader.scanChar()
			reader.digit(pos, digitRadix) >= 0 || reader.ch == '_'
		}) {}

		if (saveCh == '_') lexError(savePos, "illegal.underscore")
	}

	/** Read fractional part of hexadecimal floating point number.
	 */
	private def scanHexExponentAndSuffix(pos: Int): Unit = {
		if (reader.ch == 'p' || reader.ch == 'P') {
			reader.putChar(true)
			skipIllegalUnderscores()
			if (reader.ch == '+' || reader.ch == '-') reader.putChar(true)
			skipIllegalUnderscores()
			if (reader.digit(pos, 10) >= 0) {
				scanDigits(pos, 10)
				if (!hexFloatsWork) lexError(pos, "unsupported.cross.fp.lit")
			}
			else lexError(pos, "malformed.fp.lit")
		}
		else lexError(pos, "malformed.fp.lit")
		if (reader.ch == 'f' || reader.ch == 'F') {
			reader.putChar(true)
			tk = FTokenKind.FLOATLITERAL
			radix = 16
		}
		else {
			if (reader.ch == 'd' || reader.ch == 'D') reader.putChar(true)
			tk = FTokenKind.DOUBLELITERAL
			radix = 16
		}
	}

	/** Read fractional part of floating point number.
	 */
	private def scanFraction(pos: Int): Unit = {
		skipIllegalUnderscores()
		if (reader.digit(pos, 10) >= 0) scanDigits(pos, 10)
		val sp1 = reader.sp
		if (reader.ch == 'e' || reader.ch == 'E') {
			reader.putChar(true)
			skipIllegalUnderscores()
			if (reader.ch == '+' || reader.ch == '-') reader.putChar(true)
			skipIllegalUnderscores()
			if (reader.digit(pos, 10) >= 0) {
				scanDigits(pos, 10)
				return
			}
			lexError(pos, "malformed.fp.lit")
			reader.sp = sp1
		}
	}

	/** Read fractional part and 'd' or 'f' suffix of floating point number.
	 */
	private def scanFractionAndSuffix(pos: Int): Unit = {
		radix = 10
		scanFraction(pos)
		if (reader.ch == 'f' || reader.ch == 'F') {
			reader.putChar(true)
			tk = FTokenKind.FLOATLITERAL
		}
		else {
			if (reader.ch == 'd' || reader.ch == 'D') reader.putChar(true)
			tk = FTokenKind.DOUBLELITERAL
		}
	}

	/** Read fractional part and 'd' or 'f' suffix of floating point number.
	 */
	private def scanHexFractionAndSuffix(pos: Int, seenDigit: Boolean): Unit = {
		radix = 16
		Assert.check(reader.ch == '.')
		reader.putChar(true)
		skipIllegalUnderscores()
		var seendigit: Boolean = seenDigit
		if (reader.digit(pos, 16) >= 0) {
			seendigit = true
			scanDigits(pos, 16)
		}
		if (!seendigit) lexError(pos, "invalid.hex.number")
		else scanHexExponentAndSuffix(pos)
	}

	private def skipIllegalUnderscores(): Unit = {
		if (reader.ch == '_') {
			lexError(reader.bp, "illegal.underscore")
			while (reader.ch == '_') reader.scanChar()
		}
	}

	/** Read a number.
	 *
	 * @param radix The radix of the number; one of 2, 8, 10, 16.
	 */
	private def scanNumber(pos: Int, radix: Int): Unit = {
		// for octal, allow base-10 digit in case it's a float literal
		this.radix = radix
		val digitRadix = if (radix == 8) 10
		else radix
		val firstDigit = reader.digit(pos, Math.max(10, digitRadix))
		val seendigit = firstDigit >= 0
		val seenValidDigit = firstDigit >= 0 && firstDigit < digitRadix
		if (seendigit) scanDigits(pos, digitRadix)
		if (radix == 16 && reader.ch == '.') scanHexFractionAndSuffix(pos, seendigit)
		else if (seendigit && radix == 16 && (reader.ch == 'p' || reader.ch == 'P')) scanHexExponentAndSuffix(pos)
		else if (digitRadix == 10 && reader.ch == '.') {
			reader.putChar(true)
			scanFractionAndSuffix(pos)
		}
		else if (digitRadix == 10 && (reader.ch == 'e' || reader.ch == 'E' || reader.ch == 'f' || reader.ch == 'F' || reader.ch == 'd' || reader.ch == 'D')) scanFractionAndSuffix(pos)
		else {
			if (!seenValidDigit) radix match {
				case 2 =>
					lexError(pos, "invalid.binary.number")

				case 16 =>
					lexError(pos, "invalid.hex.number")

				case _ => // do nothing
			}
			if (reader.ch == 'l' || reader.ch == 'L') {
				reader.scanChar()
				tk = FTokenKind.LONGLITERAL
			}
			else tk = FTokenKind.INTLITERAL
		}
	}

	//	def scanSpecial():Unit = {
	//		var loop = true
	//		while(loop) {
	//			reader.putChar(false)
	//			val newname = reader.name()
	//			val newtk = FToken.lookupKind(newname)
	//		}
	//	}

}
