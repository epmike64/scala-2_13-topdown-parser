package com.fdk.compiler.util

object Convert {
	/** Convert string to integer.
	 */
	@throws[NumberFormatException]
	def string2int(s: String, radix: Int): Int = if (radix == 10) Integer.parseInt(s, radix)
	else {
		val cs = s.toCharArray
		val limit = Integer.MAX_VALUE / (radix / 2)
		var n = 0
		for (c <- cs) {
			val d = Character.digit(c, radix)
			if (n < 0 || n > limit || n * radix > Integer.MAX_VALUE - d) throw new NumberFormatException
			n = n * radix + d
		}
		n
	}

	/** Convert string to long integer.
	 */
	@throws[NumberFormatException]
	def string2long(s: String, radix: Int): Long = if (radix == 10) s.toLong
	else {
		val cs = s.toCharArray
		val limit = Long.MaxValue / (radix / 2)
		var n = 0
		for (c <- cs) {
			val d = Character.digit(c, radix)
			if (n < 0 || n > limit || n * radix > Long.MaxValue - d) throw new NumberFormatException
			n = n * radix + d
		}
		n
	}

	/** Convert `len' bytes from utf8 to characters.
	 * Parameters are as in System.arraycopy
	 * Return first index in `dst' past the last copied char.
	 * @param src    The array holding the bytes to convert.
	 * @param sindex The start index from which bytes are converted.
	 * @param dst    The array holding the converted characters..
	 * @param dindex The start index from which converted characters
	 *               are written.
	 * @param len    The maximum number of bytes to convert.
	 */
	/* Conversion routines between names, strings, and byte arrays in Utf8 format
	 */
	def utf2chars(src: Array[Byte], sindex: Int, dst: Array[Char], dindex: Int, len: Int): Int = {
		var i = sindex
		var j = dindex
		val limit = sindex + len
		while (i < limit) {
			var b = src({
				i += 1;
				i - 1
			}) & 0xFF
			if (b >= 0xE0) {
				b = (b & 0x0F) << 12
				b = b | (src({
					i += 1;
					i - 1
				}) & 0x3F) << 6
				b = b | (src({
					i += 1;
					i - 1
				}) & 0x3F)
			}
			else if (b >= 0xC0) {
				b = (b & 0x1F) << 6
				b = b | (src({
					i += 1;
					i - 1
				}) & 0x3F)
			}
			dst({
				j += 1;
				j - 1
			}) = b.toChar
		}
		j
	}

	/** Return bytes in Utf8 representation as an array of characters.
	 *
	 * @param src    The array holding the bytes.
	 * @param sindex The start index from which bytes are converted.
	 * @param len    The maximum number of bytes to convert.
	 */
	def utf2chars(src: Array[Byte], sindex: Int, len: Int): Array[Char] = {
		val dst = new Array[Char](len)
		val len1 = utf2chars(src, sindex, dst, 0, len)
		val result = new Array[Char](len1)
		System.arraycopy(dst, 0, result, 0, len1)
		result
	}

	/** Return all bytes of a given array in Utf8 representation
	 * as an array of characters.
	 *
	 * @param src The array holding the bytes.
	 */
	def utf2chars(src: Array[Byte]): Array[Char] = utf2chars(src, 0, src.length)

	/** Return bytes in Utf8 representation as a string.
	 *
	 * @param src    The array holding the bytes.
	 * @param sindex The start index from which bytes are converted.
	 * @param len    The maximum number of bytes to convert.
	 */
	def utf2string(src: Array[Byte], sindex: Int, len: Int): String = {
		val dst = new Array[Char](len)
		val len1 = utf2chars(src, sindex, dst, 0, len)
		new String(dst, 0, len1)
	}

	/** Return all bytes of a given array in Utf8 representation
	 * as a string.
	 *
	 * @param src The array holding the bytes.
	 */
	def utf2string(src: Array[Byte]): String = utf2string(src, 0, src.length)

	/** Copy characters in source array to bytes in target array,
	 * converting them to Utf8 representation.
	 * The target array must be large enough to hold the result.
	 * returns first index in `dst' past the last copied byte.
	 * @param src    The array holding the characters to convert.
	 * @param sindex The start index from which characters are converted.
	 * @param dst    The array holding the converted characters..
	 * @param dindex The start index from which converted bytes
	 *               are written.
	 * @param len    The maximum number of characters to convert.
	 */
	def chars2utf(src: Array[Char], sindex: Int, dst: Array[Byte], dindex: Int, len: Int): Int = {
		var j = dindex
		val limit = sindex + len
		for (i <- sindex until limit) {
			val ch = src(i)
			if (1 <= ch && ch <= 0x7F) dst({
				j += 1;
				j - 1
			}) = ch.toByte
			else if (ch <= 0x7FF) {
				dst({
					j += 1;
					j - 1
				}) = (0xC0 | (ch >> 6)).toByte
				dst({
					j += 1;
					j - 1
				}) = (0x80 | (ch & 0x3F)).toByte
			}
			else {
				dst({
					j += 1;
					j - 1
				}) = (0xE0 | (ch >> 12)).toByte
				dst({
					j += 1;
					j - 1
				}) = (0x80 | ((ch >> 6) & 0x3F)).toByte
				dst({
					j += 1;
					j - 1
				}) = (0x80 | (ch & 0x3F)).toByte
			}
		}
		j
	}

	/** Return characters as an array of bytes in Utf8 representation.
	 *
	 * @param src    The array holding the characters.
	 * @param sindex The start index from which characters are converted.
	 * @param len    The maximum number of characters to convert.
	 */
	def chars2utf(src: Array[Char], sindex: Int, len: Int): Array[Byte] = {
		val dst = new Array[Byte](len * 3)
		val len1 = chars2utf(src, sindex, dst, 0, len)
		val result = new Array[Byte](len1)
		System.arraycopy(dst, 0, result, 0, len1)
		result
	}

	/** Return all characters in given array as an array of bytes
	 * in Utf8 representation.
	 *
	 * @param src The array holding the characters.
	 */
	def chars2utf(src: Array[Char]): Array[Byte] = chars2utf(src, 0, src.length)

	/** Return string as an array of bytes in in Utf8 representation.
	 */
	def string2utf(s: String): Array[Byte] = chars2utf(s.toCharArray)

	/**
	 * Escapes each character in a string that has an escape sequence or
	 * is non-printable ASCII.  Leaves non-ASCII characters alone.
	 */
	def quote(s: String): String = {
		val buf = new StringBuilder
		for (i <- 0 until s.length) {
			buf.append(quote(s.charAt(i)))
		}
		buf.toString
	}

	/**
	 * Escapes a character if it has an escape sequence or is
	 * non-printable ASCII.  Leaves non-ASCII characters alone.
	 */
	def quote(ch: Char): String = ch match {
		case '\b' =>
			"\\b"
		case '\f' =>
			"\\f"
		case '\n' =>
			"\\n"
		case '\r' =>
			"\\r"
		case '\t' =>
			"\\t"
		case '\'' =>
			"\\'"
		case '\"' =>
			"\\\""
		case '\\' =>
			"\\\\"
		case _ =>
			if (isPrintableAscii(ch)) String.valueOf(ch)
			else String.format("\\u%04x", ch.toInt)
	}

	/**
	 * Is a character printable ASCII?
	 */
	private def isPrintableAscii(ch: Char) = ch >= ' ' && ch <= '~'

	/** Escape all unicode characters in string.
	 */
	def escapeUnicode(ss: String): String = {
		var s = ss
		val len = s.length
		var i = 0
		while (i < len) {
			var ch = s.charAt(i)
			if (ch > 255) {
				val buf = new StringBuilder
				buf.append(s.substring(0, i))
				while (i < len) {
					ch = s.charAt(i)
					if (ch > 255) {
						buf.append("\\u")
						buf.append(Character.forDigit((ch >> 12) % 16, 16))
						buf.append(Character.forDigit((ch >> 8) % 16, 16))
						buf.append(Character.forDigit((ch >> 4) % 16, 16))
						buf.append(Character.forDigit(ch % 16, 16))
					}
					else buf.append(ch)
					i += 1
				}
				s = buf.toString
			}
			else i += 1
		}
		s
	}

	/** Return the last part of a class name.
	 */
	/* Conversion routines for qualified name splitting
	 */
	def shortName(classname: FName): FName = classname.subName(classname.lastIndexOf('.'.toByte) + 1, classname.getByteLength)

	def shortName(classname: String): String = classname.substring(classname.lastIndexOf('.') + 1)

	/** Return the package name of a class name, excluding the trailing '.',
	 * "" if not existent.
	 */
	def packagePart(classname: FName): FName = classname.subName(0, classname.lastIndexOf('.'.toByte))

	def packagePart(classname: String): String = {
		val lastDot = classname.lastIndexOf('.')
		if (lastDot < 0) ""
		else classname.substring(0, lastDot)
	}

	def enclosingCandidates(name: FName): JCList[FName] = {
		var names = JCList.nil
		var index = 0
		while ((index = name.lastIndexOf('$'.toByte)) > 0) {
			name = name.subName(0, index)
			names = names.prepend(name)
		}
		names
	}

	def classCandidates(name: FName): JCList[FName] = {
		var names = JCList.nil
		val nameStr = name.toString
		var index = -1
		while ((index = nameStr.indexOf('.', index + 1)) > 0) {
			val pack = nameStr.substring(0, index + 1)
			val clz = nameStr.substring(index + 1).replace('.', '$')
			names = names.prepend(name.table.names.fromString(pack + clz))
		}
		names.reverse
	}
}
