package com.fdk.compiler.parser

object LayoutChars {

	/** Tabulator character.
	 */
	val TAB:Char = 0x9

	/** Line feed character.
	 */
	val LF:Char = 0xA

	/** Form feed character.
	 */
	val FF:Char = 0xC

	/** Carriage return character.
	 */
	val CR:Char = 0xD

	/** End of input character.  Used as a sentinel to denote the
	 * character one beyond the last defined character in a
	 * source file.
	 */
	val EOI:Char = 0x1A
}
