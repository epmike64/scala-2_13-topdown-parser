package com.fdk.compiler.util

abstract class FName(val table:FTable)  {

	def contentEquals(cs: CharSequence): Boolean = toString == cs.toString
	def length: Int = toString.length
	def charAt(index: Int): Char = toString.charAt(index)
	def subSequence(start: Int, end: Int): CharSequence = toString.subSequence(start, end)

	/** Return the concatenation of this name and name `n'.
	 */
	def append(n: FName): FName = {
		val len = byteLength
		val bs = new Array[Byte](len + n.byteLength)
		getBytes(bs, 0)
		n.getBytes(bs, len)
		table.fromUtf(bs, 0, bs.length)
	}

	/** Return the concatenation of this name, the given ASCII
	 * character, and name `n'.
	 */
	def append(c: Char, n: FName): FName = {
		val len = byteLength
		val bs = new Array[Byte](len + 1 + n.byteLength)
		getBytes(bs, 0)
		bs(len) = c.toByte
		n.getBytes(bs, len + 1)
		table.fromUtf(bs, 0, bs.length)
	}

	/** An arbitrary but consistent complete order among all Names.
	 */
	def compareTo(other: FName): Int = other.index - this.index

	/** Return true if this is the empty name.
	 */
	def isEmpty: Boolean = byteLength == 0

	/** Returns last occurrence of byte b in this name, -1 if not found.
	 */
	def lastIndexOf(b: Byte): Int = {
		val bytes = byteArray
		val offset = getByteOffset
		var i = byteLength - 1
		while (i >= 0 && bytes(offset + i) != b) i -= 1
		i
	}

	/** Does this name start with prefix?
	 */
	def startsWith(prefix: FName): Boolean = {
		val thisBytes = this.byteArray
		val thisOffset = this.getByteOffset
		val thisLength = this.byteLength
		val prefixBytes = prefix.byteArray
		val prefixOffset = prefix.getByteOffset
		val prefixLength = prefix.byteLength
		if (thisLength < prefixLength) return false
		var i = 0
		while (i < prefixLength && thisBytes(thisOffset + i) == prefixBytes(prefixOffset + i)) i += 1
		i == prefixLength
	}

	/** Returns the sub-name starting at position start, up to and
	 * excluding position end.
	 */
	def subName(start: Int, end: Int): FName = {
		val theEnd = if (end < start) start else end
		table.fromUtf(byteArray, getByteOffset + start, theEnd - start)
	}

	/** Return the string representation of this name.
	 */
	override def toString: String = Convert.utf2string(byteArray, getByteOffset, byteLength)

	/** Return the Utf8 representation of this name.
	 */
	def toUtf: Array[Byte] = {
		val bs = new Array[Byte](byteLength)
		getBytes(bs, 0)
		bs
	}

	/* Get a "reasonably small" value that uniquely identifies this name
		  * within its name table.
		  */
	def index: Int

	/** Get the length (in bytes) of this name.
	 */
	def byteLength: Int

	/** Returns i'th byte of this name.
	 */
	def byteAt(i: Int): Byte

	/** Copy all bytes of this name to buffer cs, starting at start.
	 */
	def getBytes(cs: Array[Byte], start: Int): Unit = {
		System.arraycopy(byteArray, getByteOffset, cs, start, byteLength)
	}

	/** Get the underlying byte array for this name. The contents of the
	 * array must not be modified.
	 */
	def byteArray: Array[Byte]

	/** Get the start offset of this name within its byte array.
	 */
	def getByteOffset: Int
}

class FNameImpl(val table:FTable, val bytes: Array[Byte], val index: Int) extends FName(table) {
	var next: FNameImpl = null
	override var byteLength: Int = -1
	override def byteArray: Array[Byte] = table.bytes
	override def byteAt(i: Int): Byte = byteArray(index + i)
}
