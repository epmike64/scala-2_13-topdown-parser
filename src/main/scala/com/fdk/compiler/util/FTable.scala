package com.fdk.compiler.util

import java.lang.ref.WeakReference
import scala.util.boundary

object FTable {

	/** The hashcode of a name.
	 */
	protected def hashValue(bytes: Array[Byte], offset: Int, length: Int): Int = {
		var h = 0
		var off = offset
		for (i <- 0 until length) {
			h = (h << 5) - h + bytes({
				off += 1;
				off - 1
			})
		}
		h
	}

	/** Compare two subarrays
	 */
	protected def equals(bytes1: Array[Byte], offset1: Int, bytes2: Array[Byte], offset2: Int, length: Int): Boolean = {
		var i = 0
		while (i < length && bytes1(offset1 + i) == bytes2(offset2 + i)) i += 1
		i == length
	}
}

trait IFTable {
	/** Get the name from the characters in cs[start..start+len-1].
	 */
	def fromChars(cs: Array[Char], start: Int, len: Int): FName

	/** Get the name for the characters in string s.
	 */
	def fromString(s: String): FName = {
		val cs = s.toCharArray
		fromChars(cs, 0, cs.length)
	}

	/** Get the name for the bytes in array cs.
	 * Assume that bytes are in utf8 format.
	 */
	def fromUtf(cs: Array[Byte]): FName = fromUtf(cs, 0, cs.length)

	/** get the name for the bytes in cs[start..start+len-1].
	 * Assume that bytes are in utf8 format.
	 */
	def fromUtf(cs: Array[Byte], start: Int, len: Int): FName

	/** Release any resources used by this table.
	 */
	def dispose(): Unit
}

class FHashEntry private[util](referent: FNameImpl) extends WeakReference[FNameImpl](referent) {
	var next: FHashEntry = null
}

class FTable(hashSize: Int = 0x8000, nameSize: Int = 0x20000) extends IFTable {
	/** The hash table for names.
	 */
	var hashes = new Array[FHashEntry](hashSize)
	/** The shared byte array holding all encountered names.
	 */
	var bytes = new Array[Byte](nameSize)

	/** The mask to be used for hashing
	 */
	var hashMask = hashSize - 1

	/** Index counter for names in this table.
	 */
	var index = 0

	override def fromChars(cs: Array[Char], start: Int, len: Int): FName = {
		val name = new Array[Byte](len * 3)
		val nbytes = Convert.chars2utf(cs, start, name, 0, len)
		fromUtf(name, 0, nbytes)
	}

	override def fromUtf(cs: Array[Byte], start: Int, len: Int): FName = {
		val h = FTable.hashValue(cs, start, len) & hashMask
		var element = hashes(h)
		var n: FNameImpl = null
		var previousNonNullTableEntry: FHashEntry = null
		var firstTableEntry = element
		boundary {
			while (element != null) {
				if (element == null) {
					boundary.break()
				}
				n = element.get
				if (n == null) {
					if (firstTableEntry eq element) {
						firstTableEntry = element.next
						hashes(h) = firstTableEntry
					}
					else {
						assert(previousNonNullTableEntry != null, "previousNonNullTableEntry cannot be null here.")
						previousNonNullTableEntry.next = element.next
					}
				}

				else {
					if (n.byteLength == len && equals(n.bytes, 0, cs, start, len)) return n
					previousNonNullTableEntry = element
				}
				element = element.next
			}
		}

		val bytes = new Array[Byte](len)
		System.arraycopy(cs, start, bytes, 0, len)
		n = new FNameImpl(this, bytes, {
			index += 1;
			index - 1
		})
		val newEntry = new FHashEntry(n)
		if (previousNonNullTableEntry == null) { // We are not the first name with that hashCode.
			hashes(h) = newEntry
		}
		else {
			assert(previousNonNullTableEntry.next != null, "previousNonNullTableEntry.next must be null.")
			previousNonNullTableEntry.next = newEntry
		}
		n
	}

	override def dispose(): Unit = {
		hashes = null
	}
}
