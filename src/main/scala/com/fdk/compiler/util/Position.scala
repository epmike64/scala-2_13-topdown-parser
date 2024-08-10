package com.fdk.compiler.util



import com.flint.tools.flintc.util.DefinedBy.Api

import java.util
import com.flint.tools.flintc.util.LayoutCharacters.TabInc

import java.util.BitSet


object Position {
	val NOPOS: Int = -1
	val FIRSTPOS = 0
	val FIRSTLINE = 1
	val FIRSTCOLUMN = 1
	val LINESHIFT = 10
	val MAXCOLUMN: Int = (1 << LINESHIFT) - 1
	val MAXLINE: Int = (1 << (Integer.SIZE - LINESHIFT)) - 1
	val MAXPOS: Int = Integer.MAX_VALUE

	/** A two-way map between line/column numbers and positions,
	 * derived from a scan done at creation time.  Tab expansion is
	 * optionally supported via a character map.  Text content
	 * is not retained.
	 * <p>
	 * Notes:  The first character position FIRSTPOS is at
	 * (FIRSTLINE,FIRSTCOLUMN).  No account is taken of Unicode escapes.
	 *
	 * @param src        Source characters
	 * @param max        Number of characters to read
	 * @param expandTabs If true, expand tabs when calculating columns
	 */
	def makeLineMap(src: Array[Char], max: Int, expandTabs: Boolean): Position.LineMap = {
		val lineMap = if (expandTabs) new Position.LineTabMapImpl(max)
		else new Position.LineMapImpl
		lineMap.build(src, max)
		lineMap
	}

	/** Encode line and column numbers in an integer as:
	 * {@code line-number << LINESHIFT + column-number }.
	 * {@link Position# NOPOS} represents an undefined position.
	 *
	 * @param line number of line (first is 1)
	 * @param col  number of character on line (first is 1)
	 * @return an encoded position or {@link Position# NOPOS}
	 *         if the line or column number is too big to
	 *         represent in the encoded format
	 * @throws IllegalArgumentException if line or col is less than 1
	 */
	def encodePosition(line: Int, col: Int): Int = {
		if (line < 1) throw new IllegalArgumentException("line must be greater than 0")
		if (col < 1) throw new IllegalArgumentException("column must be greater than 0")
		if (line > MAXLINE || col > MAXCOLUMN) return NOPOS
		(line << LINESHIFT) + col
	}

	trait LineMap extends LineMap {
		/** Find the start position of a line.
		 *
		 * @param line number of line (first is 1)
		 * @return position of first character in line
		 * @throws ArrayIndexOutOfBoundsException
		 * if {@code lineNumber < 1}
		 * if {@code lineNumber > no. of lines}
		 */
		def getStartPosition(line: Int): Int

		/** Find the position corresponding to a (line,column).
		 *
		 * @param line   number of line (first is 1)
		 * @param column number of character on line (first is 1)
		 * @return position of character
		 * @throws ArrayIndexOutOfBoundsException
		 * if {@code line < 1}
		 * if {@code line > no. of lines}
		 */
		def getPosition(line: Int, column: Int): Int

		/** Find the line containing a position; a line termination
		 * character is on the line it terminates.
		 *
		 * @param pos character offset of the position
		 * @return the line number on which pos occurs (first line is 1)
		 */
		def getLineNumber(pos: Int): Int

		/** Find the column for a character position.
		 * Note:  this method does not handle tab expansion.
		 * If tab expansion is needed, use a LineTabMap instead.
		 *
		 * @param pos character offset of the position
		 * @return the column number at which pos occurs
		 */
		def getColumnNumber(pos: Int): Int
	}

	private[util] object LineMapImpl {
		private def longToInt(longValue: Long) = {
			val intValue = longValue.toInt
			if (intValue != longValue) throw new IndexOutOfBoundsException
			intValue
		}
	}

	private[util] class LineMapImpl protected extends Position.LineMap {
		protected var startPosition: Array[Int] = null // start position of each line

		protected def build(src: Array[Char], max: Int): Unit = {
			var c = 0
			var i = 0
			val linebuf = new Array[Int](max)
			while (i < max) {
				linebuf({
					c += 1;
					c - 1
				}) = i
				do {
					val ch = src(i)
					if (ch == '\r' || ch == '\n') {
						if (ch == '\r' && (i + 1) < max && src(i + 1) == '\n') i += 2
						else i += 1
						break //todo: break is not supported

					}
					else if (ch == '\t') setTabPosition(i)
				} while ( {
					i += 1;
					i
				} < max)
			}
			this.startPosition = new Array[Int](c)
			System.arraycopy(linebuf, 0, startPosition, 0, c)
		}

		override def getStartPosition(line: Int): Int = startPosition(line - FIRSTLINE)

		@DefinedBy(Api.COMPILER_TREE) override def getStartPosition(line: Long): Long = getStartPosition(LineMapImpl.longToInt(line))

		override def getPosition(line: Int, column: Int): Int = startPosition(line - FIRSTLINE) + column - FIRSTCOLUMN

		@DefinedBy(Api.COMPILER_TREE) override def getPosition(line: Long, column: Long): Long = getPosition(LineMapImpl.longToInt(line), LineMapImpl.longToInt(column))

		// Cache of last line number lookup
		private var lastPosition = Position.FIRSTPOS
		private var lastLine = Position.FIRSTLINE

		override def getLineNumber(pos: Int): Int = {
			if (pos == lastPosition) return lastLine
			lastPosition = pos
			var low = 0
			var high = startPosition.length - 1
			while (low <= high) {
				val mid = (low + high) >> 1
				val midVal = startPosition(mid)
				if (midVal < pos) low = mid + 1
				else if (midVal > pos) high = mid - 1
				else {
					lastLine = mid + 1 // pos is at beginning of this line

					return lastLine
				}
			}
			lastLine = low
			lastLine // pos is on this line

		}

		@DefinedBy(Api.COMPILER_TREE) override def getLineNumber(pos: Long): Long = getLineNumber(LineMapImpl.longToInt(pos))

		override def getColumnNumber(pos: Int): Int = pos - startPosition(getLineNumber(pos) - FIRSTLINE) + FIRSTCOLUMN

		@DefinedBy(Api.COMPILER_TREE) override def getColumnNumber(pos: Long): Long = getColumnNumber(LineMapImpl.longToInt(pos))

		protected def setTabPosition(offset: Int): Unit = {
		}
	}

	/**
	 * A LineMap that handles tab expansion correctly.  The cost is
	 * an additional bit per character in the source array.
	 */
	class LineTabMapImpl(max: Int) extends Position.LineMapImpl {
		tabMap = new util.BitSet(max)
		private var tabMap: util.BitSet = null // bits set for tab positions.

		override protected def setTabPosition(offset: Int): Unit = {
			tabMap.set(offset)
		}

		override def getColumnNumber(pos: Int): Int = {
			val lineStart = startPosition(getLineNumber(pos) - FIRSTLINE)
			var column = 0
			for (bp <- lineStart until pos) {
				if (tabMap.get(bp)) column = (column / TabInc * TabInc) + TabInc
				else column += 1
			}
			column + FIRSTCOLUMN
		}

		override def getPosition(line: Int, column: Int): Int = {
			var pos = startPosition(line - FIRSTLINE)
			column -= FIRSTCOLUMN
			var col = 0
			while (col < column) {
				pos += 1
				if (tabMap.get(pos)) col = (col / TabInc * TabInc) + TabInc
				else col += 1
			}
			pos
		}
	}
}

class Position private

/**
 * This is class is not supposed to be instantiated.
 */ {
}
