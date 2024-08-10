package com.fdk.compiler.util


import java.lang.reflect.Array
import java.util._
import java.util.function.Function
import java.util.stream.Collector
import scala.collection.mutable.ListBuffer

object JCList {
	/** Construct an empty list.
	 */
	@SuppressWarnings(Array("unchecked")) def nil[A]: JCList[A] = EMPTY_LIST.asInstanceOf[JCList[A]]

	private val EMPTY_LIST = new JCList[AnyRef]((null, null)) {
		override def setTail(tail: JCList[AnyRef]): JCList[AnyRef] = throw new UnsupportedOperationException

		override def isEmpty = true
	}

	/** Returns the list obtained from 'l' after removing all elements 'elem'
	 */
	def filter[A](l: JCList[A], elem: A): JCList[A] = {
		Assert.checkNonNull(elem)
		var res = JCList.nil
		import scala.collection.JavaConversions._
		for (a <- l) {
			if (a != null && !(a == elem)) res = res.prepend(a)
		}
		res.reverse
	}

	/** Construct a list consisting of given element.
	 */
	def of[A](x1: A) = new JCList[A](x1, JCList.nil)

	/** Construct a list consisting of given elements.
	 */
	def of[A](x1: A, x2: A) = new JCList[A](x1, of(x2))

	/** Construct a list consisting of given elements.
	 */
	def of[A](x1: A, x2: A, x3: A) = new JCList[A](x1, of(x2, x3))

	/** Construct a list consisting of given elements.
	 */
	@SuppressWarnings(Array(Array("varargs", "unchecked"))) def of[A](x1: A, x2: A, x3: A, rest: A*) = new JCList[A](x1, new JCList[A](x2, new JCList[A](x3, from(rest))))

	/**
	 * Construct a list consisting all elements of given array.
	 *
	 * @param array an array; if {@code null} return an empty list
	 */
	def from[A](array: Array[A]): JCList[A] = {
		var xs = nil
		if (array != null) for (i <- array.length - 1 to 0 by -1) {
			xs = new JCList[A](array(i), xs)
		}
		xs
	}

	def from[A](coll: Iterable[_ <: A]): JCList[A] = {
		val xs = new ListBuffer[A]
		import scala.collection.JavaConversions._
		for (a <- coll) {
			xs.append(a)
		}
		xs.toList
	}

	/** Construct a list consisting of a given number of identical elements.
	 *
	 * @param len  The number of elements in the list.
	 * @param init The value of each element.
	 */
	@deprecated def fill[A](len: Int, init: A): JCList[A] = {
		var l = nil
		for (i <- 0 until len) {
			l = new JCList[A](init, l)
		}
		l
	}

	/** Are the two lists the same?
	 */
	def equals(xs: JCList[_], ys: JCList[_]): Boolean = {
		while (xs.tail != null && ys.tail != null) {
			if (xs.head == null) if (ys.head != null) return false
			else if (!(xs.head == ys.head)) return false
			xs = xs.tail
			ys = ys.tail
		}
		xs.tail == null && ys.tail == null
	}

	@SuppressWarnings(Array("unchecked")) def convert[T](klass: Class[T], list: JCList[_]): JCList[T] = {
		if (list == null) return null
		import scala.collection.JavaConversions._
		for (o <- list) {
			klass.cast(o)
		}
		list.asInstanceOf[JCList[T]]
	}

	private val EMPTYITERATOR = new util.Iterator[AnyRef]() {
		override def hasNext = false

		override def next: AnyRef = throw new NoSuchElementException

		override def remove(): Unit = {
			throw new UnsupportedOperationException
		}
	}

	@SuppressWarnings(Array("unchecked")) private def emptyIterator[A] = EMPTYITERATOR.asInstanceOf[util.Iterator[A]]

	/**
	 * Collect elements into a new list (using a @code{ListBuffer})
	 */
	def collector[Z]: Collector[Z, ListBuffer[Z], JCList[Z]] = Collector.of(com.flint.tools.flintc.util.ListBuffer.`new`, com.flint.tools.flintc.util.ListBuffer.add, (buf1: ListBuffer[Z], buf2: ListBuffer[Z]) => {
		buf1.addAll(buf2)
		buf1

	}, ListBuffer.toList)
}

class JCList[A] private[util](
											  /** The first element of the list, supposed to be immutable.
												*/
											  var head: A,

											  /** The remainder of the list except for its first element, supposed
												* to be immutable.
												*/
											  //@Deprecated
											  var tail: JCList[A])

/** Construct a list given its head and tail.
 */
		extends util.AbstractCollection[A] with util.List[A] {
	def intersect(that: JCList[A]): JCList[A] = {
		val buf = new ListBuffer[A]
		import scala.collection.JavaConversions._
		for (el <- this) {
			if (that.contains(el)) buf.append(el)
		}
		buf.toList
	}

	def diff(that: JCList[A]): JCList[A] = {
		val buf = new ListBuffer[A]
		import scala.collection.JavaConversions._
		for (el <- this) {
			if (!that.contains(el)) buf.append(el)
		}
		buf.toList
	}

	/**
	 * Create a new list from the first {@code n} elements of this list
	 */
	def take(n: Int): JCList[A] = {
		val buf = new ListBuffer[A]
		var count = 0
		import scala.collection.JavaConversions._
		for (el <- this) {
			if ( {
				count += 1;
				count - 1
			} == n) break //todo: break is not supported
			buf.append(el)
		}
		buf.toList
	}

	/** Does list have no elements?
	 */
	override def isEmpty: Boolean = tail == null

	/** Does list have elements?
	 */
	//@Deprecated
	def nonEmpty: Boolean = tail != null

	/** Return the number of elements in this list.
	 */
	//@Deprecated
	def length: Int = {
		var l = this
		var len = 0
		while (l.tail != null) {
			l = l.tail
			len += 1
		}
		len
	}

	override def size: Int = length

	def setTail(tail: JCList[A]): JCList[A] = {
		this.tail = tail
		tail
	}

	/** Prepend given element to front of list, forming and returning
	 * a new list.
	 */
	def prepend(x: A) = new JCList[A](x, this)

	/** Prepend given list of elements to front of list, forming and returning
	 * a new list.
	 */
	def prependList(xs: JCList[A]): JCList[A] = {
		if (this.isEmpty) return xs
		if (xs.isEmpty) return this
		if (xs.tail.isEmpty) return prepend(xs.head)
		// return this.prependList(xs.tail).prepend(xs.head);
		var result = this
		var rev = xs.reverse
		Assert.check(rev ne xs)
		// since xs.reverse() returned a new list, we can reuse the
		// individual List objects, instead of allocating new ones.
		while (rev.nonEmpty) {
			val h = rev
			rev = rev.tail
			h.setTail(result)
			result = h
		}
		result
	}

	/** Reverse list.
	 * If the list is empty or a singleton, then the same list is returned.
	 * Otherwise a new list is formed.
	 */
	def reverse: JCList[A] = {
		// if it is empty or a singleton, return itself
		if (isEmpty || tail.isEmpty) return this
		var rev = JCList.nil
		var l = this
		while (l.nonEmpty) {
			rev = new JCList[A](l.head, rev)
			l = l.tail
		}
		rev
	}

	/** Append given element at length, forming and returning
	 * a new list.
	 */
	def append(x: A): JCList[A] = JCList.of(x).prependList(this)

	/** Append given list at length, forming and returning
	 * a new list.
	 */
	def appendList(x: JCList[A]): JCList[A] = x.prependList(this)

	/**
	 * Append given list buffer at length, forming and returning a new
	 * list.
	 */
	def appendList(x: ListBuffer[A]): JCList[A] = appendList(x.toList)

	/** Copy successive elements of this list into given vector until
	 * list is exhausted or end of vector is reached.
	 */
	@SuppressWarnings(Array("unchecked")) override def toArray[T](vec: Array[T]): Array[T] = {
		var i = 0
		var l = this
		val dest = vec
		while (l.nonEmpty && i < vec.length) {
			dest(i) = l.head
			l = l.tail
			i += 1
		}
		if (l.isEmpty) {
			if (i < vec.length) vec(i) = null
			return vec
		}
		vec = Array.newInstance(vec.getClass.getComponentType, size).asInstanceOf[Array[T]]
		toArray(vec)
	}

	override def toArray: Array[AnyRef] = toArray(new Array[AnyRef](size))

	/** Form a string listing all elements with given separator character.
	 */
	def toString(sep: String): String = if (isEmpty) ""
	else {
		val buf = new StringBuilder
		buf.append(head)
		var l = tail
		while (l.nonEmpty) {
			buf.append(sep)
			buf.append(l.head)

			l = l.tail
		}
		buf.toString
	}

	/** Form a string listing all elements with comma as the separator character.
	 */
	override def toString: String = toString(",")

	/** Compute a hash code, overrides Object
	 *
	 * @see java.util.List#hashCode
	 */
	override def hashCode: Int = {
		var l = this
		var h = 1
		while (l.tail != null) {
			h = h * 31 + (if (l.head == null) 0
			else l.head.hashCode)
			l = l.tail
		}
		h
	}

	/** Is this list the same as other list?
	 *
	 * @see java.util.List#equals
	 */
	override def equals(other: AnyRef): Boolean = {
		if (other.isInstanceOf[JCList[_]]) return JCList.equals(this, other.asInstanceOf[JCList[_]])
		if (other.isInstanceOf[util.List[_]]) {
			var t = this
			val oIter = other.asInstanceOf[List[_]].iterator
			while (t.tail != null && oIter.hasNext) {
				val o = oIter.next
				if (!(if (t.head == null) o == null
				else t.head == (o))) return false
				t = t.tail
			}
			return t.isEmpty && !(oIter.hasNext)
		}
		false
	}

	/** Does the list contain the specified element?
	 */
	override def contains(x: AnyRef): Boolean = {
		var l = this
		while (l.tail != null) {
			if (x == null) if (l.head == null) return true
			else if (l.head == x) return true
			l = l.tail
		}
		false
	}

	/** The last element in the list, if any, or null.
	 */
	def last: A = {
		var last: A = null
		var t = this
		while (t.tail != null) {
			last = t.head
			t = t.tail
		}
		last
	}

	@SuppressWarnings(Array("unchecked")) def map[Z](mapper: Function[A, Z]): JCList[Z] = {
		var changed = false
		val buf = new ListBuffer[Z]
		import scala.collection.JavaConversions._
		for (a <- this) {
			val z = mapper.apply(a)
			buf.append(z)
			changed |= (z ne a)
		}
		if (changed) buf.toList
		else this.asInstanceOf[JCList[Z]]
	}

	override def iterator: util.Iterator[A] = {
		if (tail == null) return JCList.emptyIterator
		new util.Iterator[A]() {
			private[util] var elems = thisJCList

			override def hasNext: Boolean = return elems.tail != null

			override def next: A = {
				if (elems.tail == null) throw new NoSuchElementException
				val result = elems.head
				elems = elems.tail
				result
			}

			override def remove(): Unit = {
				throw new UnsupportedOperationException
			}
		}
	}

	override def get(index: Int): A = {
		if (index < 0) throw new IndexOutOfBoundsException(String.valueOf(index))
		var l = this
		var i = index
		while ( {
			i -= 1;
			i + 1
		} > 0 && !l.isEmpty) l = l.tail
		if (l.isEmpty) throw new IndexOutOfBoundsException("Index: " + index + ", " + "Size: " + size)
		l.head
	}

	override def addAll(index: Int, c: util.Collection[_ <: A]): Boolean = {
		if (c.isEmpty) return false
		throw new UnsupportedOperationException
	}

	override def set(index: Int, element: A): A = throw new UnsupportedOperationException

	override def add(index: Int, element: A): Unit = {
		throw new UnsupportedOperationException
	}

	override def remove(index: Int): A = throw new UnsupportedOperationException

	override def indexOf(o: AnyRef): Int = {
		var i = 0
		var l = this
		while (l.tail != null) {
			if (if (l.head == null) o == null
			else l.head == o) return i

			l = l.tail
			i += 1
		}
				- 1
	}

	override def lastIndexOf(o: AnyRef): Int = {
		var last = -1
		var i = 0
		var l = this
		while (l.tail != null) {
			if (if (l.head == null) o == null
			else l.head == o) last = i

			l = l.tail
			i += 1
		}
		last
	}

	override def listIterator: util.ListIterator[A] = Collections.unmodifiableList(new util.ArrayList[A](this)).listIterator

	override def listIterator(index: Int): util.ListIterator[A] = Collections.unmodifiableList(new util.ArrayList[A](this)).listIterator(index)

	override def subList(fromIndex: Int, toIndex: Int): util.List[A] = {
		if (fromIndex < 0 || toIndex > size || fromIndex > toIndex) throw new IllegalArgumentException
		val a = new util.ArrayList[A](toIndex - fromIndex)
		var i = 0
		var l = this
		while (l.tail != null) {
			if (i == toIndex) break //todo: break is not supported
			if (i >= fromIndex) a.add(l.head)

			l = l.tail
			i += 1
		}
		Collections.unmodifiableList(a)
	}
}
