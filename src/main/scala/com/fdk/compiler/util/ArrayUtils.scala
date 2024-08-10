package com.fdk.compiler.util


object ArrayUtils {
	private def calculateNewLength(currentLength: Int, maxIndex: Int) = {
		var cl = currentLength
		while (currentLength < maxIndex + 1) cl = cl * 2
		cl
	}

	def ensureCapacity[T](array: Array[T], maxIndex: Int): Array[T] = if (maxIndex < array.length) array
	else {
		val newLength = calculateNewLength(array.length, maxIndex)
		@SuppressWarnings(Array("unchecked")) val result = java.lang.reflect.Array.newInstance(array.getClass.getComponentType, newLength).asInstanceOf[Array[T]]
		System.arraycopy(array, 0, result, 0, array.length)
		result
	}

	def ensureCapacity(array: Array[Byte], maxIndex: Int): Array[Byte] = if (maxIndex < array.length) array
	else {
		val newLength = calculateNewLength(array.length, maxIndex)
		val result = new Array[Byte](newLength)
		System.arraycopy(array, 0, result, 0, array.length)
		result
	}

	def ensureCapacity(array: Array[Char], maxIndex: Int): Array[Char] = if (maxIndex < array.length) array
	else {
		val newLength = calculateNewLength(array.length, maxIndex)
		val result = new Array[Char](newLength)
		System.arraycopy(array, 0, result, 0, array.length)
		result
	}

	def ensureCapacity(array: Array[Int], maxIndex: Int): Array[Int] = if (maxIndex < array.length) array
	else {
		val newLength = calculateNewLength(array.length, maxIndex)
		val result = new Array[Int](newLength)
		System.arraycopy(array, 0, result, 0, array.length)
		result
	}
}
