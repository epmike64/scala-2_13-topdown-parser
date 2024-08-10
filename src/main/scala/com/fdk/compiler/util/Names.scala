package com.fdk.compiler.util

object Names {

	val table = new FTable

	def fromString(s: String): FName = {
		table.fromString(s)
	}
}
