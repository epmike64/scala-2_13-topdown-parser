package com.fdk.compiler.parser

import com.fdk.compiler.parser.{FTag, FTokenKind}
import com.fdk.compiler.util.FName
import com.fdk.compiler.parser.FTokens

object FToken {
	val DUMMY = FToken(FTokenKind.ERROR, 0, 0)
}

class FToken(val kind: FTokenKind, val pos: Int, val endPos: Int){
	{
		checkKind()
	}
	def name: FName = throw UnsupportedOperationException()
	def stringVal: String = throw UnsupportedOperationException()
	def radix: Int = throw UnsupportedOperationException()
	
	def checkKind(): Unit = {
		assert(kind.tag == FTag.DEFAULT)
	}

	def split(): Array[FToken] = {
		if (kind.name.length < 2 || kind.tag != FTag.DEFAULT) throw new AssertionError("Cant split" + kind)
		val t1 = FTokens.lookupKind(kind.name.substring(0, 1))
		val t2 = FTokens.lookupKind(kind.name.substring(1))
		if (t1 == null || t2 == null) throw new AssertionError("Cant split - bad subtokens")
		Array[FToken](new FToken(t1, pos, pos + t1.name.length), new FToken(t2, pos + t1.name.length, endPos))
	}
}

class FNamedToken(val kind: FTokenKind, pos: Int, endPos: Int, override val name: FName) extends FToken(kind, pos, endPos){
	override def checkKind(): Unit = {
		assert(kind.tag == FTag.NAMED)
	}
}

class FStringToken(val kind: FTokenKind, pos: Int, endPos: Int, override val stringVal: String) extends FToken(kind, pos, endPos){
	override def checkKind(): Unit = {
		assert(kind.tag == FTag.STRING)
	}
}

class FNumericToken(val kind: FTokenKind, pos: Int, endPos: Int, val stringVal: String, override val radix:Int) extends FStringToken(kind, pos, endPos, stringVal){
	override def checkKind(): Unit = {
		assert(kind.tag == FTag.NUMERIC)
	}
}