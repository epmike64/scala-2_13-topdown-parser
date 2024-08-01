package com.fdk.compiler

import com.fdk.compiler.parser.{FTag, FTokenKind}
import com.fdk.compiler.util.FName

object FToken {
	val DUMMY = FToken(FTokenKind.ERROR, 0, 0)
}

class FToken(val kind: FTokenKind, val pos: Int, val endPos: Int){
	def name: FName = ???
	def checkKind(): Unit = {
		assert(kind.tag == FTag.DEFAULT)
	}
}

class FNamedToken(val kind: FTokenKind, pos: Int, endPos: Int, override val name: FName) extends FToken(kind, pos, endPos){
}

class FStringToken(val kind: FTokenKind, pos: Int, endPos: Int, val stringVal: String) extends FToken(kind, pos, endPos){
}

class FNumericToken(val kind: FTokenKind, pos: Int, endPos: Int, val stringVal: String, val radix:Int) extends FStringToken(kind, pos, endPos, stringVal){
}