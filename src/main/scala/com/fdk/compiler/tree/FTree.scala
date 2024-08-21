package com.fdk.compiler.tree

trait FTree {

}
case object FEmpty extends FTree

class FImport extends FTree
class FStableId extends FTree
class FIdent(val name:String) extends FTree