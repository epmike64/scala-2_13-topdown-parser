package com.fdk.compiler.tree

import com.fdk.compiler.tree.FTree

trait EndPosTable {
	def getEndPos(tree: FTree): Int
	def storeEnd(tree: FTree, endPos: Int): Unit
}