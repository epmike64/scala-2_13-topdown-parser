//package com.fdk.compiler.tree
//
//import com.fdk.compiler.tree.*
//import com.fdk.compiler.util.FName
//
//class FTreeMaker {
//	private var pos: Int = -1
//
//	def at(pos: Int): FTreeMaker = {
//		this.pos = pos
//		this
//	}
//
//	def ident(name: FName): FIdent = {
//		val tree = FIdent(name)
//		tree.pos = pos
//		tree
//	}
//
//	def select(selected: FExpression, name: FName): FFieldAccess = {
//		val tree = FFieldAccess(selected, name, null)
//		tree.pos = pos
//		tree
//	}
//
//	def packageDecl(pid: FExpression): FPackageDecl = {
//		assert(pid != null)
//		val tree = FPackageDecl(pid)
//		tree.pos = pos
//		tree
//	}
//	
//	def makeImport(qualid: FTree): FImport = {
//		FImport(qualid)
//	}
//
//	def makeClassDecl(): FClassDecl = {
//		FClassDecl()
//	}
//
//	def makeMethodDecl(): FMethodDecl = {
//		FMethodDecl()
//	}
//
//	def makeVariableDecl(): FVariableDecl = {
//		FVariableDecl()
//	}
//
//	def makeSkip(): FSkip = {
//		FSkip()
//	}
//
//	def makeBlock(): FBlock = {
//		FBlock()
//	}
//
//	def makeDoWhileLoop(): FDoWhileLoop = {
//		FDoWhileLoop()
//	}
//
//	def makeWhileLoop(): FWhileLoop = {
//		FWhileLoop()
//	}
//
//	def makeForLoop(): FForLoop = {
//		FForLoop()
//	}
//
//	def makeEnhancedForLoop(): FEnhancedForLoop = {
//		FEnhancedForLoop()
//	}
//
//	def makeLabeledStatement(): FLabeledStatement = {
//		FLabeledStatement()
//	}
//}
