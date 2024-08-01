package com.fdk.compiler.tree

trait IFVisitor {
	def visitTopLevel(that: FCompilationUnit): Unit = {
		visitTree(that)
	}

	def visitPackageDef(that: FPackageDecl): Unit = {
		visitTree(that)
	}

	def visitImport(that: FImport): Unit = {
		visitTree(that)
	}

	def visitClassDef(that: FClassDecl): Unit = {
		visitTree(that)
	}

	def visitMethodDef(that: FMethodDecl): Unit = {
		visitTree(that)
	}

	def visitVarDef(that: FVariableDecl): Unit = {
		visitTree(that)
	}

	def visitSkip(that: FSkip): Unit = {
		visitTree(that)
	}

	def visitBlock(that: FBlock): Unit = {
		visitTree(that)
	}

	def visitDoLoop(that: FDoWhileLoop): Unit = {
		visitTree(that)
	}

	def visitWhileLoop(that: FWhileLoop): Unit = {
		visitTree(that)
	}

	def visitForLoop(that: FForLoop): Unit = {
		visitTree(that)
	}

	def visitForeachLoop(that: FEnhancedForLoop): Unit = {
		visitTree(that)
	}

	def visitLabelled(that: FLabeledStatement): Unit = {
		visitTree(that)
	}

	def visitSwitch(that: FSwitch): Unit = {
		visitTree(that)
	}

	def visitCase(that: FCase): Unit = {
		visitTree(that)
	}

	def visitSynchronized(that: FSynchronized): Unit = {
		visitTree(that)
	}

	def visitTry(that: FTry): Unit = {
		visitTree(that)
	}

	def visitCatch(that: FCatch): Unit = {
		visitTree(that)
	}

	def visitConditional(that: FConditional): Unit = {
		visitTree(that)
	}

	def visitIf(that: FIf): Unit = {
		visitTree(that)
	}

	def visitExec(that: FExpressionStatement): Unit = {
		visitTree(that)
	}

	def visitBreak(that: FBreak): Unit = {
		visitTree(that)
	}

	def visitContinue(that: FContinue): Unit = {
		visitTree(that)
	}

	def visitReturn(that: FReturn): Unit = {
		visitTree(that)
	}

	def visitThrow(that: FThrow): Unit = {
		visitTree(that)
	}

	def visitAssert(that: FAssert): Unit = {
		visitTree(that)
	}

	def visitApply(that: FMethodInvocation): Unit = {
		visitTree(that)
	}

	def visitNewClass(that: FNewClass): Unit = {
		visitTree(that)
	}

	def visitNewArray(that: FNewArray): Unit = {
		visitTree(that)
	}

	def visitLambda(that: FLambda): Unit = {
		visitTree(that)
	}

	def visitParens(that: FParens): Unit = {
		visitTree(that)
	}

	def visitAssign(that: FAssign): Unit = {
		visitTree(that)
	}

	def visitAssignop(that: FAssignOp): Unit = {
		visitTree(that)
	}

	def visitUnary(that: FUnary): Unit = {
		visitTree(that)
	}

	def visitBinary(that: FBinary): Unit = {
		visitTree(that)
	}

	def visitTypeCast(that: FTypeCast): Unit = {
		visitTree(that)
	}

	def visitTypeTest(that: FInstanceOf): Unit = {
		visitTree(that)
	}

	def visitIndexed(that: FArrayAccess): Unit = {
		visitTree(that)
	}

	def visitSelect(that: FFieldAccess): Unit = {
		visitTree(that)
	}

	def visitReference(that: FMemberReference): Unit = {
		visitTree(that)
	}

	def visitIdent(that: FIdent): Unit = {
		visitTree(that)
	}

	def visitLiteral(that: FLiteral): Unit = {
		visitTree(that)
	}

	def visitTypeIdent(that: FPrimitiveTypeTree): Unit = {
		visitTree(that)
	}

	def visitTypeArray(that: FArrayTypeTree): Unit = {
		visitTree(that)
	}

	def visitTypeApply(that: FTypeApply): Unit = {
		visitTree(that)
	}

	def visitTypeUnion(that: FTypeUnion): Unit = {
		visitTree(that)
	}

	def visitTypeIntersection(that: FTypeIntersection): Unit = {
		visitTree(that)
	}

	def visitTypeParameter(that: FTypeParameter): Unit = {
		visitTree(that)
	}

	def visitWildcard(that: FWildcard): Unit = {
		visitTree(that)
	}

	def visitTypeBoundKind(that: TypeBoundKind): Unit = {
		visitTree(that)
	}

	def visitAnnotation(that: FAnnotation): Unit = {
		visitTree(that)
	}

	def visitModifiers(that: FModifiers): Unit = {
		visitTree(that)
	}

	def visitAnnotatedType(that: FAnnotatedType): Unit = {
		visitTree(that)
	}

	def visitErroneous(that: FErroneous): Unit = {
		visitTree(that)
	}

	def visitModuleDef(that: FModuleDecl): Unit = {
		visitTree(that)
	}

	def visitExports(that: FExports): Unit = {
		visitTree(that)
	}

	def visitOpens(that: FOpens): Unit = {
		visitTree(that)
	}

	def visitProvides(that: FProvides): Unit = {
		visitTree(that)
	}

	def visitRequires(that: FRequires): Unit = {
		visitTree(that)
	}

	def visitUses(that: FUses): Unit = {
		visitTree(that)
	}

	def visitLetExpr(that: LetExpr): Unit = {
		visitTree(that)
	}

	def visitTree(that: FTree): Unit = {
		throw new UnsupportedOperationException(that.getClass.getName)
	}
}