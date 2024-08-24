package com.fdk.compiler.tree

import com.fdk.compiler.parser.FToken.FTokenKind

import scala.collection.mutable.ArrayBuffer

trait FTree {
}

case object FNon extends FTree

class FPackage(val qs:FTree) extends FTree
class FImport(val vs:List[FImportExpr]) extends FTree
class FImportExpr(val sid:FStableId) extends FTree {
	var selectors: List[FImportSelector] = List()
}
class FImportSelector(val id:FIdent) extends FTree {
	var alias: FIdent = null
}
class FIdent(var bFlag: Int, val name:String = "") extends FTree
class FCompilationUnit(val packages:List[FTree], val topSmnts: List[FTree]) extends FTree
class FClassDef extends FTree
class FObjectDef extends FTree
class FTraitDef extends FTree
class FAccessQualifier extends FTree
class FLiteral extends FTree
class FQualId(val ids:List[FTree]) extends FTree
class FType extends FTree
class FStableId extends FTree {
	private[this] val ids = ArrayBuffer[FTree]()
	def addId(id: FTree) = ids += id
}
class FClassQualifier extends FTree
class FFunctionArgTypes extends FTree
class FInfixType extends FTree
class FExistentialClause extends FTree
class FParamType(val t: FType) extends FTree
class FExistentialDcl extends FTree
class FTypeDcl extends FTree
class FValDcl extends FTree
class FCompoundType extends FTree
class FAnnotType extends FTree
class FRefinement extends FTree
class FSimpleType(var ann: FTree = null) extends FTree
class FTypes extends FTree
class FTypeArgs extends FTree
class FRefineStat extends FTree
class FDcl extends FTree
class FTypeDef extends FTree
class FTypePat extends FTree
class FAscription extends FTree
class FAnnotation extends FTree
class FExpr extends FTree
class FExprs extends FTree
class FBindings extends FTree
class FExpr1 extends FTree
class FPostfixExpr extends FTree
class FPrefixDef extends FTree
class FSimpleExpr extends FTree
class FSimpleExpr1 extends FTree
class FArgumentExprs extends FTree
class FBlockExpr extends FTree
class FArgs extends FTree
class FBlock extends FTree
class FBlockStat extends FTree
class FCaseClause extends FTree
class FCaseClauses extends FTree
class FResultExpr extends FTree
class FDef extends FTree
class FEnumerators extends FTree
class FGenerator extends FTree
class FGuard extends FTree
class FPattern1 extends FTree
class FPattern2 extends FTree
class FPattern3 extends FTree
class FSimplePattern extends FTree
class FPatterns extends FTree
class FPattern extends FTree
class FTypeParamClause(val vtps: List[FVariantTypeParam]) extends FTree
class FVariantTypeParam(val plusMinus:FIdent, val tp: FTypeParam) extends FTree
class FTypeParam(val id: FIdent, var tpc:FTypeParamClause=null, var lowerB:FType=null, var upperB:FType=null, var ctxB: FType=null, var parType: FType=null) extends FTree {}
class FFunTypeParamClause extends FTree
class FParamClauses extends FTree
class FParamClause extends FTree
class FParams extends FTree
class FParam extends FTree
class FClassParamClause extends FTree
class FClassParam(val mods:FModifiers, val varVal:FTokenKind, val id:FIdent, val pt:FParamType, val exp:FExpr) extends FTree
class FClassParams extends FTree
class FClassParamClauses extends FTree
class FTemplateBody extends FTree
class FTemplateStat extends FTree
class FSelfType extends FTree
class FModifiers(val bFlags: Int) extends FTree
