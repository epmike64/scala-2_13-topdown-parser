class AddingHoldingFlag1(val transformation: KingTransformation) extends TransformationTrait{

	def addingHoldingFlag1(previousDF: DataFrame): Frame = {
		// adding holding flag field when a customer has at least one account valid and is in scope for CS
		val addingFlag = previousDF
				.withColumn(Forms.HOLD_1, when(col(Forms.capamer).contains("WER") && col(Forms.FLCOUNT) === 1, lit("YES"))
						.otherwise(lit("NO")))
		addingFlag
	}
}