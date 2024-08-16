class AddingHoldingFlag1(val transformation: KingTransformation) extends TransformationTrait{

	def concatTest() = {
		val a = "test" + "test" + "test"
		val schema = StructType(Seq( StructField(Fields.Alpha + "_trc", StringType), StructField(Fields.Beta + "_trc", DecimalType(10,0)), StructField(Fields.Gamma, StringType) ))
	}
}