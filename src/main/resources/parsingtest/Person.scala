
object Person {

	def &:(name: String, age: Int) = new Person(name, age)

	def main(args: Array[String]): Unit = {
		val p = new Person("John", 10)
		val w = new Worker("Mike", 20, "Developer")
		val k = new Worker2("Mike", 20, "Developer")
	}
}