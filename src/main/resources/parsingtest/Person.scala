
abstract class Person {

	//* def -(a: Int): Unit ****/a
	def *(a: Int): Char
	/* def /(a: Int): Unit
	*/
	def ++(a: Int): Boolean

	def --(a: Int): Unit = println("Meow")

	def **(a: Int): Unit = println("Meow")

	def <(a: Int): Unit = println("Meow")

	def >(a: Int): Unit = println("Meow")

	def /:(a: Int): Unit = println("Meow")

	def :@(a: Int): Unit = println("Meow")

	def !(a: Int): Unit = println("Meow")

	//	case '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~' =>
	def +++(x: Int) = {
		val _h_@ = x
	}
	//	def =(x: Int) = 5 + x
}

object Main extends App {
	val p = new Person
	val + = 6
	val z = 7 + 8
	val > = 7
	val __ = 8
	println(z)
}
