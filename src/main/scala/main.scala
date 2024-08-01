
class Car(val make: String = "Toyota", val model: String = "Corolla", val year: Int = 2021) {
  def this(make: String, model: String) = this(make, model, 2021)
  def this(make: String) = this(make, "Corolla", 2021)
  def this() = this("Toyota", "Corolla", 2021)
}

@main
def main(): Unit = {
  println("Hello world!")
   val car = new Car("Ford", "Fiesta")
   println(car.year)
}