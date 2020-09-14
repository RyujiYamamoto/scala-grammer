trait Adder {
  def add(a: Double, b: Double): Double = {
    a + b
  }
}

trait Subtractor {
  def subtract(a: Double, b: Double): Double = {
    a - b
  }
}

trait Multiplier {
  def multiply(a: Double, b: Double): Double = {
    a * b
  }
}

trait Divider {
  def divide(a: Double, b: Double): Double = {
    a / b
  }
}

class Calculator extends Adder with Subtractor with Multiplier with Divider {

}
