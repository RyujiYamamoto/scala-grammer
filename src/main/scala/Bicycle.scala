case class Course(id: Int)

case class Driver(name: String)

object NoDriver extends Driver("no-driver")

trait Ridable {
  def ride(driver: Driver): Unit
}

trait Runnable {
  def run(course: Course): Unit
}

class Bicycle extends Ridable with Runnable {
  private var driver: Driver = NoDriver

  override def ride(driver: Driver): Unit = {
    this.driver = driver
  }

  override def run(course: Course): Unit = {
    println(s"$driver finishes Course ${course.id}")
  }
}
