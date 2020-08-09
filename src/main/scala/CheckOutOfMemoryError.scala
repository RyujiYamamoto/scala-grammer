object CheckOutOfMemoryError extends App {

  Cache.causeOutOfMemoryError()
}

object Cache {
  var map: Map[Int, String] = Map(0 -> "")

  var str = ""

  def causeOutOfMemoryError(): Unit = {
    var i = 1
    while (i > 0) {
      str = str + s"i = ${i}"
      map = map + (i -> str)
      println(map(i))
      i += 1
    }
  }
}