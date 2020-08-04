object ControlSyntax {

  def bar(): String = {
    "bar"
  }

  def printOver18(age: Int): Unit = {
    val message = if (age < 18) {
      "18歳未満です"
    } else {
      "18歳以上です"
    }
    println(message)
  }

  def printWhileResult(): Unit = {
    var i = 0
    val result = while (i < 10) i = i + 1
    println(result)
  }

  def doubleLoop(): Unit = {
    for (x <- 1 to 5; y <- 1 until 5) {
      println("x = " + x + " y = " + y)
    }
  }

  def collectionLoop(): Unit =
    for(e <- Seq("A", "B", "C", "D", "E")) println(e)

  def generateCollection(): Seq[String] =
    for(e <- Seq("A", "B", "C", "D", "E")) yield "Pre" + e

  def printIsBaby(age: Int, isSchoolStarted: Boolean): Unit = {
    if(1 <= age && age <= 6 && !isSchoolStarted) println("幼児です") else println("幼児ではありません")
  }

  def loopFrom0To9(): Unit ={
    var i = 0
    do {
      println(s"i = $i")
      i = i + 1
    } while (i < 10)
  }

  def printRightTriangles(): Unit =
    for (a <- 1 to 1000;b <- 1 to 1000;c <- 1 to 1000 if a*a == b*b + c*c) println((a, b, c))
}
