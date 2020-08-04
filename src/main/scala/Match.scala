import scala.annotation.tailrec

object Match {

  def intToName(num: Int): String = {
    num match {
      case 1 => "one"
      case 2 => "two"
      case _ => "other"
    }
  }

  def message(message: String): String = {
    message match {
      case "good" | "bad" => "game"
    }
  }

  def patternMatch(): Unit = {
    val seq = Seq("A", "B", "C", "D", "E")
    seq match {
      case Seq("A", b, c, d, e) if b != "B" =>

        println("b = " + b)
        println("c = " + c)
        println("d = " + d)
        println("e = " + e)
      case _ =>
        println("nothing")
    }
  }

  def patternMatchNest(): Unit = {
    val seq = Seq(Seq("A"), Seq("B", "C", "D", "E"))
    seq match {
      case Seq(a@Seq("A"), x) =>
        println(a)
        println(x)
      case _ => println("nothing")
    }
  }

  def patternMatchType(): Unit = {
    val obj: AnyRef = "String Literal"
    obj match {
      case v: java.lang.Integer => println("Integer!")
      case v: String => println(s"String! length is ${v.length}")
    }
  }

  def patternMatchGenerics(): Unit = {
    val obj: AnyRef = Seq("a", "b", "c")
    obj match {
      case v: Seq[_] => println(v)
      case _ => println("nothing")
    }
  }

  def printRandomChars(): Unit = {
    for (i <- 1 to 1000) {
      val chars: Seq[Char] = new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(5).toList
      val result = chars match {
        case Seq(a, b, c, d, _) => Seq(a, b, c, d, a)
      }
      println(result)
    }
  }

  @tailrec
  def last(seq: Seq[Int]): Int = {
    seq match {
      // こっちが自力で考えた方
      // case l :: ls if ls.isEmpty => l
      // case l :: ls if ls.nonEmpty => last(ls)

      // こっちが模範解答 (無駄なifがないのでシンプル)
      case Seq(x) => x
      case x :: xs => last(xs)
    }
  }

  def reverse(seq: Seq[Int]): Seq[Int] = {
    val last = seq.last
    seq match {
      // こっちが自力で考えた方
      // case l :: ls if ls.isEmpty => Seq(l)
      // case l :: ls if ls.nonEmpty => reverse(ls) :+ l

      // こっちが模範解答 (無駄なifがないのでシンプル)
      case Seq(x) => Seq(x)
      case x :: xs => reverse(xs) :+ x
    }
  }
}
