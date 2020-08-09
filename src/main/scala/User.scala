class User(private val name: String, private val age: Int) {
  override def toString = s"User($name, $age)"
}

object User {
  def apply(str: String): User = {
    val inputSeq = str.split(",").toSeq
    inputSeq match {
      case Seq(name, age) if (age.toIntOption.isDefined) =>
        new User(name, age.toInt)
      case _ =>
        println("ERROR:\"[name],[age]\"の形式で入力してください。")
        null
    }
  }

  def printAge(user: User): Unit = println(user.age)
}

