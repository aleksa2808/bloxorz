object main {
  val actions = Map[Int, () => Boolean](
    1 -> handleOne,
    2 -> handleTwo,
    3 -> handleThree)

  def handleOne(): Boolean = {
    println("selected 1")
    true
  }

  def handleTwo(): Boolean = {
    println("selected 2")
    true
  }

  def handleThree(): Boolean = {
    println("selected quit")
    false
  }

  def readOption: Int = {
    println("""|Please select one of the following:
             |  1 - one 
             |  2 - two
             |  3 - quit""".stripMargin)
    scala.io.StdIn.readInt()
  }

  def menu(option: Int): Boolean = {
    actions.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
        true
    }
  }

  def main(args: Array[String]) {
    while (menu(readOption)) {}
  }
}