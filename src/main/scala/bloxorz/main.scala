package bloxorz

object main {
  object Level1 extends Playable with FileParserTerrain {
    val filePath = "/home/murtaugh/master/fp/level.txt"
  }

  val actions =
    Map[Int, () => Unit](1 -> handleOne, 2 -> handleTwo)

  def handleOne() = {
    Level1.play match {
      case true  => println("You win!")
      case false => println("You lose!")
    }
  }

  def handleTwo() = {
    println("selected 2")
    true
  }

  def readOption: Int = {
    println("""|Please select one of the following:
             |  1 - play 
             |  2 - two
             |  3 - quit""".stripMargin)
    scala.io.StdIn.readInt()
  }

  def menu(option: Int) = {
    actions.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
    }
  }

  def main(args: Array[String]) {
    val quitOption = 3
    def inputStream: Stream[Int] = readOption #:: inputStream
    inputStream.takeWhile(_ != quitOption).map(x => menu(x)).toList
  }
}
