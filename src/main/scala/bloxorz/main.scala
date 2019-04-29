package bloxorz

object main {
  class Level(val filePath: String)
      extends Playable
      with Solver
      with FileParserTerrain

  val levelFilePath = "/home/murtaugh/master/fp/level.txt"
  val level = new Level(levelFilePath)

  val actions =
    Map[Int, () => Unit](1 -> handleOne, 2 -> handleTwo)

  def handleOne() = {
    level.play match {
      case Win  => println("You win!")
      case Lose => println("You lose!")
    }
  }

  def handleTwo() = {
    println(level.solution)
  }

  def readOption: Int = {
    println("""|Please select one of the following:
             |  1 - play 
             |  2 - solve
             |  0 - quit""".stripMargin)
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
    def inputStream: Stream[Int] = readOption #:: inputStream
    inputStream.takeWhile(_ != 0).map(x => menu(x)).toList
  }
}
