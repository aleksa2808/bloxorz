package bloxorz

import java.io.File

object main {
  class Level(val filePath: String)
      extends Playable
      with Solver
      with FileParserTerrain

  val actions =
    Map[Int, () => Unit](1 -> playLevel, 2 -> solveLevel)

  def mainMenu(option: Int) = {
    actions.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
    }
  }

  def getListOfFiles(dir: File): List[File] =
    dir.listFiles.filter(_.isFile).toList

  def chooseLevel(): Level = {
    val levelFiles = getListOfFiles(new File("/home/murtaugh/master/fp/levels"))

    println("Choose level:")
    levelFiles
      .sortBy(f => f.getName())
      .foreach(f => println("  " + f.getName()))

    def loop: Level = {
      val input = scala.io.StdIn.readLine()
      levelFiles.find(_.getName() == input) match {
        case Some(file) => {
          val levelFilePath = file.getCanonicalPath()
          new Level(levelFilePath)
        }
        case None => {
          println("Invalid input")
          loop
        }
      }
    }
    loop
  }

  def playLevel() = {
    val level = chooseLevel()
    level.play match {
      case Win  => println("You win!")
      case Lose => println("You lose!")
    }
  }

  def solveLevel() = {
    val level = chooseLevel()
    println(level.solution)
  }

  def readOption: Int = {
    println("""|Please select one of the following:
             |  1 - play 
             |  2 - solve
             |  0 - quit""".stripMargin)
    scala.io.StdIn.readInt()
  }

  def main(args: Array[String]) {
    def inputStream: Stream[Int] = readOption #:: inputStream
    inputStream.takeWhile(_ != 0).foreach(x => mainMenu(x))
  }
}
