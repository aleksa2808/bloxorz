package bloxorz

import java.io.File
import scala.annotation.tailrec

object main {
  class Level(val filePath: String)
      extends Playable
      with Solver
      with FileParserTerrain

  val actions =
    Map[Int, () => Unit](1 -> playLevel, 2 -> solveLevel)

  def readOption: Int = {
    println("""|Please select one of the following:
               |  1 - play 
               |  2 - solve
               |  0 - quit""".stripMargin)
    scala.io.StdIn.readInt()
  }

  @tailrec
  def mainMenu: Unit = {
    readOption match {
      case 0 =>
      case option =>
        actions.get(option) match {
          case Some(f) => f()
          case None =>
            println("Invalid input")
        }
        mainMenu
    }
  }

  def getListOfFiles(dir: File): List[File] =
    dir.listFiles.filter(_.isFile).toList

  def chooseLevel(): Level = {
    val levelFiles = getListOfFiles(new File("/home/murtaugh/master/fp/levels"))

    def printAvailableLevels = {
      println("Choose level:")
      levelFiles
        .sortBy(f => f.getName())
        .foreach(f => println("  " + f.getName()))
    }

    printAvailableLevels

    @tailrec
    def getLevel: Level = {
      val input = scala.io.StdIn.readLine()
      levelFiles.find(_.getName() == input) match {
        case Some(file) => {
          val levelFilePath = file.getCanonicalPath()
          new Level(levelFilePath)
        }
        case None => {
          println("Invalid input")
          printAvailableLevels
          getLevel
        }
      }
    }
    getLevel
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

  def main(args: Array[String]) {
    mainMenu
  }
}
