package bloxorz

import scala.annotation.tailrec
import java.io.File

object ConsoleUI {
  class Level(val filePath: String)
      extends GameDef
      with FileParserTerrain
      with Playable

  val blockChar = 'B'
  val fieldToCharMap = Map[Field, Char](
    Start -> 'S',
    Goal -> 'T',
    Normal -> 'o',
    Weak -> '.',
    Nil -> '-'
  )

  val actions =
    Map[String, () => Unit]("1" -> playLevel, "2" -> solveLevel)

  def readOption(options: String): String = {
    println("Please select one of the following:")
    println(options)
    scala.io.StdIn.readLine()
  }

  @tailrec
  def mainMenu(): Unit = {
    val options = """|  1 - play 
                     |  2 - solve
                     |  0 - quit""".stripMargin
    readOption(options) match {
      case "0" =>
      case option =>
        actions.get(option) match {
          case Some(f) => f()
          case None =>
            println("Invalid input")
        }
        mainMenu()
    }
  }

  def getListOfFiles(dir: File): List[File] =
    dir.listFiles.filter(_.isFile).toList

  def chooseLevel(): Level = {
    val levelFiles = getListOfFiles(new File("/home/murtaugh/master/fp/levels"))

    def printAvailableLevels() = {
      println("Choose level:")
      levelFiles
        .sortBy(f => f.getName())
        .foreach(f => println("  " + f.getName()))
    }

    printAvailableLevels()

    @tailrec
    def getLevel(): Level = {
      val input = scala.io.StdIn.readLine()
      levelFiles.find(_.getName() == input) match {
        case Some(file) => {
          val levelFilePath = file.getCanonicalPath()
          new Level(levelFilePath)
        }
        case None => {
          println("Invalid input")
          printAvailableLevels()
          getLevel()
        }
      }
    }
    getLevel()
  }

  def printLevel(level: Level)(block: Block) = {
    val Block(b1, b2) = block
    for (r <- 0 until level.vector.size) {
      for (c <- 0 until level.vector(r).size) {
        val here = Pos(r, c)
        b1 == here || b2 == here match {
          case true  => print(blockChar)
          case false => print(fieldToCharMap(level.vector(r)(c)))
        }
      }
      print('\n')
    }
  }

  def getNextBlock(block: Block) =
    readOption("""|  a s w d""".stripMargin) match {
      case "a" => block.left
      case "s" => block.down
      case "w" => block.up
      case "d" => block.right
      case _ => {
        println("Invalid action.")
        block
      }
    }

  def playLevel() = {
    val level = chooseLevel()
    level.play(printLevel(level), getNextBlock) match {
      case Win  => println("You win!")
      case Lose => println("You lose!")
    }
  }

  def solveLevel() = {
    val level = chooseLevel()
    println(level.solution)
  }

  def main(args: Array[String]) {
    mainMenu()
  }
}
