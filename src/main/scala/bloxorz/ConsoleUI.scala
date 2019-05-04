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
    Map[String, () => Unit](
      "1" -> playLevel,
      "2" -> solveLevel,
      "3" -> editLevel
    )

  def readOption(options: String): String = {
    println("Please select one of the following:")
    println(options)
    scala.io.StdIn.readLine()
  }

  @tailrec
  def mainMenu(): Unit = {
    val options = """|  1 - play 
                     |  2 - solve
                     |  3 - edit
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
    val levelFiles =
      getListOfFiles(new File("/home/murtaugh/master/fp/levels"))
        .sortBy(f => f.getName())

    def printAvailableLevels() = {
      println("Choose level:")
      levelFiles.foreach(f => println("  " + f.getName()))
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

  @tailrec
  def getNextMove: Move =
    readOption("""|  a s w d""".stripMargin) match {
      case "a" => Left
      case "s" => Down
      case "w" => Up
      case "d" => Right
      case _ => {
        println("Invalid action.")
        getNextMove
      }
    }

  def playLevel() = {
    val level = chooseLevel()
    level.play(printLevel(level), getNextMove) match {
      case Win  => println("You win!")
      case Lose => println("You lose!")
    }
  }

  def solveLevel() = {
    val level = chooseLevel()
    val solver = new Solver(level)
    println(solver.solution)
  }

  def editLevel() = {
    val level = chooseLevel()
    val edit = Edit(level.startPos, PlaceGoalTile)
    val editedLevel = Editor.editLevel(level, edit)

    val editedLevelString =
      editedLevel.vector
        .map(_.map(f => fieldToCharMap(f)).mkString)
        .mkString("\n")

    println(editedLevelString)
  }

  def main(args: Array[String]) {
    mainMenu()
  }
}
