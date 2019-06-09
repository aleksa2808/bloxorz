package bloxorz

import scala.annotation.tailrec
import java.io.File

object ConsoleUI {
  class FileLevel(val filePath: String) extends GameDef with FileParserTerrain

  val blockChar = 'B'
  val fieldToCharMap = Map[Field, Char](
    Start -> 'S',
    Goal -> 'T',
    Normal -> 'o',
    Weak -> '.',
    Nil -> '-'
  )

  val actions =
    Map[String, GameDef => Unit](
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
          case Some(f) => f(chooseLevel())
          case None =>
            println("Invalid input")
        }
        mainMenu()
    }
  }

  def getListOfFiles(dir: File): List[File] =
    dir.listFiles.filter(_.isFile).toList

  def chooseLevel(): GameDef = {
    val levelFiles =
      getListOfFiles(new File("/home/murtaugh/master/fp/levels"))
        .sortBy(f => f.getName())

    def printAvailableLevels() = {
      println("Choose level:")
      levelFiles.foreach(f => println("  " + f.getName()))
    }

    printAvailableLevels()

    @tailrec
    def getLevel(): GameDef = {
      val input = scala.io.StdIn.readLine()
      levelFiles.find(_.getName() == input) match {
        case Some(file) => {
          val levelFilePath = file.getCanonicalPath()
          new FileLevel(levelFilePath)
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

  def printLevel(level: GameDef)(block: Block) = {
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
  def getNextMove: Option[Move] =
    readOption("""|  a s w d / e for exit""".stripMargin) match {
      case "a" => Some(Left)
      case "s" => Some(Down)
      case "w" => Some(Up)
      case "d" => Some(Right)
      case "e" => None
      case _ => {
        println("Invalid action.")
        getNextMove
      }
    }

  def playLevel(level: GameDef) = {
    Player.play(level, printLevel(level), getNextMove) match {
      case Win  => println("You win!")
      case Lose => println("You lose!")
    }
  }

  def solveLevel(level: GameDef) = {
    val solver = new Solver(level)
    println(solver.solution)
  }

  def editLevel(level: GameDef) = {
    val editedLevel = Editor.doEdit(level, GlobalEdit(InverseLevel))

    playLevel(editedLevel)
  }

  def main(args: Array[String]) {
    mainMenu()
  }
}
