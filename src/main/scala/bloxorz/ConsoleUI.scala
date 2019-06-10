package bloxorz

import scala.annotation.tailrec
import java.io.File
import scala.collection.immutable.ListMap
import scala.util.Try

object ConsoleUI {
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
    val option = scala.io.StdIn.readLine()
    print("\033[2J")
    option
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

  def chooseLevel(): GameDef = {
    def printAvailableLevels() = {
      println("Choose level:")
      levelNames.foreach(n => println("  " + n))
    }
    printAvailableLevels()

    @tailrec
    def getLevel(): GameDef = {
      val input = scala.io.StdIn.readLine()
      levelNames.find(_ == input) match {
        case Some(lname) => {
          new FileLevel(levelNameFileMap(lname))
        }
        case None => {
          println("Invalid input")
          printAvailableLevels()
          getLevel()
        }
      }
    }
    val level = getLevel()
    print("\033[2J")
    level
  }

  def printBoard(level: GameDef, printFunc: (Field, Pos) => Unit) = {
    for (r <- 0 until level.vector.size) {
      for (c <- 0 until level.vector(r).size) {
        printFunc(level.vector(r)(c), Pos(r, c))
      }
      print('\n')
    }
  }

  def printLevel(level: GameDef)(block: Block) = {
    val Block(b1, b2) = block
    printBoard(level, (f, p) => {
      b1 == p || b2 == p match {
        case true  => print(blockChar)
        case false => print(fieldToCharMap(f))
      }
    })
  }

  def getNextMove: Option[Move] = {
    @tailrec
    def getMove(): Option[Move] =
      readOption("""|  a s w d / e for exit""".stripMargin) match {
        case "a" => Some(Left)
        case "s" => Some(Down)
        case "w" => Some(Up)
        case "d" => Some(Right)
        case "e" => None
        case _ => {
          println("Invalid action.")
          getMove
        }
      }

    val move = getMove()
    print("\033[2J")
    move
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

  def chooseEditAction(): Option[EditAction] = {
    val editActions = ListMap[String, EditAction](
      "Remove tile" -> RemoveTile,
      "Add tile" -> AddTile,
      "Set normal to weak" -> ReplaceNormalWithWeak,
      "Set weak to normal" -> ReplaceWeakWithNormal,
      "Place start tile" -> PlaceStartTile,
      "Place goal tile" -> PlaceGoalTile,
      "Inverse start and goal" -> InverseLevel,
      "Normalise weak tiles" -> SwitchWeak
    )
    val editActionNames = editActions.keySet.toList

    def printAvailableActions() = {
      println("Choose edit action:")
      editActionNames.zipWithIndex.foreach {
        case (as, i) => println(i + ". " + as)
      }
      println("f. Finish editing")
    }
    printAvailableActions()

    @tailrec
    def getAction(): Option[EditAction] = {
      val input = scala.io.StdIn.readLine()
      input match {
        case "f" => None
        case _ => {
          val option = Try(editActionNames(input.toInt)).toOption
          option match {
            case Some(as) => Some(editActions(as))
            case None => {
              println("Invalid input")
              printAvailableActions()
              getAction()
            }
          }
        }
      }
    }
    getAction()
  }

  def getNextEdit(): Option[Edit] = {
    @tailrec
    def readNum(): Int = {
      val input = scala.io.StdIn.readLine()
      val num = Try(input.toInt).toOption
      num match {
        case Some(n) => n
        case None => {
          println("Invalid input")
          readNum()
        }
      }
    }

    def getPos(): Pos = {
      println("Row:")
      val r = readNum()
      println("Column:")
      val c = readNum()
      Pos(r, c)
    }

    val action = chooseEditAction() match {
      case Some(a) =>
        a match {
          case la: LocalEditAction => {
            val pos = getPos()
            Some(LocalEdit(pos, la))
          }
          case ga: GlobalEditAction => Some(GlobalEdit(ga))
        }
      case None => None
    }
    print("\033[2J")
    action
  }

  def editLevel(level: GameDef) = {
    val editedLevel = Editor.editLevel(level, modifiedLevel => {
      printBoard(modifiedLevel, (f, _) => print(fieldToCharMap(f)))
    }, getNextEdit)

    playLevel(editedLevel)
  }

  def main(args: Array[String]) {
    print("\033[2J")
    mainMenu()
  }
}
