package bloxorz
import scala.annotation.tailrec

sealed abstract class GameResult
case object Win extends GameResult
case object Lose extends GameResult

trait Playable extends GameDef {
  def readAction: Char = {
    println("""|Please select one of the following:
                 |  a s w d""".stripMargin)
    scala.io.StdIn.readChar()
  }

  def getNextBlock(block: Block) = readAction match {
    case 'a' => block.left
    case 's' => block.down
    case 'w' => block.up
    case 'd' => block.right
    case _ => {
      println("Invalid action.")
      block
    }
  }

  def play = {
    @tailrec
    def loop(block: Block): GameResult = {
      printLevel(block)

      block match {
        case block if done(block)   => Win
        case block if block.isLegal => loop(getNextBlock(block))
        case _                      => Lose
      }
    }
    loop(startBlock)
  }
}
