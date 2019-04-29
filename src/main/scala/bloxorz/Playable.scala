package bloxorz

sealed abstract class GameResult
case object Win extends GameResult
case object Lose extends GameResult

trait Playable extends GameDef {

  def done(b: Block): Boolean = b.b1 == goal && b.b2 == goal

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
    def loop(block: Block): Boolean = {
      printLevel(block)

      done(block) || block.isLegal && loop(getNextBlock(block))
    }
    loop(startBlock) match {
      case true  => Win
      case false => Lose
    }
  }
}
