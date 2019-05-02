package bloxorz
import scala.annotation.tailrec

sealed abstract class GameResult
case object Win extends GameResult
case object Lose extends GameResult

trait Playable extends GameDef {
  def play(reportBlockState: Block => Unit, nextBlockState: Block => Block) = {
    @tailrec
    def loop(block: Block): GameResult = {
      reportBlockState(block)

      block match {
        case block if done(block)            => Win
        case block if block.isLegal(terrain) => loop(nextBlockState(block))
        case _                               => Lose
      }
    }
    loop(startBlock)
  }
}
