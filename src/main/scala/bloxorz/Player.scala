package bloxorz
import scala.annotation.tailrec

sealed abstract class GameResult
case object Win extends GameResult
case object Lose extends GameResult

object Player {
  def play(
      level: GameDef,
      reportBlockState: Block => Unit,
      nextMove: => Move
  ) = {
    @tailrec
    def loop(block: Block): GameResult = {
      reportBlockState(block)

      block match {
        case block if level.done(block)            => Win
        case block if block.isLegal(level.terrain) => loop(block.move(nextMove))
        case _                                     => Lose
      }
    }
    loop(level.startBlock)
  }
}
