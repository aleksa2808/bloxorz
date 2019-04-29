package bloxorz
import scala.annotation.tailrec

object play {
  object PlayableLevel extends GameDef with FileParserTerrain {
    val filePath = "/home/murtaugh/master/fp/level.txt"

    def done(b: Block): Boolean = b.b1 == goal && b.b2 == goal

    def readAction: Char = {
      println("""|Please select one of the following:
                 |  a s w d""".stripMargin)
      scala.io.StdIn.readChar()
    }

    def play = {
      def loop(block: Block): Boolean = {
        val Block(b1, b2) = block

        for (r <- 0 to levelVector.size - 1) {
          for (c <- 0 to levelVector(r).size - 1) {
            val here = Pos(r, c)
            b1 == here || b2 == here match {
              case true  => print('B')
              case false => print(levelVector(r)(c))
            }
          }
          print('\n')
        }

        done(block) || block.isLegal && {
          readAction match {
            case 'a' => loop(block.left)
            case 's' => loop(block.down)
            case 'w' => loop(block.up)
            case 'd' => loop(block.right)
            case _ => {
              println("Invalid action.")
              loop(block)
            }
          }
        }
      }
      loop(startBlock)
    }
  }

  def main(args: Array[String]) {
    PlayableLevel.play match {
      case true  => println("You win!")
      case false => println("You lose!")
    }
  }
}
