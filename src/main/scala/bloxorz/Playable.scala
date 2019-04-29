package bloxorz

trait Playable extends GameDef {
  def done(b: Block): Boolean = b.b1 == goal && b.b2 == goal

  def readAction: Char = {
    println("""|Please select one of the following:
                 |  a s w d""".stripMargin)
    scala.io.StdIn.readChar()
  }

  def play = {
    def loop(block: Block): Boolean = {
      printLevel(block)

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
