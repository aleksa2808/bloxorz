package bloxorz

object Bloxorz extends App {
  object InfiniteLevel extends Solver with InfiniteTerrain {
    val startPos = Pos(1, 3)
    val goal = Pos(5, 8)

    def printLevel(b: Block) = {}
  }

  println(InfiniteLevel.solution)

  abstract class Level extends Solver
  abstract class StringLevel extends Level with StringParserTerrain
  abstract class FileLevel extends Level with FileParserTerrain

  object Level0 extends StringLevel {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  println(Level0.solution)

  object Level1 extends StringLevel {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }

  println(Level1.solution)

  object Level2 extends FileLevel {
    val filePath = "/home/murtaugh/master/fp/level.txt"
  }

  println(Level2.solution)
}
