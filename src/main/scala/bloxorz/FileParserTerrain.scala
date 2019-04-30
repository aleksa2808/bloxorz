package bloxorz

trait FileParserTerrain extends GameDef {
  val filePath: String

  val charMap = Map[Char, Field](
    'S' -> Start,
    'T' -> Goal,
    'o' -> Normal,
    '.' -> Weak,
    '-' -> Nil
  )

  def terrainFunction(levelVector: Vector[Vector[Char]]): Terrain = { pos =>
    pos match {
      case Pos(r, _) if !levelVector.isDefinedAt(r)    => Nil
      case Pos(r, c) if !levelVector(r).isDefinedAt(c) => Nil
      case Pos(r, c)                                   => charMap(levelVector(r)(c))
    }
  }

  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val y = levelVector.indexWhere(_.indexOf(c) > -1)
    val x = levelVector(y).indexOf(c)
    Pos(y, x)
  }

  private lazy val vector: Vector[Vector[Char]] = {
    val source = scala.io.Source.fromFile(filePath)
    try {
      val lines = source.getLines()
      lines.map(line => Vector(line: _*)).toVector
    } finally source.close()
  }
  assert(vector.forall(_.forall(c => charMap.isDefinedAt(c))))

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

  def printLevel(b: Block) = {
    val Block(b1, b2) = b

    def makeLine(r: Vector[Char], i: Int): String = {
      r.zipWithIndex.map {
        case (c, j) =>
          Pos(i, j) match {
            case p if b1 == p || b2 == p => 'B'
            case _                       => c
          }
      } mkString
    }

    val levelString =
      vector.zipWithIndex.map(x => makeLine _ tupled x).mkString("\n")

    println(levelString)
  }
}
