package bloxorz

sealed trait ParserTerrain extends GameDef {
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

  protected val vector: Vector[Vector[Char]]
  assert(vector.forall(_.forall(c => charMap.isDefinedAt(c))))

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

  def printLevel(b: Block) = {
    val Block(b1, b2) = b
    for (r <- 0 to vector.size - 1) {
      for (c <- 0 to vector(r).size - 1) {
        val here = Pos(r, c)
        b1 == here || b2 == here match {
          case true  => print('B')
          case false => print(vector(r)(c))
        }
      }
      print('\n')
    }
  }
}

trait StringParserTerrain extends ParserTerrain {
  val level: String

  protected final lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)
}

trait FileParserTerrain extends ParserTerrain {
  val filePath: String

  protected final lazy val vector: Vector[Vector[Char]] = {
    val source = scala.io.Source.fromFile(filePath)
    try {
      val lines = source.getLines()
      lines.map(line => Vector(line: _*)).toVector
    } finally source.close()
  }
}
