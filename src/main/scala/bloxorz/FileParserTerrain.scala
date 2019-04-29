package bloxorz

trait FileParserTerrain extends GameDef {
  val filePath: String

  def terrainFunction(levelVector: Vector[Vector[Char]]): Terrain = { pos =>
    levelVector.isDefinedAt(pos.row) &&
    levelVector(pos.row).isDefinedAt(pos.col) &&
    levelVector(pos.row)(pos.col) != '-'
  }

  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val y = levelVector.indexWhere(_.indexOf(c) > -1)
    val x = levelVector(y).indexOf(c)
    Pos(y, x)
  }

  lazy val levelVector: Vector[Vector[Char]] = {
    val source = scala.io.Source.fromFile(filePath)
    try {
      val lines = source.getLines()
      lines.map(line => Vector(line: _*)).toVector
    } finally source.close()
  }

  lazy val terrain: Terrain = terrainFunction(levelVector)
  lazy val startPos: Pos = findChar('S', levelVector)
  lazy val goal: Pos = findChar('T', levelVector)
}
