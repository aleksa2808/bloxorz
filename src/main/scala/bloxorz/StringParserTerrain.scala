package bloxorz

trait StringParserTerrain extends GameDef {
  val level: String

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

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}
