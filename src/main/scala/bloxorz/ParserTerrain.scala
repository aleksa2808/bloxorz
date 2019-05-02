package bloxorz

trait StringParserTerrain extends GameDef {
  def level: String

  def charMap = Map[Char, Field](
    'S' -> Start,
    'T' -> Goal,
    'o' -> Normal,
    '.' -> Weak,
    '-' -> Nil
  )
  def fieldToCharMap = for ((k, v) <- charMap) yield (v, k)

  def vector: Vector[Vector[Field]] =
    Vector(
      level.split("\n").map(str => Vector(str.map(c => charMap(c)): _*)): _*
    )
}

trait FileParserTerrain extends StringParserTerrain {
  def filePath: String

  override def level = {
    val source = scala.io.Source.fromFile(filePath)
    try {
      source.getLines().mkString("\n")
    } finally source.close()
  }
}
