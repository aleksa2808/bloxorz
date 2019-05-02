package bloxorz

trait StringParserTerrain extends GameDef {
  def level: String

  val charMap = Map[Char, Field](
    'S' -> Start,
    'T' -> Goal,
    'o' -> Normal,
    '.' -> Weak,
    '-' -> Nil
  )
  def fieldToCharMap = for ((k, v) <- charMap) yield (v, k)

  private object LevelFormatChecker {
    private val yMargin = 1
    private val xMargin = 2

    private def checkSize(rows: Array[String]): Boolean = {
      rows.size > 2 * yMargin + 1 &&
      rows.forall(r => r.size == rows.head.size) &&
      rows.head.size > 2 * xMargin + 1
    }

    private def checkHorizontalMargins(rows: Array[String]): Boolean = {
      rows.forall(
        r =>
          r.zipWithIndex.forall {
            case (c, i) if (i < xMargin) || (i >= r.size - xMargin) =>
              c == fieldToCharMap(Nil)
            case _ => true
          }
      )
    }

    private def checkVerticalMargins(rows: Array[String]): Boolean = {
      rows.zipWithIndex.forall {
        case (r, i) if (i < yMargin) || (i >= rows.size - yMargin) =>
          r.forall(_ == fieldToCharMap(Nil))
        case _ => true
      }
    }

    def check(level: => String): Boolean = {
      val rows = level.split("\n")

      checkSize(rows) &&
      checkHorizontalMargins(rows) &&
      checkVerticalMargins(rows)
    }
  }
  require(LevelFormatChecker.check(level))

  lazy val vector: Vector[Vector[Field]] =
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
