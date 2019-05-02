package bloxorz

sealed trait ParserTerrain extends GameDef {
  def terrainFunction(levelVector: Vector[Vector[Field]]): Terrain = { pos =>
    pos match {
      case Pos(r, _) if !levelVector.isDefinedAt(r)    => Nil
      case Pos(r, c) if !levelVector(r).isDefinedAt(c) => Nil
      case Pos(r, c)                                   => levelVector(r)(c)
    }
  }

  def findField(f: Field, levelVector: Vector[Vector[Field]]): Pos = {
    val y = levelVector.indexWhere(_.indexOf(f) > -1)
    val x = levelVector(y).indexOf(f)
    Pos(y, x)
  }

  protected val vector: Vector[Vector[Field]]

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findField(Start, vector)
  lazy val goal: Pos = findField(Goal, vector)

  val blockChar = 'B'
  val fieldToCharMap = Map[Field, Char](
    Start -> 'S',
    Goal -> 'T',
    Normal -> 'o',
    Weak -> '.',
    Nil -> '-'
  )

  def printLevel(b: Block) = {
    val Block(b1, b2) = b
    for (r <- 0 to vector.size - 1) {
      for (c <- 0 to vector(r).size - 1) {
        val here = Pos(r, c)
        b1 == here || b2 == here match {
          case true  => print(blockChar)
          case false => print(fieldToCharMap(vector(r)(c)))
        }
      }
      print('\n')
    }
  }
}

trait StringParserTerrain extends ParserTerrain {
  def level: String

  def charMap = for ((k, v) <- fieldToCharMap) yield (v, k)

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

  protected final lazy val vector: Vector[Vector[Field]] =
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
