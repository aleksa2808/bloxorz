package bloxorz

case class Pos(row: Int, col: Int) {
  def deltaRow(d: Int): Pos = copy(row = row + d)
  def deltaCol(d: Int): Pos = copy(col = col + d)
}

sealed abstract class Field
case object Start extends Field
case object Goal extends Field
case object Normal extends Field
case object Weak extends Field
case object Nil extends Field

case class Block(b1: Pos, b2: Pos) {
  require(
    b1.row <= b2.row && b1.col <= b2.col,
    "Invalid block position: b1=" + b1 + ", b2=" + b2
  )

  def deltaRow(d1: Int, d2: Int) = Block(b1.deltaRow(d1), b2.deltaRow(d2))
  def deltaCol(d1: Int, d2: Int) = Block(b1.deltaCol(d1), b2.deltaCol(d2))

  def left = isStanding match {
    case true                      => deltaCol(-2, -1)
    case false if b1.row == b2.row => deltaCol(-1, -2)
    case _                         => deltaCol(-1, -1)
  }

  def right = isStanding match {
    case true                      => deltaCol(1, 2)
    case false if b1.row == b2.row => deltaCol(2, 1)
    case _                         => deltaCol(1, 1)
  }

  def up = isStanding match {
    case true                      => deltaRow(-2, -1)
    case false if b1.row == b2.row => deltaRow(-1, -1)
    case _                         => deltaRow(-1, -2)
  }

  def down = isStanding match {
    case true                      => deltaRow(1, 2)
    case false if b1.row == b2.row => deltaRow(1, 1)
    case _                         => deltaRow(2, 1)
  }

  def isStanding: Boolean = b1 == b2
  def isLegal(terrain: Terrain): Boolean = isStanding match {
    case true => {
      terrain(b1) match {
        case Weak => false
        case Nil  => false
        case _    => true
      }
    }
    case false => {
      terrain(b1) match {
        case Nil => false
        case _ =>
          terrain(b2) match {
            case Nil => false
            case _   => true
          }
      }
    }
  }
}

trait GameDef {
  private def terrainFunction(levelVector: Vector[Vector[Field]]): Terrain = {
    pos =>
      pos match {
        case Pos(r, _) if !levelVector.isDefinedAt(r)    => Nil
        case Pos(r, c) if !levelVector(r).isDefinedAt(c) => Nil
        case Pos(r, c)                                   => levelVector(r)(c)
      }
  }

  private def findUniqueField(
      f: Field,
      levelVector: Vector[Vector[Field]]
  ): Pos = {
    require(levelVector.count(_.indexOf(f) > -1) == 1)
    val y = levelVector.indexWhere(_.indexOf(f) > -1)
    val x = levelVector(y).indexOf(f)
    Pos(y, x)
  }

  val vector: Vector[Vector[Field]]

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findUniqueField(Start, vector)
  lazy val goal: Pos = findUniqueField(Goal, vector)

  def startBlock: Block = Block(startPos, startPos)

  def done(b: Block): Boolean = terrain(b.b1) match {
    case Goal if b.isStanding => true
    case _                    => false
  }
}
