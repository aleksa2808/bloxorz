package bloxorz
import scala.annotation.tailrec

sealed abstract class EditAction
case object RemoveTile extends EditAction
case object AddTile extends EditAction
case object ReplaceNormalWithWeak extends EditAction
case object ReplaceWeakWithNormal extends EditAction
case object PlaceStartTile extends EditAction
case object PlaceGoalTile extends EditAction

case class Edit(pos: Pos, action: EditAction)

object Editor {
  private def updatedVector(
      vector: Vector[Vector[Field]],
      pos: Pos,
      field: Field
  ) =
    vector.zipWithIndex.map {
      case (r, i) =>
        r.zipWithIndex.map {
          case (c, j) =>
            Pos(i, j) == pos match {
              case true  => field
              case false => c
            }
        }
    }

  def edit(level: GameDef, edit: Edit): GameDef = {
    val Edit(pos, action) = edit
    require(pos.row >= 0 && pos.row < level.vector.size)
    require(pos.col >= 0 && pos.col < level.vector.head.size)

    val newVector = action match {
      case RemoveTile =>
        level.terrain(pos) match {
          case Normal | Weak => updatedVector(level.vector, pos, Nil)
          case Nil           => level.vector
          case _ => {
            println("Cannot remove that tile")
            level.vector
          }
        }
      case AddTile =>
        level.terrain(pos) match {
          case Nil => updatedVector(level.vector, pos, Normal)
          case _ => {
            println("Cannot place a tile there")
            level.vector
          }
        }
      case ReplaceNormalWithWeak =>
        level.terrain(pos) match {
          case Normal => updatedVector(level.vector, pos, Weak)
          case _ => {
            println("Cannot place a weak tile there")
            level.vector
          }
        }
      case ReplaceWeakWithNormal =>
        level.terrain(pos) match {
          case Weak => updatedVector(level.vector, pos, Normal)
          case _ => {
            println("Cannot place a normal tile there")
            level.vector
          }
        }
      case PlaceStartTile =>
        val replacementTile = level.terrain(pos) match {
          case Goal => Goal
          case _    => Normal
        }
        val blankStart =
          updatedVector(level.vector, level.startPos, replacementTile)
        updatedVector(blankStart, pos, Start)
      case PlaceGoalTile =>
        val replacementTile = level.terrain(pos) match {
          case Start => Start
          case _     => Normal
        }
        val blankGoal =
          updatedVector(level.vector, level.goal, replacementTile)
        updatedVector(blankGoal, pos, Goal)
    }

    new AssignableTerrain(newVector)
  }

  def editLevel(
      level: GameDef,
      reportState: GameDef => Unit,
      getEditAction: () => Option[Edit]
  ): GameDef = {
    @tailrec
    def loop(level: GameDef): GameDef = {
      reportState(level)

      getEditAction() match {
        case Some(e) => loop(edit(level, e))
        case None    => level
      }
    }
    loop(level)
  }
}
