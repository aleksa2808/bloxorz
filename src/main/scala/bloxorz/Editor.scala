package bloxorz
import scala.annotation.tailrec

sealed abstract class EditAction
sealed abstract class LocalEditAction extends EditAction
sealed abstract class GlobalEditAction extends EditAction

case object RemoveTile extends LocalEditAction
case object AddTile extends LocalEditAction
case object ReplaceNormalWithWeak extends LocalEditAction
case object ReplaceWeakWithNormal extends LocalEditAction
case object PlaceStartTile extends LocalEditAction
case object PlaceGoalTile extends LocalEditAction

case object InverseLevel extends GlobalEditAction
case object SwitchWeak extends GlobalEditAction

sealed abstract class Edit(action: EditAction)
case class LocalEdit(pos: Pos, action: LocalEditAction) extends Edit(action)
case class GlobalEdit(action: GlobalEditAction) extends Edit(action)

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

  private def doEdit(level: GameDef, edit: Edit): GameDef = {
    val newVector = edit match {
      case LocalEdit(pos, action) => {
        require(pos.row >= 0 && pos.row < level.vector.size)
        require(pos.col >= 0 && pos.col < level.vector.head.size)

        action match {
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
      }
      case GlobalEdit(action) => {
        action match {
          case InverseLevel => {
            val blankStart =
              updatedVector(level.vector, level.startPos, Goal)
            updatedVector(blankStart, level.goal, Start)
          }
          case SwitchWeak => {
            level.vector.map(_.map(_ match {
              case Weak => Normal
              case f    => f
            }))
          }
        }
      }
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
        case Some(e) => loop(doEdit(level, e))
        case None =>
          level.isValidFormat match {
            case true => level
            case false => {
              println("Invalid level format, needs more editing")
              loop(level)
            }
          }
      }
    }
    loop(level)
  }
}
