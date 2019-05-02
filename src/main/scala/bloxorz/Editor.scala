package bloxorz

sealed abstract class EditAction
case object RemoveTile extends EditAction
case object AddTile extends EditAction
case object ReplaceNormalWithSpecial extends EditAction
case object ReplaceSpecialWithNormal extends EditAction
case object PlaceStartTile extends EditAction
case object PlaceGoalTile extends EditAction

class Edit(val pos: Pos, val action: EditAction)

class Editor {
  def edit(level: GameDef, getNextEdit: Edit): GameDef = {
    new AssignableTerrain(level.vector)
  }
}
