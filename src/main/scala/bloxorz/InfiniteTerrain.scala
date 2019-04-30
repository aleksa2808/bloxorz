package bloxorz

trait InfiniteTerrain extends GameDef {
  val terrain: Terrain = pos =>
    pos match {
      case pos if pos == goal     => Goal
      case pos if pos == startPos => Start
      case _                      => Normal
    }
}
