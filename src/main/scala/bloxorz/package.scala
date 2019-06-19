import java.io.File

package object bloxorz {
  type Terrain = Pos => Field

  class FileLevel(val filePath: String) extends GameDef with FileParserTerrain

  val levelDirPath = "levels"
  val levelNameFileMap =
    new File(levelDirPath).listFiles
      .filter(_.isFile)
      .filter(f => new FileLevel(f.getCanonicalPath()).isValidFormat)
      .toList
      .sortBy(f => f.getName())
      .map(f => f.getName() -> f.getCanonicalPath())
      .toMap

  val levelNames = levelNameFileMap.keySet.toList.sorted
  assert(!levelNames.isEmpty)
}
