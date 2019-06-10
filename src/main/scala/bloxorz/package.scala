import java.io.File

package object bloxorz {
  type Terrain = Pos => Field

  val levelDirPath = "/home/murtaugh/master/fp/levels"
  val levelNameFileMap =
    new File(levelDirPath).listFiles
      .filter(_.isFile)
      .toList
      .sortBy(f => f.getName())
      .map(f => f.getName() -> f.getCanonicalPath())
      .toMap

  val levelNames = levelNameFileMap.keySet.toList.sorted
  assert(!levelNames.isEmpty)
}
