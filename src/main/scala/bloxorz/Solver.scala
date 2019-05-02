package bloxorz

class Solver(game: GameDef) {
  def neighbors(block: Block): List[(Block, Move)] =
    List(Left, Right, Up, Down).map(m => (block.move(m), m))

  def legalNeighbors(block: Block): List[(Block, Move)] =
    neighbors(block).filter { case (b, m) => b.isLegal(game.terrain) }

  def neighborsWithHistory(
      b: Block,
      history: List[Move]
  ): Stream[(Block, List[Move])] = {
    def loop(
        legalNeighborList: List[(Block, Move)]
    ): Stream[(Block, List[Move])] = {
      legalNeighborList match {
        case List()       => Stream.empty
        case (b, m) :: ts => (b, m :: history) #:: loop(legalNeighborList.tail)
      }
    }
    loop(legalNeighbors(b))
  }

  def newNeighborsOnly(
      neighbors: Stream[(Block, List[Move])],
      explored: Set[Block]
  ): Stream[(Block, List[Move])] =
    neighbors.filter(n => !explored.contains(n._1))

  def from(
      initial: Stream[(Block, List[Move])],
      explored: Set[Block]
  ): Stream[(Block, List[Move])] = {
    initial match {
      case Stream() => Stream.empty
      case (b, m) #:: ts => {
        val more = newNeighborsOnly(
          neighborsWithHistory(b, m),
          explored
        )

        more #::: from(ts #::: more, explored ++ (more map (_._1)))
      }
    }
  }

  lazy val pathsFromStart: Stream[(Block, List[Move])] =
    from(Stream((game.startBlock, List())), Set())

  lazy val pathsToGoal: Stream[(Block, List[Move])] =
    pathsFromStart.filter(p => game.done(p._1))

  lazy val solution: Option[List[Move]] = !pathsToGoal.isEmpty match {
    case true  => Some(pathsToGoal.head._2.reverse)
    case false => None
  }
}
