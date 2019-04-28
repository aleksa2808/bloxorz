package bloxorz

trait Solver extends GameDef {
  def done(b: Block): Boolean = b.b1 == goal && b.b2 == goal

  def neighborsWithHistory(
      b: Block,
      history: List[Move]
  ): Stream[(Block, List[Move])] = {
    def loop(
        legalNeighborList: List[(Block, Move)]
    ): Stream[(Block, List[Move])] = {
      if (legalNeighborList.isEmpty) Stream.empty
      else {
        val b = legalNeighborList.head._1
        val m = legalNeighborList.head._2
        (b, m :: history) #:: loop(legalNeighborList.tail)
      }
    }
    loop(b.legalNeighbors)
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
    if (initial.isEmpty) Stream.empty
    else {
      val more = newNeighborsOnly(
        neighborsWithHistory(initial.head._1, initial.head._2),
        explored
      )

      more #::: from(initial.tail #::: more, explored ++ (more map (_._1)))
    }
  }

  lazy val pathsFromStart: Stream[(Block, List[Move])] =
    from(Stream((startBlock, List())), Set())

  lazy val pathsToGoal: Stream[(Block, List[Move])] =
    pathsFromStart.filter(p => done(p._1))

  lazy val solution: List[Move] =
    if (pathsToGoal.isEmpty) List() else pathsToGoal.head._2.reverse
}
