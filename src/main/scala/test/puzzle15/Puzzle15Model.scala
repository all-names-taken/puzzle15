package test.puzzle15

class Puzzle15Model(val states: List[Puzzle15], val moves: Int = 0, val message: Option[String] = None) {

  require(states.nonEmpty)

  def slide(tile: Int): Puzzle15Model = {
    def tilePos(tile: Int, puzzle: Puzzle15): (Int, Int) = (for {
      row <- 0 until puzzle.rowCount
      col <- 0 until puzzle.columnCount if puzzle.tile(row, col) == tile
    } yield (row, col)).head

    val puzzle = states.head
    val (tileRow, tileCol) = tilePos(tile, puzzle)

    puzzle.slide(tileRow, tileCol)
        .map(p => new Puzzle15Model(p :: states, moves + 1))
        .getOrElse(new Puzzle15Model(states, moves, Some(s"Can't move tile $tile")))
  }

  def solved: Boolean = states.head.solved

  def undo(): Puzzle15Model = if (states.tail.isEmpty) this else new Puzzle15Model(states.tail, moves - 1)

  def reset(): Puzzle15Model = new Puzzle15Model(List(Puzzle15.shuffled()), 0, Some("The game was reset"))
}
