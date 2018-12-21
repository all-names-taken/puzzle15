package test.puzzle15

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Puzzle15 private(tiles: Long, blankRow: Int, blankCol: Int) {

  import Puzzle15.{bitsAfter, bitsPerTile}

  /**
    * @param row a tile's row, [0, [[Puzzle15.rowCount]]).
    * @param col a tile's column, [0, [[Puzzle15.columnCount]]).
    * @return a tile value if it is present at a position specified by a row and a column or 0 otherwise.
    */
  def tile(row: Int, col: Int): Int = (tiles >> bitsAfter(row, col)).toInt & 0xF

  /**
    * Attempts to slide the tile specified by a row and a column and returns a new puzzle state if
    * it is possible to slide the given tile.
    *
    * @param row a tile's row, [0, [[Puzzle15.rowCount]]).
    * @param col a tile's column, [0, [[Puzzle15.columnCount]]).
    * @return a new puzzle state if it is possible to slide the given tile.
    */
  def slide(row: Int, col: Int): Option[Puzzle15] = {
    val updatedTiles = if (row == blankRow)
      if (col > blankCol)
        slideLeft(isolateRow(row, blankCol + 1, col + 1))
      else
        slideRight(isolateRow(row, col, blankCol))
    else if (col == blankCol)
      if (row > blankRow)
        slideUp(isolateCol(col, blankRow + 1, row + 1))
      else
        slideDown(isolateCol(col, row, blankRow))
    else tiles

    if (updatedTiles == tiles) None else Some(new Puzzle15(updatedTiles, row, col))
  }

  /**
    * @return a number of rows the puzzle has.
    */
  val rowCount: Int = Puzzle15.rowCount

  /**
    * @return a number of columns the puzzle has.
    */
  val columnCount: Int = Puzzle15.colCount

  /**
    * @return `true` if the puzzle is solved.
    */
  def solved: Boolean = tiles == 0x123456789ABCDEF0L

  private def isolateRow(row: Int, fromCol: Int, toCol: Int) = {
    val mask = (1L << (toCol - fromCol) * bitsPerTile) - 1 << bitsAfter(row, toCol - 1)
    tiles & mask
  }

  private def slideLeft(rowTiles: Long) = tiles - rowTiles + (rowTiles << bitsPerTile)

  private def slideRight(rowTiles: Long) = tiles - rowTiles + (rowTiles >>> bitsPerTile)

  private def isolateCol(col: Int, fromRow: Int, toRow: Int) = {
    val mask = (fromRow until toRow).map(isolateRow(_, col, col + 1)).foldLeft(0L)((a, b) => a | b)
    tiles & mask
  }

  private def slideUp(colTiles: Long) = tiles - colTiles + (colTiles << bitsPerTile * columnCount)

  private def slideDown(colTiles: Long) = tiles - colTiles + (colTiles >>> bitsPerTile * columnCount)
}

object Puzzle15 {

  private val rowCount = 4
  private val colCount = 4
  private val bitsPerTile = 4

  def from(tiles: Seq[Int]): Puzzle15 = {
    require(tiles.toSet.size == rowCount * colCount)
    require(tiles.count(t => t < 0 || t > 15) == 0)

    val blankIndex = tiles.indexOf(0)

    from(tiles, asRow(blankIndex), asCol(blankIndex))
  }

  private def from(tiles: Seq[Int], blankRow: Int, blankCol: Int): Puzzle15 = {
    val condensedTiles = tiles.foldLeft(0L)((a, t) => a << bitsPerTile | t)
    new Puzzle15(condensedTiles, blankRow, blankCol)
  }

  def shuffled(): Puzzle15 = {

    def shuffledTiles() = {
      val tiles = ArrayBuffer[Int]()
      tiles.insertAll(0, 0 until rowCount * colCount)
      Random.shuffle(tiles)
    }

    // A naive approach with a quadratic complexity.
    def inversions(tiles: IndexedSeq[Int]) =
      (for {
        i <- tiles.indices
        j <- i until tiles.size
      } yield if (tiles(j) != 0 && tiles(i) > tiles(j)) 1 else 0).sum

    def makeSolvable(tiles: ArrayBuffer[Int], blankTileIndex: Int): Unit = {
      def swap(tiles: ArrayBuffer[Int], i: Int, j: Int): Unit = {
        val ti = tiles(i)
        tiles(i) = tiles(j)
        tiles(j) = ti
      }

      if (blankTileIndex > 1) swap(tiles, 0, 1) else swap(tiles, 2, 3)
    }

    /*

    If the width is odd, then every solvable state has an even number of inversions.
    If the width is even, then every solvable state has
        an even number of inversions if the blank is on an odd numbered row counting from the bottom;
        an odd number of inversions if the blank is on an even numbered row counting from the bottom;

    (https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html)

     */

    val tiles = shuffledTiles()
    val blankIndex = tiles.indexOf(0)
    val blankRow = asRow(blankIndex)
    val blankCol = asCol(blankIndex)
    val invs = inversions(tiles)

    if (invs % 2 == 0) {
      if (blankRow % 2 == 0) makeSolvable(tiles, blankIndex)
    } else {
      if (blankRow % 2 != 0) makeSolvable(tiles, blankIndex)
    }

    from(tiles, blankRow, blankCol)
  }

  private def bitsBefore(row: Int, col: Int) = {
    assert(row < rowCount)
    assert(col < colCount)

    bitsPerTile * (row * colCount + col)
  }

  private def bitsAfter(row: Int, col: Int) = bitsPerTile * (rowCount * colCount - 1) - bitsBefore(row, col)

  private def asRow(index: Int): Int = index / colCount

  private def asCol(index: Int): Int = index % colCount
}