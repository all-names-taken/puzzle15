package test.puzzle15

import scala.io.StdIn.readLine

object Puzzle15View {

  def interact(model: Puzzle15Model): String = {

    println(
      """ ~
          ~      │    ┌─────┘
          ~  ──  │    │
          ~      │    │
          ~      │   ─────  │
          ~      │          │
          ~      │          │
          ~    ──┘   ───────┘
          ~   · P U Z Z L E ·
          ~""".stripMargin('~')
    )

    printTiles(model.states.head)

    model.message.foreach{s =>
      println(s"$s")
      println("···················")
    }

    print(if (model.moves > 999) " :)  " else f"${model.moves}%3s  ")

    readLine("Tile/U/R/Q: ")
  }

  private def printTiles(puzzle: Puzzle15): Unit = {
    for (row <- 0 until puzzle.rowCount) {
      val rowTiles = 0 until puzzle.columnCount map (puzzle.tile(row, _))
      rowTiles map (t => if (t == 0) "     " else f" $t%2s │") foreach (t => print(t))
      println()
      rowTiles foreach (t => if (t == 0) print("     ") else print("────┘"))
      println()
    }
  }
}
