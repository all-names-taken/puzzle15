package test.puzzle15

import scala.util.Try

/**
  * Puzzle 15 main class.
  */
object Puzzle15Game {

  def main(args: Array[String]): Unit = {
    object TileNum {
      def unapply(s: String): Option[Int] = Try(s.toInt).toOption.filter(t => t > 0 && t <= 16)
    }

    var model = new Puzzle15Model(List(Puzzle15.shuffled()), 0)
    var quit = false

    while (!model.solved && !quit) {
      val input = Puzzle15View.interact(model)

      model = input.trim match {
        case TileNum(s) => model.slide(s.toInt)
        case "U" | "u" => model.undo()
        case "R" | "r" => model.reset()
        case "Q" | "q" => quit = true; model
        case _ => new Puzzle15Model(model.states, model.moves, Some(s"Bad input $input"))
      }
    }
  }
}
