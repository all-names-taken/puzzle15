package test.puzzle15

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class Puzzle15Test extends FlatSpec
    with GeneratorDrivenPropertyChecks
    with Matchers {

  val allTiles = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0)

  "from" should s"fail with ${classOf[IllegalArgumentException]} if there are less tiles than required" in {
    val notEnoughTiles = for {
      count <- Gen.choose(0, 15)
      tiles <- Gen.pick(count, allTiles)
    } yield tiles

    forAll(notEnoughTiles)(t => assertThrows[IllegalArgumentException](Puzzle15.from(t)))
  }

  it should s"fail with ${classOf[IllegalArgumentException]} if there are duplicate tiles" in {
    val tilesWithDuplicates = for {
      maxTile <- Gen.choose(0, 14)
    } yield List.fill(16)(Random.nextInt(maxTile))

    forAll(tilesWithDuplicates)(t => assertThrows[IllegalArgumentException](Puzzle15.from(t)))
  }

  it should "succeed if correct set of tiles is passed" in {
    forAll(Gen.pick(16, allTiles))(t => Puzzle15.from(t))
  }
}
