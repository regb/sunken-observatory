package observatory
package model

import org.scalatest.funsuite.AnyFunSuite

class RecordsSuite extends AnyFunSuite {
  private def lost(depth: Int, rank: Int = 1) = Record(false, Calling.Diver, depth, rank, 100, 1L, "the Curator; of course")

  test("records are read back as they were written, and what is broken is left out") {
    val records = Vector(Record(true, Calling.Lamplighter, 1, 12, 4000, -5L, ""), lost(3))
    assert(Records.read(Records.write(records)) == records)
    assert(Records.read("") == Vector.empty)
    assert(Records.read("x\n" + Records.write(records) + "\n1;9;1;1;1;1;a\n1;2;b;1;1;1;a") == records)
  }

  test("the best are kept, the quickest of those that were won first") {
    val won = Record(true, Calling.Diver, 1, 10, 5000, 1L, "")
    val quicker = won.copy(turns = 3000)
    val records = Vector(lost(2), won, lost(7), quicker, lost(7, rank = 9)).foldLeft(Vector.empty[Record])(Records.add)
    assert(records == Vector(quicker, won, lost(7, rank = 9), lost(7), lost(2)))
    val many = (1 to 30).foldLeft(records)((all, i) => Records.add(all, lost(1 + i % 4)))
    assert(many.size == Records.Kept && many.take(3) == records.take(3))
  }

  test("a hint is told once, and the urgent ones first") {
    val game = new Game(4L)
    var told = Set.empty[Hint]
    var order = Vector.empty[Hint]
    while(Hint.due(game, told).isDefined) {
      val hint = Hint.due(game, told).get
      order :+= hint
      told += hint
    }
    assert(order.contains(Hint.Walk) && order.distinct == order)
    game.player.hp = 1
    game.oil = 10
    assert(Hint.due(game, Set(Hint.Walk) ++ told.filter(_ != Hint.Hurt)).contains(Hint.Hurt) || told(Hint.Hurt))
    game.hasLodestar = true
    assert(Hint.due(game, Set.empty).contains(Hint.Flood))
  }
}
