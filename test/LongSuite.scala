package observatory
package model

import org.scalatest.funsuite.AnyFunSuite

/** Whole games, many of them. */
class LongSuite extends AnyFunSuite {

  private def play(game: Game, turns: Int, choices: Rng): Unit = {
    var left = turns
    while(left > 0 && game.outcome == Outcome.Playing) {
      left -= 1
      val command = Bot.next(game, choices)
      assert(game.perform(command), command)
    }
  }

  test("a level does not lose anyone, and games end") {
    val games = (0 until 40).map { seed =>
      val game = new Game(seed, Calling.fromOrdinal(seed % 3))
      val choices = new Rng(seed * 7L)
      (0 until 60).foreach { _ =>
        play(game, 50, choices)
        val all = game.player +: game.level.monsters.toVector
        assert(all.forall(a => game.level.walkable(a.pos)), seed)
        assert(all.map(_.pos).distinct.size == all.size, seed)
        assert(game.level.monsters.forall(_.hp > 0) && game.player.hp <= game.player.maxHp, seed)
        assert(game.pack.size <= game.PackSize + 1 && game.pack.forall(_.count > 0), seed)
        assert(game.level.gas.forall(_ >= 0) && game.level.fire.forall(_ >= 0), seed)
        assert(game.outcome != Outcome.Playing || game.level.monsters.forall(_.time >= game.player.time), seed)
      }
      game
    }
    info("depths: " + games.map(_.depth).groupBy(identity).view.mapValues(_.size).toVector.sorted.mkString(" "))
    info("outcomes: " + games.map(_.outcome).groupBy(identity).view.mapValues(_.size).toMap)
    assert(games.count(_.outcome != Outcome.Playing) > 20)
    assert(games.map(_.depth).max >= 3)
  }

  test("someone strong enough finishes, whatever the seed, by walking as a tap does") {
    val unfinished = (0 until 40).filter { seed =>
      val game = new Game(seed, Calling.fromOrdinal(seed % 3))
      val choices = new Rng(seed + 3L)
      var turn = 0
      while(turn < 9000 && game.outcome == Outcome.Playing) {
        game.player.maxHp = 100000
        game.player.hp = 100000
        assert(game.perform(Bot.next(game, choices)))
        val _ = game.takeEvents()
        turn += 1
      }
      if(game.outcome != Outcome.Won) info(s"seed $seed: floor ${game.depth} at ${game.player.pos}, ${game.level.monsters.map(m => (m.title, m.pos))}")
      game.outcome != Outcome.Won
    }
    assert(unfinished.isEmpty)
  }
}
