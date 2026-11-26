package observatory
package model

import scala.collection.mutable

/** Plays many games, and reports where and why they end. */
object Balance {
  private final class Run(val game: Game) {
    var cause = "unfinished"
    var illegal = 0
    /** Health, oil, rank and turn on arriving at a floor for the first time. */
    val arrivals = mutable.Map.empty[Int, (Double, Int, Int, Int)]
  }

  private def play(seed: Long, calling: Calling, bot: (Game, Rng) => Command, maxTurns: Int): Run = {
    val run = new Run(new Game(seed, calling))
    val game = run.game
    val choices = new Rng(seed * 31 + 7)
    def arrived() = if(!run.arrivals.contains(game.depth))
      run.arrivals(game.depth) = (game.player.hp.toDouble / game.player.maxHp, game.oil, game.rank, game.turns)
    arrived()
    var left = maxTurns
    while(left > 0 && game.outcome == Outcome.Playing) {
      left -= 1
      if(!game.perform(bot(game, choices))) {
        run.illegal += 1
        val _ = game.perform(Command.Wait)
      }
      game.takeEvents().foreach {
        case Event.Lost(cause) => run.cause = cause
        case Event.Won => run.cause = "won"
        case _ => ()
      }
      arrived()
    }
    run
  }

  def main(args: Array[String]): Unit = {
    val count = args.headOption.map(_.toInt).getOrElse(90)
    // Other seeds than those a change was made for.
    val from = args.lift(2).map(_.toLong).getOrElse(1000L)
    val bots = Vector[(String, (Game, Rng) => Command)]("careless" -> (Bot.next(_, _)), "expert" -> (Expert.next(_, _)))
      .filter(bot => args.length < 2 || args(1) == bot._1)
    bots.foreach { case (name, bot) =>
      val runs = (0 until count).map(i => play(from + i, Calling.fromOrdinal(i % 3), bot, 12000))
      println(s"== $name: $count games, ${runs.map(_.illegal).sum} commands that could not be done")
      def share(of: Int) = f"${of * 100.0 / count}%.0f%%"
      println(s"won ${share(runs.count(_.cause == "won"))}, unfinished ${share(runs.count(_.cause == "unfinished"))}, " +
        s"took the Lodestar ${share(runs.count(_.game.hasLodestar))}")
      Calling.values.foreach { calling =>
        val own = runs.filter(_.game.calling == calling)
        println(f"  ${calling.title}%-12s won ${own.count(_.cause == "won")} of ${own.size}, got to floor ${own.map(_.arrivals.keys.max).sum.toDouble / own.size}%.1f")
      }
      println("floor  reached  health   oil   rank   turns   died here")
      (1 to Biome.LastDepth).foreach { floor =>
        val there = runs.flatMap(_.arrivals.get(floor))
        if(there.nonEmpty) {
          def mean(f: ((Double, Int, Int, Int)) => Double) = there.map(f).sum / there.size
          val died = runs.count(r => r.game.outcome == Outcome.Lost && r.game.depth == floor)
          println(f"$floor%5d  ${share(there.size)}%7s  ${mean(_._1) * 100}%5.0f%%  ${mean(_._2.toDouble)}%4.0f  ${mean(_._3.toDouble)}%5.1f  ${mean(_._4.toDouble)}%6.0f  ${share(died)}%6s of all, ${died * 100 / there.size}%d%% of those")
        }
      }
      // Runs that go nowhere are a bot going in circles, or a game that cannot be finished.
      runs.filter(_.cause == "unfinished").foreach { run =>
        val game = run.game
        val level = game.level
        println(s"unfinished: seed ${game.seed}, ${game.calling.title}, floor ${game.depth}, lodestar ${game.hasLodestar}, oil ${game.oil}, " +
          s"at ${game.player.pos}, up ${level.start} in ${game.pathTo(level.start).size}, down ${level.exit} in ${game.pathTo(level.exit).size}, sealed ${game.sealedOff}")
        println("  sees " + game.visibleMonsters.map(m => (m.title, m.pos, m.asleep, m.loot.map(_.kind.title), game.pathTo(m.pos).size)))
        val choices = new Rng(1L)
        println("  " + (0 until 12).map { _ =>
          val command = Expert.next(game, choices)
          val _ = game.perform(command)
          Command.encode(command)
        }.mkString(" "))
      }
      println("ends: " + runs.groupBy(_.cause).view.mapValues(_.size).toVector.sortBy(-_._2).map((c, n) => s"$c $n").mkString(", "))
    }
  }
}
