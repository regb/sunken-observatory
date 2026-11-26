package observatory
package model

/** Writes games as text, and reads them back.
  *
  * A save is a list of numbers, the first being the version of the format and
  * the last one a checksum, so that what was cut or edited is not read.
  */
object Codec {
  private val Version = 7L

  private final class Writer {
    private val out = new StringBuilder
    private var sum = 17L
    def apply(value: Long): Unit = {
      sum = sum * 31 + value
      if(out.nonEmpty) out += ' '
      out ++= value.toString
    }
    def apply(value: Boolean): Unit = apply(if(value) 1L else 0L)
    def apply(p: Pos): Unit = {
      apply(p.x.toLong)
      apply(p.y.toLong)
    }
    def all[A](values: Iterable[A])(write: A => Unit): Unit = {
      apply(values.size.toLong)
      values.foreach(write)
    }
    /** Values that mostly repeat, as runs. */
    def runs(values: Array[Int]): Unit = {
      var i = 0
      while(i < values.length) {
        var end = i
        while(end < values.length && values(end) == values(i)) end += 1
        apply((end - i).toLong)
        apply(values(i).toLong)
        i = end
      }
    }
    def result: String = {
      val checksum = sum
      apply(checksum)
      out.toString
    }
  }

  private final class Invalid extends RuntimeException("Not a save")

  private final class Reader(text: String) {
    private val values: Array[Long] =
      try text.split(" ").map(_.toLong)
      catch { case _: NumberFormatException => throw new Invalid }
    if(values.length < 2 || values.init.foldLeft(17L)((sum, value) => sum * 31 + value) != values.last) throw new Invalid
    private var at = 0

    def long(): Long = {
      if(at >= values.length - 1) throw new Invalid
      at += 1
      values(at - 1)
    }
    def int(): Int = long().toInt
    def boolean(): Boolean = long() == 1L
    def pos(): Pos = { val x = int(); Pos(x, int()) }
    def all[A](read: => A): Vector[A] = Vector.fill(int())(read)
    def runs(length: Int): Array[Int] = {
      val result = new Array[Int](length)
      var i = 0
      while(i < length) {
        val count = int()
        val value = int()
        if(count <= 0 || i + count > length) throw new Invalid
        java.util.Arrays.fill(result, i, i + count, value)
        i += count
      }
      result
    }
    def pick[A](choices: IndexedSeq[A]): A = choices.lift(int()).getOrElse(throw new Invalid)
  }

  def write(game: Game): String = {
    val w = new Writer

    def item(value: Item): Unit = {
      w(ItemKind.all.indexOf(value.kind).toLong)
      w(value.upgrade.toLong)
      w(value.count.toLong)
      w(value.etching.map(_.ordinal).getOrElse(-1).toLong)
      w(value.charge.toLong)
      w(value.worn)
    }
    def actor(value: Actor): Unit = {
      w(value.id.toLong)
      w(value.kind.map(_.ordinal).getOrElse(-1).toLong)
      w(value.pos)
      w(value.maxHp.toLong)
      w(value.hp.toLong)
      w(value.time.toLong)
      w(value.asleep)
      w.all(value.hunting.toVector)(w(_))
      w(value.cooldown.toLong)
      w.all(value.loot.toVector)(item)
      w.all(value.aim)(w(_))
      w.all(value.statuses.toVector.sortBy(_._1.ordinal)) { case (status, turns) =>
        w(status.ordinal.toLong)
        w(turns.toLong)
      }
    }
    def floor(level: Level): Unit = {
      w(level.width.toLong)
      w(level.height.toLong)
      w(level.biome.ordinal.toLong)
      w.runs(level.positions.map(p => level(p).ordinal).toArray)
      w.runs(level.explored.map(if(_) 1 else 0))
      w.runs(level.fire)
      w.runs(level.gas)
      w(level.start)
      w(level.exit)
      w(level.leftAt.toLong)
      w(level.wanderers.toLong)
      w.all(level.traps.toVector.sortBy(entry => level.index(entry._1))) { case (p, trap) =>
        w(p)
        w(trap.kind.ordinal.toLong)
        w(trap.hidden)
      }
      w.all(level.items.toVector.sortBy(entry => level.index(entry._1))) { case (p, items) =>
        w(p)
        w.all(items)(item)
      }
      w.all(level.tidal.toVector.sortBy(level.index(_)))(w(_))
      w.all(level.wares.toVector.sortBy(entry => level.index(entry._1))) { case (p, price) =>
        w(p)
        w(price.toLong)
      }
      w.all(level.monsters)(actor)
    }

    w(Version)
    w(game.seed)
    w(game.calling.ordinal.toLong)
    w(game.rng.state)
    w(game.ids.toLong)
    actor(game.player)
    w(game.depth.toLong)
    w(game.experience.toLong)
    w(game.rank.toLong)
    w(game.outcome.ordinal.toLong)
    w(game.turns.toLong)
    w(game.hasLodestar)
    w(game.nextTick.toLong)
    w(game.oil.toLong)
    w(game.coins.toLong)
    w(game.brass.toLong)
    w(game.abilityReady.toLong)
    w.all(game.pack)(item)
    item(game.instrument)
    item(game.coat)
    w.all(game.lens.toVector)(item)
    w.all(game.known.toVector.map(ItemKind.all.indexOf(_)).sorted)(index => w(index.toLong))
    w.all(game.met.toVector.map(_.ordinal).sorted)(index => w(index.toLong))
    w.all(game.vialLooks)(look => w(ItemKind.VialLooks.indexOf(look).toLong))
    w.all(game.chartLooks)(look => w(ItemKind.ChartLooks.indexOf(look).toLong))
    w.all(game.floors.toVector.sortBy(_._1)) { case (depth, level) =>
      w(depth.toLong)
      floor(level)
    }
    w.result
  }

  def read(save: String): Option[Game] =
    try Some(read(new Reader(save)))
    catch { case _: Invalid => None }

  private def read(r: Reader): Game = {
    if(r.long() != Version) throw new Invalid

    def item(): Item = {
      val kind = r.pick(ItemKind.all)
      val upgrade = r.int()
      val count = r.int()
      val etching = r.int()
      val charge = r.int()
      Item(kind, upgrade, count, Etching.values.lift(etching), charge, r.boolean())
    }
    def fill(actor: Actor): Actor = {
      actor.hp = r.int()
      actor.time = r.int()
      actor.asleep = r.boolean()
      actor.hunting = r.all(r.pos()).headOption
      actor.cooldown = r.int()
      actor.loot = r.all(item()).headOption
      actor.aim = r.all(r.pos())
      r.all((r.pick(Status.values.toIndexedSeq), r.int())).foreach { case (status, turns) => actor.statuses(status) = turns }
      actor
    }
    def actor(): Actor = {
      val id = r.int()
      val kind = r.pick(MonsterKind.values.toIndexedSeq)
      val pos = r.pos()
      fill(new Actor(id, Some(kind), pos, r.int()))
    }
    def floor(): Level = {
      val width = r.int()
      val height = r.int()
      val level = new Level(width, height, r.pick(Biome.values.toIndexedSeq))
      val tiles = r.runs(width * height)
      val kinds = Tile.values
      level.positions.foreach(p => level(p) = kinds.lift(tiles(level.index(p))).getOrElse(throw new Invalid))
      r.runs(width * height).zipWithIndex.foreach { case (value, i) => level.explored(i) = value == 1 }
      r.runs(width * height).copyToArray(level.fire)
      r.runs(width * height).copyToArray(level.gas)
      level.start = r.pos()
      level.exit = r.pos()
      level.leftAt = r.int()
      level.wanderers = r.int()
      r.all((r.pos(), r.pick(Trap.values.toIndexedSeq), r.boolean())).foreach { case (p, kind, hidden) =>
        level.traps(p) = PlacedTrap(kind, hidden)
      }
      r.all((r.pos(), r.all(item()))).foreach { case (p, items) => level.items(p) = items.toList }
      level.tidal ++= r.all(r.pos())
      r.all((r.pos(), r.int())).foreach { case (p, price) => level.wares(p) = price }
      level.monsters ++= r.all(actor())
      level
    }

    val seed = r.long()
    val game = new Game(seed, r.pick(Calling.values.toIndexedSeq), fresh = false)
    game.rng.state = r.long()
    game.ids = r.int()
    r.int()
    r.int()
    game.player.pos = r.pos()
    game.player.maxHp = r.int()
    val _ = fill(game.player)
    game.depth = r.int()
    game.experience = r.int()
    game.rank = r.int()
    game.outcome = r.pick(Outcome.values.toIndexedSeq)
    game.turns = r.int()
    game.hasLodestar = r.boolean()
    game.nextTick = r.int()
    game.oil = r.int()
    game.coins = r.int()
    game.brass = r.int()
    game.abilityReady = r.int()
    game.pack ++= r.all(item())
    game.instrument = item()
    game.coat = item()
    game.lens = r.all(item()).headOption
    game.known ++= r.all(r.pick(ItemKind.all))
    game.met ++= r.all(r.pick(MonsterKind.values.toIndexedSeq))
    game.vialLooks = r.all(r.pick(ItemKind.VialLooks))
    game.chartLooks = r.all(r.pick(ItemKind.ChartLooks))
    r.all((r.int(), floor())).foreach { case (depth, level) => game.floors(depth) = level }
    game.level = game.floors.getOrElse(game.depth, throw new Invalid)
    game.look()
    game
  }
}
