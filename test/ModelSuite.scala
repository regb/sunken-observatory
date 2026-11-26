package observatory
package model

import org.scalatest.funsuite.AnyFunSuite

class ModelSuite extends AnyFunSuite {

  private def play(game: Game, turns: Int, choices: Rng): Unit = {
    var left = turns
    while(left > 0 && game.outcome == Outcome.Playing) {
      left -= 1
      val command = Bot.next(game, choices)
      assert(game.perform(command), command)
    }
  }

  test("every floor can be walked all over") {
    for(seed <- 0 until 60; depth <- 1 to Biome.LastDepth) {
      var id = 0
      val level = Generator.level(depth, new Rng(seed * 31L + depth), () => { id += 1; id })
      def closed(p: Pos) = level(p) == Tile.LockedDoor || level(p) == Tile.SecretDoor
      val reach = Paths.distances(level.width, level.height, level.start)(p => level.walkable(p) || closed(p))
      def reached(p: Pos) = reach(level.index(p)) >= 0
      // Closed doors only close off what is optional.
      val needed = level.reachable.toSet
      assert(needed(level.exit) && level.items.keySet.count(!needed(_)) <= 6)
      assert(level.traps.keySet.forall(needed) && level.monsters.forall(m => needed(m.pos)))
      if(level.positions.exists(level(_) == Tile.LockedDoor))
        assert(level.items.exists((p, items) => needed(p) && items.exists(_.kind == ItemKind.Key)))
      val walkable = level.positions.filter(level.walkable).toVector
      assert(walkable.size > 150, (seed, depth))
      assert(walkable.forall(reached), (seed, depth))
      assert(level.positions.count(level(_) == Tile.Stairs) == (if(depth < Biome.LastDepth) 1 else 0))
      assert(level.monsters.forall(m => level.walkable(m.pos) && m.pos.distance(level.start) > 6))
      assert(level.monsters.map(_.pos).distinct.size == level.monsters.size)
      // Nothing open on the border, so that neighbors are always on the map.
      assert(walkable.forall(p => p.x > 0 && p.y > 0 && p.x < level.width - 1 && p.y < level.height - 1))
    }
  }

  test("what is seen is around, and a wall hides what is behind") {
    val room = Fov.compute(Pos(5, 5), 8)(p => p.x <= 2 || p.y <= 2 || p.x >= 8 || p.y >= 8)
    for(x <- 2 to 8; y <- 2 to 8) assert(room(Pos(x, y)), (x, y))
    assert(room.forall(p => p.x >= 2 && p.y >= 2 && p.x <= 8 && p.y <= 8))
    val wall = Fov.compute(Pos(0, 0), 6)(_ == Pos(2, 0))
    assert(wall(Pos(2, 0)) && !wall(Pos(4, 0)) && wall(Pos(0, 4)) && wall(Pos(-4, 0)))
  }

  test("a path avoids what is not open, and ends where asked") {
    def open(p: Pos) = p.x != 3 || p.y == 6
    val path = Paths.find(8, 8, Pos(0, 0), Pos(6, 0))(open)
    assert(path.last == Pos(6, 0) && path.contains(Pos(3, 6)))
    assert((Pos(0, 0) +: path).sliding(2).forall(step => step(0).distance(step(1)) == 1))
    assert(Paths.find(8, 8, Pos(0, 0), Pos(6, 0))(p => p.x < 3).isEmpty)
  }

  test("commands are read back as they were written") {
    val commands = Vector(Command.Step(-1, 1), Command.Wait, Command.PickUp, Command.Descend, Command.Ascend, Command.Search, Command.Ability, Command.Use(3), Command.Drop(0), Command.Throw(2, 10, 4), Command.Aim(1, 3, 7), Command.Scrap(4))
    assert(commands.map(c => Command.decode(Command.encode(c))) == commands.map(Some(_)))
    assert(Vector("", "x", "s1", "uA", "w1").forall(Command.decode(_).isEmpty))
  }

  test("there is no path into what cannot be walked on, nor to what was not seen") {
    val game = new Game(3L)
    val level = game.level
    val walls = level.positions.filter(p => level.explored(level.index(p)) && !level.walkable(p)).toVector
    val unseen = level.positions.filter(p => level.walkable(p) && !level.explored(level.index(p))).toVector
    assert(walls.nonEmpty && unseen.nonEmpty)
    assert((walls ++ unseen).forall(game.pathTo(_).isEmpty))
  }

  test("traps can always be walked around") {
    for(seed <- 0 until 60; depth <- 1 to Biome.LastDepth) {
      var id = 0
      val level = Generator.level(depth, new Rng(seed * 17L + depth), () => { id += 1; id })
      def closed(p: Pos) = level(p) == Tile.LockedDoor || level(p) == Tile.SecretDoor
      def open(p: Pos) = (level.walkable(p) || closed(p)) && !level.traps.contains(p)
      val reach = Paths.distances(level.width, level.height, level.start)(open)
      assert(level.traps.nonEmpty, (seed, depth))
      assert(level.positions.filter(open).forall(p => reach(level.index(p)) >= 0), (seed, depth))
    }
  }

  test("a save gives the same game back") {
    (0 until 10).foreach { seed =>
      val game = new Game(seed)
      game.player.maxHp = 400
      game.player.hp = 400
      play(game, 600, new Rng(seed + 100L))
      val restored = Game.restore(game.save).get
      assert(restored.save == game.save)
      def state(g: Game) = (
        g.player.pos, g.player.hp, g.depth, g.rank, g.experience, g.pack.toVector, g.instrument, g.outcome,
        g.level.monsters.map(m => (m.id, m.pos, m.hp, m.time)).toVector, g.level.fire.toVector, g.level.gas.toVector, g.visible
      )
      assert(state(restored) == state(game))
      // And goes on as it would have, the randomness included.
      play(game, 200, new Rng(seed + 5L))
      play(restored, 200, new Rng(seed + 5L))
      assert(restored.save == game.save)
    }
    val save = new Game(1L).save
    val values = save.split(" ").toVector
    val edited = values.updated(8, (values(8).toLong + 70).toString).mkString(" ")
    assert(edited != save)
    val broken = Vector("", "x", "2", "1 2 3", save.dropRight(1), edited, save + " 1", "9" + save.drop(1))
    assert(broken.forall(Game.restore(_).isEmpty))
  }

  test("a floor is as it was left, and the way out needs the Lodestar") {
    val game = new Game(6L)
    game.player.maxHp = 100000
    game.player.hp = 100000
    assert(game.level(game.player.pos) == Tile.StairsUp)
    assert(!game.perform(Command.Ascend))
    val choices = new Rng(2L)
    while(game.depth == 1) assert(game.perform(Bot.next(game, choices)))
    val first = game.floors(1)
    val explored = first.explored.toVector
    assert(game.player.pos == game.level.start)
    assert(game.perform(Command.Ascend))
    assert(game.depth == 1 && (game.level eq first) && game.player.pos == first.exit)
    assert(explored.zip(first.explored).forall { case (before, after) => !before || after })
    assert(game.perform(Command.Descend) && game.depth == 2)
  }

  /** A small empty room, to try things in. */
  private def room(calling: Calling): Game = {
    val game = new Game(1L, calling)
    val level = new Level(20, 12, Biome.Cistern)
    for(x <- 1 to 18; y <- 1 to 10) level(Pos(x, y)) = Tile.Floor
    level.start = Pos(1, 1)
    level.exit = Pos(18, 10)
    game.floors(1) = level
    game.level = level
    game.player.pos = Pos(9, 5)
    game
  }

  test("the lamp burns down, and sight with it") {
    def after(calling: Calling): Game = {
      val game = room(calling)
      game.oil = 300
      (0 until 250).foreach(_ => assert(game.perform(Command.Wait)))
      game
    }
    val surveyor = after(Calling.Surveyor)
    val lamplighter = after(Calling.Lamplighter)
    assert(surveyor.oil < 60 && surveyor.sight == 6)
    assert(lamplighter.oil > 160 && lamplighter.oil < 190)
    surveyor.oil = 0
    assert(surveyor.sight == 3)
    surveyor.pack += Item(ItemKind.Oil)
    assert(surveyor.perform(Command.Use(surveyor.pack.size - 1)) && surveyor.oil >= Tuning.OilFlask - 2 && surveyor.sight == 8)
  }

  test("a vault needs a key, which is spent") {
    val game = room(Calling.Diver)
    game.level(Pos(10, 5)) = Tile.LockedDoor
    assert(!game.perform(Command.Step(1, 0)))
    game.pack += Item(ItemKind.Key)
    assert(game.perform(Command.Step(1, 0)) && game.player.pos == Pos(9, 5))
    assert(game.level(Pos(10, 5)) == Tile.Door && !game.pack.exists(_.kind == ItemKind.Key))
    assert(game.perform(Command.Step(1, 0)) && game.player.pos == Pos(10, 5))
  }

  test("searching finds hidden doors and traps around, and takes time") {
    val game = room(Calling.Diver)
    game.level(Pos(11, 5)) = Tile.SecretDoor
    game.level(Pos(15, 5)) = Tile.SecretDoor
    game.level.traps(Pos(8, 6)) = PlacedTrap(Trap.Alarm, hidden = true)
    val before = game.player.time
    assert(game.perform(Command.Search))
    assert(game.player.time == before + 300)
    assert(game.level(Pos(11, 5)) == Tile.Door && game.level(Pos(15, 5)) == Tile.SecretDoor)
    assert(!game.level.traps(Pos(8, 6)).hidden)
  }

  test("the trader is paid, and only then") {
    val game = room(Calling.Surveyor)
    game.level.drop(game.player.pos, Item(ItemKind.Oil))
    game.level.wares(game.player.pos) = 15
    game.coins = 14
    assert(!game.perform(Command.PickUp))
    game.coins = 20
    assert(game.perform(Command.PickUp) && game.coins == 5 && game.pack.exists(_.kind == ItemKind.Oil))
    assert(game.level.wares.isEmpty && game.level.items.isEmpty)
    game.level.drop(game.player.pos, Item(ItemKind.Coins, count = 7))
    assert(game.perform(Command.PickUp) && game.coins == 12)
  }

  private def meet(game: Game, kind: MonsterKind, at: Pos): Actor = {
    val monster = new Actor(70 + game.level.monsters.size, Some(kind), at, kind.hp)
    monster.time = game.player.time + 1
    game.level.monsters += monster
    assert(game.perform(Command.Wait))
    monster
  }

  test("prisms strike what they are aimed at, and fill again") {
    def aimed(effect: PrismEffect, monsters: Pos*): (Game, Vector[Actor]) = {
      val game = room(Calling.Surveyor)
      game.pack.clear()
      game.pack += Item(ItemKind.Prism(effect), charge = 2)
      game.level.monsters ++= monsters.zipWithIndex.map { case (p, i) =>
        val monster = new Actor(50 + i, Some(MonsterKind.Guardian), p, 60)
        monster.time = 100000
        monster
      }
      assert(game.perform(Command.Wait))
      assert(game.perform(Command.Aim(0, monsters.head.x, monsters.head.y)) && game.pack(0).charge == 1)
      (game, game.level.monsters.toVector)
    }
    val (_, frozen) = aimed(PrismEffect.Frost, Pos(12, 5), Pos(14, 5))
    assert(frozen(0).hp < 60 && frozen(0).has(Status.Stilled) && frozen(1).hp == 60)
    val (_, pierced) = aimed(PrismEffect.Lance, Pos(12, 5), Pos(14, 5))
    assert(pierced.forall(_.hp < 60))
    val (tethered, traded) = aimed(PrismEffect.Tether, Pos(12, 5))
    assert(tethered.player.pos == Pos(12, 5) && traded(0).pos == Pos(9, 5))
    val (_, blown) = aimed(PrismEffect.Gust, Pos(12, 5))
    assert(blown(0).pos == Pos(16, 5) && blown(0).hp < 60)

    val (game, _) = aimed(PrismEffect.Frost, Pos(12, 5))
    assert(game.perform(Command.Aim(0, 12, 5)) && !game.perform(Command.Aim(0, 12, 5)))
    (0 until 85).foreach(_ => assert(game.perform(Command.Wait)))
    assert(game.pack(0).charge == 2)
  }

  test("cursed gear stays on until it is tempered, and etchings show once worn") {
    val game = room(Calling.Diver)
    game.pack.clear()
    game.pack += Item(ItemKind.instruments(2), etching = Some(Etching.Leaden))
    game.pack += Item(ItemKind.Chart(ChartEffect.Tempering))
    assert(!game.title(game.pack(0)).contains("leaden"))
    assert(game.perform(Command.Use(0)) && game.title(game.instrument).contains("leaden"))
    assert(!game.perform(Command.Use(0)))
    assert(game.perform(Command.Use(1)) && !game.instrument.cursed && game.instrument.upgrade == 1)
    assert(game.perform(Command.Use(0)))
  }

  test("a thief runs with what it took, and leaves it when it dies") {
    val game = room(Calling.Diver)
    game.player.maxHp = 1000
    game.player.hp = 1000
    game.pack.clear()
    game.pack += Item(ItemKind.Oil)
    val thief = meet(game, MonsterKind.Mudlark, Pos(10, 5))
    thief.maxHp = 1000
    thief.hp = 1000
    while(thief.loot.isEmpty) assert(game.perform(Command.Wait))
    assert(game.pack.isEmpty && thief.loot.exists(_.kind == ItemKind.Oil))
    (0 until 4).foreach(_ => game.perform(Command.Wait))
    assert(thief.pos.distance(game.player.pos) > 2)
    thief.hp = 1
    thief.statuses(Status.Burning) = 4
    while(thief.alive) assert(game.perform(Command.Wait))
    assert(game.level.items.valuesIterator.flatten.exists(_.kind == ItemKind.Oil))
  }

  test("mercury breaks in two, without gaining anything") {
    val game = room(Calling.Diver)
    game.player.maxHp = 1000
    game.player.hp = 1000
    val blob = meet(game, MonsterKind.Mercury, Pos(10, 5))
    var total = blob.hp
    while(game.level.monsters.size < 2) {
      val d = blob.pos - game.player.pos
      if(blob.pos.distance(game.player.pos) == 1) { val _ = game.perform(Command.Step(d.x, d.y)) } else { val _ = game.perform(Command.Wait) }
      assert(game.level.monsters.map(_.hp).sum <= total)
      total = game.level.monsters.map(_.hp).sum
    }
    assert(game.level.monsters.map(_.pos).distinct.size == game.level.monsters.size)
  }

  test("what the orrery aims at can be stepped out of") {
    def struck(stepOut: Boolean): Boolean = {
      val game = room(Calling.Diver)
      game.player.maxHp = 1000
      game.player.hp = 1000
      val engine = meet(game, MonsterKind.Engine, Pos(14, 5))
      var tries = 0
      while(engine.aim.isEmpty && tries < 20) {
        val _ = game.perform(Command.Wait)
        tries += 1
      }
      assert(engine.aim.contains(game.player.pos))
      val before = game.player.hp
      while(engine.aim.nonEmpty) { val _ = game.perform(if(stepOut) Command.Step(0, 1) else Command.Wait) }
      game.player.hp < before
    }
    assert(struck(stepOut = false) && !struck(stepOut = true))
  }

  test("a keeper shuts the stairs until it falls") {
    val game = room(Calling.Diver)
    game.level(game.player.pos) = Tile.Stairs
    val boss = meet(game, MonsterKind.Archivist, Pos(15, 9))
    boss.asleep = true
    assert(!game.perform(Command.Descend))
    game.level.monsters.clear()
    assert(game.perform(Command.Descend) && game.depth == 2)
    assert(Vector(2, 4, 6, 8).forall(Generator.bossAt(_).isDefined) && Vector(1, 3, 5, 7).forall(Generator.bossAt(_).isEmpty))
  }

  test("a keeper that lost the player goes back to its post") {
    val game = new Game(1L, Calling.Diver)
    val level = new Level(40, 12, Biome.Archive)
    for(x <- 1 to 38; y <- 1 to 10) level(Pos(x, y)) = Tile.Floor
    for(y <- 1 to 9) level(Pos(20, y)) = Tile.Wall
    level.start = Pos(1, 1)
    level.exit = Pos(35, 3)
    game.floors(1) = level
    game.level = level
    game.player.pos = Pos(3, 3)
    game.player.maxHp = 10000
    game.player.hp = 10000
    val boss = new Actor(60, Some(MonsterKind.Archivist), Pos(24, 8), 22)
    boss.time = game.player.time + 1
    boss.hunting = Some(Pos(25, 8))
    level.monsters += boss
    level.wanderers = Tuning.MaxWanderers
    (0 until 20).foreach(_ => assert(game.perform(Command.Wait)))
    assert(boss.pos == level.exit)
    (0 until 5).foreach(_ => assert(game.perform(Command.Wait)))
    assert(boss.pos == level.exit)
  }

  test("the tide comes and goes") {
    val level = new Level(20, 12, Biome.Cistern)
    for(x <- 1 to 18; y <- 1 to 10) level(Pos(x, y)) = Tile.Floor
    level(Pos(9, 5)) = Tile.Water
    def water = level.positions.count(level(_) == Tile.Water)
    (0 to 30).foreach(level.tide)
    assert(water == 13)
    (31 to 79).foreach(level.tide)
    assert(water == 1 && level.tidal.isEmpty)
  }

  test("the trader buys, for less") {
    val game = room(Calling.Surveyor)
    game.level(Pos(10, 5)) = Tile.Trader
    assert(game.perform(Command.Wait))
    val item = game.pack(0)
    assert(game.perform(Command.Drop(0)) && game.coins == game.sellPrice(item) && game.sellPrice(item) < Price.of(item))
    assert(game.level.items.isEmpty)
  }

  test("the Lodestar can always be taken, whatever lies there and however full the pack") {
    val game = room(Calling.Diver)
    game.pack.clear()
    (0 until game.PackSize).foreach(_ => game.pack += Item(ItemKind.OilCoat))
    game.level.drop(game.player.pos, Item(ItemKind.Coins, count = 3))
    game.level.drop(game.player.pos, Item(ItemKind.Lodestar))
    game.level.drop(game.player.pos, Item(ItemKind.Oil))
    assert(game.perform(Command.PickUp) && game.hasLodestar)
    assert(!game.perform(Command.PickUp) && game.pack.size == game.PackSize)
  }

  test("monsters come looking for who lingers, but not without end") {
    val game = room(Calling.Diver)
    game.player.maxHp = 100000
    game.player.hp = 100000
    // A wall to come from behind.
    val level = new Level(40, 12, Biome.Archive)
    for(x <- 1 to 38; y <- 1 to 10) level(Pos(x, y)) = Tile.Floor
    for(y <- 1 to 9) level(Pos(20, y)) = Tile.Wall
    level.start = Pos(1, 1)
    game.floors(1) = level
    game.level = level
    game.player.pos = Pos(3, 3)
    (0 until Tuning.WanderTurns * (Tuning.MaxWanderers + 4)).foreach(_ => assert(game.perform(Command.Wait)))
    assert(level.wanderers == Tuning.MaxWanderers)
  }

  test("gear is taken apart towards a chart of tempering") {
    val game = room(Calling.Diver)
    game.pack.clear()
    game.pack += Item(ItemKind.Oil)
    (0 until 3).foreach(_ => game.pack += Item(ItemKind.OilCoat))
    game.brass = Tuning.BrassForChart - 3
    assert(!game.perform(Command.Scrap(0)))
    (0 until 3).foreach(_ => assert(game.perform(Command.Scrap(1))))
    assert(game.brass == 0 && game.pack.map(_.kind).toVector == Vector(ItemKind.Oil, ItemKind.Chart(ChartEffect.Tempering)))
    assert(game.isKnown(ItemKind.Chart(ChartEffect.Tempering)))
  }

  test("every calling has an ability, which needs rest") {
    val diver = room(Calling.Diver)
    assert(!diver.perform(Command.Ability))
    diver.level(diver.player.pos) = Tile.Water
    assert(diver.perform(Command.Ability) && diver.player.has(Status.Submerged))
    assert(!diver.perform(Command.Ability) && diver.abilityWait > 0)
    assert(diver.perform(Command.Step(1, 0)) && !diver.player.has(Status.Submerged))

    val lamplighter = room(Calling.Lamplighter)
    val crab = new Actor(77, Some(MonsterKind.BrassCrab), Pos(12, 5), 8)
    crab.time = lamplighter.player.time + 50
    lamplighter.level.monsters += crab
    assert(lamplighter.perform(Command.Wait))
    val oil = lamplighter.oil
    assert(lamplighter.perform(Command.Ability) && crab.has(Status.Stilled) && lamplighter.oil <= oil - 60)

    val surveyor = room(Calling.Surveyor)
    surveyor.level(Pos(16, 5)) = Tile.SecretDoor
    assert(surveyor.perform(Command.Ability) && surveyor.level(Pos(16, 5)) == Tile.Door)
    assert(!surveyor.perform(Command.Ability))
  }

  test("fire spreads over reeds, not over water, and leaves ash") {
    val level = new Level(9, 3, Biome.Archive)
    for(x <- 1 to 7) level(Pos(x, 1)) = if(x == 5) Tile.Water else Tile.Reeds
    level.ignite(Pos(1, 1), 4)
    val rng = new Rng(1)
    (0 until 60).foreach(_ => level.tick(rng))
    assert((1 to 4).forall(x => level(Pos(x, 1)) == Tile.Ash))
    assert((6 to 7).forall(x => level(Pos(x, 1)) == Tile.Reeds))
    assert(level.fire.forall(_ == 0))
  }

  test("miasma drifts without appearing from nowhere") {
    val level = new Level(9, 9, Biome.Archive)
    for(x <- 1 to 7; y <- 1 to 7) level(Pos(x, y)) = Tile.Floor
    level.addGas(Pos(4, 4), 60)
    val rng = new Rng(1)
    var total = level.gas.sum
    (0 until 20).foreach { _ =>
      level.tick(rng)
      assert(level.gas.sum <= total)
      total = level.gas.sum
    }
    assert(level.gas.count(_ > 0) > 4 || total == 0)
  }
}
