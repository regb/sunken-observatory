package observatory
package model

/** Plays like someone careless: fights, takes everything, and goes down. Only
  * ever chooses what can be done.
  */
object Bot {
  def next(game: Game, choices: Rng): Command = {
    val level = game.level
    val player = game.player
    def toward(to: Pos): Option[Command] = game.pathTo(to).headOption.map { next =>
      val d = next - player.pos
      Command.Step(d.x, d.y)
    }
    def room(item: Item) = game.pack.size < game.PackSize || item.kind == ItemKind.Lodestar || item.kind == ItemKind.Coins
    def use(slot: Int): Option[Command] = if(slot >= 0) Some(Command.Use(slot)) else None
    def when(condition: Boolean)(command: => Option[Command]): Option[Command] = if(condition) command else None

    val better = game.pack.indexWhere { item =>
      (item.kind, game.instrument.kind, game.coat.kind) match {
        case (found: ItemKind.Instrument, held: ItemKind.Instrument, _) => found.max > held.max && !game.instrument.cursed
        case (found: ItemKind.Coat, _, worn: ItemKind.Coat) => found.armor > worn.armor && !game.coat.cursed
        case _ => false
      }
    }
    val target = game.visibleMonsters.sortBy(_.pos.distance(player.pos)).map(_.pos)
    def steps = Pos.Directions.filter(d => level.walkable(player.pos + d))

    def home = when(game.hasLodestar)(
      if(level(player.pos) == Tile.StairsUp) Some(Command.Ascend) else toward(level.start)
    )

    when(player.hp * 2 < player.maxHp)(use(game.pack.indexWhere(_.kind == ItemKind.Vial(VialEffect.Mending))))
      .orElse(when(game.oil < 150)(use(game.pack.indexWhere(_.kind == ItemKind.Oil))))
      .orElse(use(better))
      .orElse(when(choices.percent(12))(target.headOption.filter(_ => game.pack.nonEmpty).map(p => Command.Throw(choices.int(game.pack.size), p.x, p.y))))
      .orElse(when(choices.percent(40))(target.headOption.flatMap { p =>
        val prism = game.pack.indexWhere(item => item.kind.isInstanceOf[ItemKind.Prism] && item.charge > 0)
        if(prism >= 0) Some(Command.Aim(prism, p.x, p.y)) else None
      }))
      .orElse(target.headOption.flatMap(toward))
      .orElse(home)
      .orElse(when(level.items.contains(player.pos) && room(level.items(player.pos).head) && game.price(player.pos).forall(_ <= game.coins))(Some(Command.PickUp)))
      .orElse(when(game.pack.nonEmpty && choices.percent(4))(use(Some(choices.int(game.pack.size)).filter(i => usable(game.pack(i))).getOrElse(-1))))
      .orElse(when(choices.percent(2))(Some(Command.Search)))
      .orElse(when(choices.percent(2) && game.abilityWait == 0 && canUseAbility(game))(Some(Command.Ability)))
      .orElse(when(game.pack.exists(_.kind == ItemKind.Key))(
        level.positions.find(p => level(p) == Tile.LockedDoor && level.explored(level.index(p))).flatMap(toward)
      ))
      .orElse(when(choices.percent(90))(explore(game, toward)))
      .orElse(when(level(player.pos) == Tile.Stairs && !game.sealedOff)(Some(Command.Descend)))
      .orElse(when(steps.nonEmpty) {
        val d = choices.pick(steps)
        Some(Command.Step(d.x, d.y))
      })
      .getOrElse(Command.Wait)
  }

  private def usable(item: Item): Boolean = item.kind match {
    case ItemKind.Key | ItemKind.Coins | ItemKind.Lodestar => false
    case _: ItemKind.Prism | _: ItemKind.Instrument | _: ItemKind.Coat => false
    case _ => true
  }

  private def canUseAbility(game: Game): Boolean = game.calling match {
    case Calling.Surveyor => true
    case Calling.Diver => game.level(game.player.pos) == Tile.Water
    case Calling.Lamplighter => game.oil >= Tuning.FlareOil
  }

  private def explore(game: Game, toward: Pos => Option[Command]): Option[Command] = {
    val level = game.level
    def room(item: Item) = game.pack.size < game.PackSize || item.kind == ItemKind.Lodestar || item.kind == ItemKind.Coins
    def seen(p: Pos) = level.contains(p) && level.explored(level.index(p))
    def open(p: Pos) = seen(p) && level.walkable(p)
    val reach = Paths.distances(level.width, level.height, game.player.pos)(open)
    val reachable = level.positions.filter(p => p != game.player.pos && open(p) && reach(level.index(p)) > 0)
      .toVector.sortBy(p => reach(level.index(p)))
    def first(wanted: Pos => Boolean): Option[Command] =
      reachable.iterator.filter(wanted).map(toward).collectFirst { case Some(command) => command }
    first(p => level.items.get(p).exists(items => room(items.head)) && game.price(p).forall(_ <= game.coins))
      .orElse(first(_.neighbors.exists(n => !seen(n))))
      // The Curator waits at the end of the last floor.
      .orElse(if(game.sealedOff) toward(level.exit) else None)
      .orElse(if(level(game.player.pos) == Tile.Stairs) None else first(level(_) == Tile.Stairs))
  }
}
