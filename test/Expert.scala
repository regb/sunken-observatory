package observatory
package model

/** Plays like someone who knows the game: rests, keeps vials for when they are
  * needed, and steps out of the way. Only knows what a player would.
  */
object Expert {
  def next(game: Game, choices: Rng): Command = {
    val level = game.level
    val player = game.player
    val health = player.hp.toDouble / player.maxHp
    val monsters = game.visibleMonsters.sortBy(m => (m.pos.distance(player.pos), m.id))
    val awake = monsters.filterNot(m => m.asleep || m.has(Status.Stilled))
    val adjacent = awake.filter(_.pos.distance(player.pos) == 1)
    val boss = monsters.exists(_.kind.exists(_.boss))

    def slot(wanted: Item => Boolean): Option[Int] = Some(game.pack.indexWhere(wanted)).filter(_ >= 0)
    def known(kind: ItemKind): Option[Int] = if(game.isKnown(kind)) slot(_.kind == kind) else None
    def vial(effect: VialEffect): Option[Int] = known(ItemKind.Vial(effect))
    def chart(effect: ChartEffect): Option[Int] = known(ItemKind.Chart(effect))
    def use(index: Option[Int]): Option[Command] = index.map(Command.Use(_))
    def when(condition: => Boolean)(command: => Option[Command]): Option[Command] = if(condition) command else None
    def toward(to: Pos): Option[Command] = game.pathTo(to).headOption.map(step)
    def step(to: Pos): Command = Command.Step(to.x - player.pos.x, to.y - player.pos.y)
    def throwAt(index: Option[Int], at: Pos): Option[Command] =
      index.filter(_ => at != player.pos && game.visible(at)).map(Command.Throw(_, at.x, at.y))

    def safe(p: Pos): Boolean = {
      level.walkable(p) && level.monsterAt(p).isEmpty && level.fire(level.index(p)) == 0 && level.gas(level.index(p)) == 0 &&
        !level.traps.get(p).exists(!_.hidden) && !level.monsters.exists(m => game.visible(m.pos) && m.aim.contains(p))
    }
    val inDanger = level.fire(level.index(player.pos)) > 0 || level.gas(level.index(player.pos)) > 0 ||
      monsters.exists(_.aim.contains(player.pos))

    def escape: Option[Command] = when(inDanger || (player.has(Status.Burning) && level(player.pos) != Tile.Water)) {
      val around = player.pos.neighbors.filter(safe)
      around.find(p => player.has(Status.Burning) && level(p) == Tile.Water)
        .orElse(if(inDanger) around.sortBy(p => -adjacent.map(_.pos.distance(p)).sum).headOption else None)
        .map(step)
    }

    def emergency: Option[Command] = when(health < 0.4 && (awake.nonEmpty || health < 0.25)) {
      use(vial(VialEffect.Mending))
        .orElse(when(adjacent.nonEmpty)(use(chart(ChartEffect.Stillness))))
        .orElse(when(adjacent.nonEmpty && !boss)(use(chart(ChartEffect.Passage))))
        .orElse(when(adjacent.nonEmpty)(use(chart(ChartEffect.Undertow))))
        .orElse(when(adjacent.nonEmpty)(use(vial(VialEffect.Quickening))))
        // Anything might help.
        .orElse(when(adjacent.nonEmpty && health < 0.25)(use(slot(i => i.kind.isInstanceOf[ItemKind.Vial] && !game.isKnown(i.kind)))))
    }

    def ability: Option[Command] = when(game.abilityWait == 0)(game.calling match {
      case Calling.Surveyor => when(level.explored.count(identity) < 250)(Some(Command.Ability))
      case Calling.Diver => when(level(player.pos) == Tile.Water && adjacent.nonEmpty)(Some(Command.Ability))
      case Calling.Lamplighter => when(game.oil >= 200 && (adjacent.size >= 2 || (boss && adjacent.nonEmpty) || (adjacent.nonEmpty && health < 0.5)))(Some(Command.Ability))
    })

    def ranged: Option[Command] = awake.headOption.flatMap { target =>
      val strong = target.hp > 8 || boss
      val far = target.pos.distance(player.pos)
      when(strong)(slot(i => i.charge > 0 && (i.kind == ItemKind.Prism(PrismEffect.Frost) || i.kind == ItemKind.Prism(PrismEffect.Lance)))
        .map(Command.Aim(_, target.pos.x, target.pos.y)))
        .orElse(when(strong && far >= 3 && level(target.pos) != Tile.Water)(throwAt(vial(VialEffect.Cinder), target.pos)))
        .orElse(when(strong && far >= 4)(throwAt(vial(VialEffect.Miasma), target.pos)))
    }

    def fight: Option[Command] =
      adjacent.sortBy(m => (m.hp, m.id)).headOption.map(m => step(m.pos))
        // What sleeps is struck first.
        .orElse(monsters.headOption.filter(m => m.pos.distance(player.pos) == 1).map(m => step(m.pos)))
        .orElse(awake.headOption.flatMap { m =>
          val shoots = m.kind.exists(k => k.behavior == Behavior.Ranged || k.behavior == Behavior.Kiter || k.behavior == Behavior.Beam)
          // Let the others come, which is worth the first blow.
          if(shoots || m.loot.nonEmpty) toward(m.pos) else if(m.pos.distance(player.pos) == 2) Some(Command.Wait) else toward(m.pos)
        })

    def score(item: Item): Double = item.kind match {
      case k: ItemKind.Instrument => (k.min + k.max + 3 * item.upgrade) * 50.0 / k.time
      case k: ItemKind.Coat => (k.armor + item.upgrade).toDouble
      case _ => 0
    }
    def worn(item: Item): Option[Item] = item.kind match {
      case _: ItemKind.Instrument => Some(game.instrument)
      case _: ItemKind.Coat => Some(game.coat)
      case _ => None
    }
    def isGear(item: Item): Boolean = worn(item).isDefined
    def improves(item: Item): Boolean =
      worn(item).exists(w => !w.cursed && !(item.worn && item.cursed) && score(item) > score(w))
    def better: Option[Command] =
      use(slot(improves)).orElse(when(game.lens.isEmpty)(use(slot(_.kind.isInstanceOf[ItemKind.Lens]))))

    def learn: Option[Command] = {
      val unknownChart = slot(i => i.kind.isInstanceOf[ItemKind.Chart] && !game.isKnown(i.kind))
      val unknownVial = slot(i => i.kind.isInstanceOf[ItemKind.Vial] && !game.isKnown(i.kind))
      when(health > 0.6)(use(unknownChart))
        .orElse(when(health > 0.8)(use(unknownVial)))
        .orElse(use(chart(ChartEffect.Tempering)))
        .orElse(use(vial(VialEffect.Vigor)))
        .orElse(when(level.explored.count(identity) < 200)(use(chart(ChartEffect.Atlas))))
    }

    def scrap: Option[Command] = slot(item => isGear(item) && !improves(item)).map(Command.Scrap(_))
    def rest: Option[Command] = when(
      (health < 0.75 || player.has(Status.Poisoned)) && game.oil > 250 && !game.hasLodestar
    )(Some(Command.Wait))
    def refill: Option[Command] = when(game.oil < 500)(use(slot(_.kind == ItemKind.Oil)))

    // Worth carrying, and affordable.
    def wants(p: Pos): Boolean = level.items.get(p).flatMap(_.headOption).exists { item =>
      val free = item.kind == ItemKind.Lodestar || item.kind == ItemKind.Coins
      val room = free || game.pack.size < game.PackSize || (item.kind.stacks && game.pack.exists(_.kind == item.kind))
      val spareLens = item.kind.isInstanceOf[ItemKind.Lens] && (game.lens.isDefined || game.pack.exists(_.kind.isInstanceOf[ItemKind.Lens]))
      room && !spareLens && (!isGear(item) || improves(item) || game.pack.size < 10) && game.price(p).forall(_ <= game.coins)
    }
    def take: Option[Command] = when(wants(player.pos))(Some(Command.PickUp))
    def home: Option[Command] = when(game.hasLodestar)(
      if(level(player.pos) == Tile.StairsUp) Some(Command.Ascend) else toward(level.start)
    )
    def explore: Option[Command] = {
      def seen(p: Pos) = level.contains(p) && level.explored(level.index(p))
      def open(p: Pos) = seen(p) && level.walkable(p)
      val reach = Paths.distances(level.width, level.height, player.pos)(open)
      lazy val reachable = level.positions.filter(p => p != player.pos && open(p) && reach(level.index(p)) > 0)
        .toVector.sortBy(p => reach(level.index(p)))
      def first(wanted: Pos => Boolean): Option[Command] =
        reachable.iterator.filter(wanted).map(toward).collectFirst { case Some(command) => command }
      first(wants)
        .orElse(when(slot(_.kind == ItemKind.Key).isDefined)(level.positions.find(p => level(p) == Tile.LockedDoor && seen(p)).flatMap(toward)))
        .orElse(first(_.neighbors.exists(n => !seen(n))))
    }
    // A keeper is met in good health.
    def onward: Option[Command] =
      if(game.sealedOff) when(health >= 0.95 || game.oil < 200)(toward(level.exit)).orElse(Some(Command.Wait))
      else if(level(player.pos) == Tile.Stairs) Some(Command.Descend)
      else level.positions.find(p => level(p) == Tile.Stairs && level.explored(level.index(p))).flatMap(toward)

    val calm = awake.isEmpty
    escape
      .orElse(emergency)
      .orElse(ability)
      .orElse(when(awake.nonEmpty)(ranged))
      .orElse(fight)
      // Now and then something else, so as not to go back and forth.
      .orElse(when(calm && choices.percent(3))(player.pos.neighbors.filter(safe) match {
        case around if around.nonEmpty => Some(step(choices.pick(around)))
        case _ => None
      }))
      .orElse(when(calm)(refill.orElse(better).orElse(scrap).orElse(learn).orElse(take).orElse(home).orElse(rest).orElse(explore).orElse(onward)))
      .orElse(player.pos.neighbors.filter(safe) match {
        case around if around.nonEmpty => Some(step(choices.pick(around)))
        case _ => None
      })
      .getOrElse(Command.Wait)
  }
}
