package observatory
package model

import scala.collection.mutable

/** Makes the floors: rooms joined by corridors, then what is in them. */
object Generator {
  val Width: Int = 44
  val Height: Int = 32

  private final case class Room(x: Int, y: Int, w: Int, h: Int) {
    def center: Pos = Pos(x + w / 2, y + h / 2)
    def contains(p: Pos): Boolean = p.x >= x && p.y >= y && p.x < x + w && p.y < y + h
    def cells: Vector[Pos] = (for(cy <- y until y + h; cx <- x until x + w) yield Pos(cx, cy)).toVector
    def near(other: Room): Boolean =
      x - 2 < other.x + other.w && other.x - 2 < x + w && y - 2 < other.y + other.h && other.y - 2 < y + h
  }

  def level(depth: Int, rng: Rng, nextId: () => Int): Level = {
    val level = new Level(Width, Height, Biome.at(depth))
    val rooms = placeRooms(rng)
    rooms.foreach(_.cells.foreach(level(_) = Tile.Floor))
    connect(level, rooms, rng)
    placeDoors(level, rooms, rng)
    level.positions.foreach { p =>
      if(level(p) == Tile.Rock && p.neighbors.exists(level.walkable)) level(p) = Tile.Wall
    }

    level.start = rooms.head.center
    val distance = Paths.distances(Width, Height, level.start)(level.walkable)
    val last = rooms.tail.maxBy(room => distance(level.index(room.center)))
    val exit = last.center

    decorate(level, rooms, rng, keep = Set(level.start, exit))
    level.exit = exit
    level(level.start) = Tile.StairsUp
    if(depth < Biome.LastDepth) level(exit) = Tile.Stairs

    val free = mutable.ArrayBuffer.from(rng.shuffle(
      level.positions.filter { p =>
        level(p) != Tile.Door && level.walkable(p) && p.distance(level.start) > 6 && p != exit
      }.toVector
    ))
    def take(): Pos = free.remove(free.size - 1)

    def spawn(kind: MonsterKind, at: Pos): Actor = {
      val monster = new Actor(nextId(), Some(kind), at, Tuning.mighty(kind.hp, depth))
      monster.time = rng.int(100)
      monster.asleep = rng.percent(60)
      level.monsters += monster
      monster
    }

    (0 until 4 + (depth + 1) / 2 + rng.int(3)).foreach(_ => spawn(monsterFor(depth, rng), take()))
    bossAt(depth).foreach(spawn(_, exit).asleep = true)

    // Every floor has some way to heal, and something to wear every other one.
    level.drop(take(), Item(ItemKind.Vial(VialEffect.Mending)))
    if(depth % 2 == 1) level.drop(take(), gear(depth, rng))
    (0 until 3 + rng.int(3)).foreach(_ => level.drop(take(), randomItem(depth, rng)))

    // A trap that was noticed is walked around, so there has to be a way around
    // all of them.
    def canAvoid(at: Pos): Boolean = {
      def open(p: Pos) = level.walkable(p) && p != at && !level.traps.contains(p)
      val reach = Paths.distances(level.width, level.height, level.start)(open)
      level.positions.forall(p => !open(p) || (reach(level.index(p)) >= 0))
    }
    (0 until 2 + depth / 2).foreach { _ =>
      (0 until 10).iterator.map(_ => take()).find(canAvoid).foreach { at =>
        level.traps(at) = PlacedTrap(rng.pick(Trap.values.toIndexedSeq), hidden = rng.percent(70))
      }
    }

    // Enough oil to see everything, not enough to linger.
    level.drop(take(), Item(ItemKind.Oil))
    if(rng.percent(Tuning.ExtraOil)) level.drop(take(), Item(ItemKind.Oil))
    (0 until 2).foreach(_ => level.drop(take(), Item(ItemKind.Coins, count = rng.between(4, 8 + depth * 3))))

    if(rng.percent(70)) sideRoom(level, Tile.LockedDoor, rng).foreach { cells =>
      val places = rng.shuffle(cells)
      level.drop(places(0), etched(gear(depth + 1, rng), cursed = false, rng).copy(upgrade = 1 + rng.int(2)))
      level.drop(places(1), if(rng.percent(50)) Item(ItemKind.Vial(VialEffect.Vigor)) else Item(ItemKind.Chart(ChartEffect.Tempering)))
      level.drop(take(), Item(ItemKind.Key))
    }
    if(rng.percent(55)) sideRoom(level, Tile.SecretDoor, rng).foreach { cells =>
      level.drop(rng.pick(cells), randomItem(depth, rng))
      level.drop(rng.pick(cells), Item(ItemKind.Coins, count = rng.between(10, 20)))
    }
    if(depth % 2 == 1 && depth > 1) sideRoom(level, Tile.Door, rng).foreach { cells =>
      level(cells(4)) = Tile.Trader
      val goods = Vector(
        Item(ItemKind.Oil), Item(ItemKind.Vial(VialEffect.Mending)), Item(rng.pick(ItemKind.charts)), gear(depth + 1, rng)
      )
      Vector(cells(0), cells(2), cells(6), cells(8)).zip(goods).foreach { case (at, item) =>
        level.drop(at, item)
        level.wares(at) = Price.of(item)
      }
    }
    level
  }

  def bossAt(depth: Int): Option[MonsterKind] = depth match {
    case 2 => Some(MonsterKind.Archivist)
    case 4 => Some(MonsterKind.Engine)
    case 6 => Some(MonsterKind.Leviathan)
    case Biome.LastDepth => Some(MonsterKind.Curator)
    case _ => None
  }

  def monsterFor(depth: Int, rng: Rng): MonsterKind = {
    val choices = MonsterKind.values.toVector.filter(_.depth <= depth)
    // The ones that just appeared are the most common.
    val weighted = choices.flatMap(kind => Vector.fill(scala.math.max(1, 5 - (depth - kind.depth)))(kind))
    rng.pick(weighted)
  }

  def randomItem(depth: Int, rng: Rng): Item = rng.int(10) match {
    case 0 | 1 => gear(depth, rng)
    case 2 if depth >= 2 => if(rng.percent(50)) Item(rng.pick(ItemKind.lenses)) else prism(rng)
    case 3 => Item(ItemKind.Coins, count = rng.between(3, 6 + depth * 2))
    case n if n < 7 => Item(rng.pick(ItemKind.vials))
    case _ => Item(rng.pick(ItemKind.charts))
  }

  def prism(rng: Rng): Item = Item(rng.pick(ItemKind.prisms), charge = 2)

  def etched(item: Item, cursed: Boolean, rng: Rng): Item = {
    val forCoats = item.kind.isInstanceOf[ItemKind.Coat]
    item.copy(etching = Some(rng.pick(Etching.values.filter(e => e.forCoats == forCoats && e.cursed == cursed).toIndexedSeq)))
  }

  def gear(depth: Int, rng: Rng): Item = {
    val choices = (ItemKind.instruments ++ ItemKind.coats).filter(_.depth <= depth + 1)
    val item = Item(rng.pick(choices), upgrade = if(rng.percent(25)) 1 else 0)
    rng.int(100) match {
      case n if n < 18 => etched(item, cursed = false, rng)
      case n if n < 32 => etched(item, cursed = true, rng)
      case _ => item
    }
  }

  private def placeRooms(rng: Rng): Vector[Room] = {
    val rooms = mutable.ArrayBuffer.empty[Room]
    var attempts = 0
    while(attempts < 400 && rooms.size < 11 || rooms.size < 5) {
      attempts += 1
      val w = rng.between(4, 9)
      val h = rng.between(4, 7)
      val room = Room(rng.between(2, Width - w - 2), rng.between(2, Height - h - 2), w, h)
      if(!rooms.exists(_.near(room))) rooms += room
    }
    rooms.toVector
  }

  /** Digs a room of three by three behind a wall, in what is solid, so that it
    * is never on the way to anything else. Returns its cells, row by row.
    */
  private def sideRoom(level: Level, door: Tile, rng: Rng): Option[Vector[Pos]] = {
    // Not from a room that is itself closed off.
    val open = level.reachable.toSet
    val places = for {
      at <- level.positions.toVector if level(at) == Tile.Wall
      d <- Pos.Cardinal
      front = at - d
      center = at + Pos(d.x * 2, d.y * 2)
      if open(front) && level(front) == Tile.Floor && !level.traps.contains(front)
      if center.x >= 3 && center.y >= 3 && center.x < level.width - 3 && center.y < level.height - 3
      area = for(y <- -2 to 2; x <- -2 to 2) yield center + Pos(x, y)
      if area.forall(p => level(p) == Tile.Rock || level(p) == Tile.Wall)
    } yield (at, center)
    if(places.isEmpty) None
    else {
      val (at, center) = rng.pick(places)
      for(y <- -2 to 2; x <- -2 to 2) if(level(center + Pos(x, y)) == Tile.Rock) level(center + Pos(x, y)) = Tile.Wall
      val cells = (for(y <- -1 to 1; x <- -1 to 1) yield center + Pos(x, y)).toVector
      cells.foreach(level(_) = Tile.Floor)
      level(at) = door
      Some(cells)
    }
  }

  private def connect(level: Level, rooms: Vector[Room], rng: Rng): Unit = {
    val joined = mutable.ArrayBuffer(rooms.head)
    val rest = mutable.ArrayBuffer.from(rooms.tail)
    while(rest.nonEmpty) {
      val (from, to) = (for(a <- joined; b <- rest) yield (a, b)).minBy { case (a, b) => a.center.distance(b.center) }
      carve(level, from.center, to.center, rng)
      joined += to
      rest -= to
    }
    // Some loops, to be chased around.
    (0 until 3).foreach { _ =>
      val a = rng.pick(rooms)
      val b = rng.pick(rooms)
      if(a != b && a.center.distance(b.center) < 20) carve(level, a.center, b.center, rng)
    }
  }

  private def carve(level: Level, from: Pos, to: Pos, rng: Rng): Unit = {
    var at = from
    val horizontalFirst = rng.percent(50)
    def step(horizontal: Boolean): Unit = {
      def done = if(horizontal) at.x == to.x else at.y == to.y
      while(!done) {
        at = if(horizontal) Pos(at.x + (to.x - at.x).sign, at.y) else Pos(at.x, at.y + (to.y - at.y).sign)
        if(level(at) == Tile.Rock) level(at) = Tile.Floor
      }
    }
    step(horizontalFirst)
    step(!horizontalFirst)
  }

  // A corridor that enters a room through a gap of one cell gets a door there.
  private def placeDoors(level: Level, rooms: Vector[Room], rng: Rng): Unit = {
    def outside(p: Pos) = !rooms.exists(_.contains(p))
    level.positions.filter(p => level(p) == Tile.Floor && outside(p)).foreach { p =>
      def open(dx: Int, dy: Int) = level.walkable(p + Pos(dx, dy))
      def entersRoom = Pos.Cardinal.exists(d => rooms.exists(_.contains(p + d)))
      val passes = (open(-1, 0) && open(1, 0) && !open(0, -1) && !open(0, 1)) ||
        (open(0, -1) && open(0, 1) && !open(-1, 0) && !open(1, 0))
      val alone = !Pos.Cardinal.exists(d => level(p + d) == Tile.Door)
      if(passes && entersRoom && alone && rng.percent(75)) level(p) = Tile.Door
    }
  }

  private def decorate(level: Level, rooms: Vector[Room], rng: Rng, keep: Set[Pos]): Unit = {
    val (pools, reeds, shelves, pillars) = level.biome match {
      case Biome.Archive => (1, 3, 14, 2)
      case Biome.Orrery => (2, 1, 2, 12)
      case Biome.Cistern => (7, 4, 0, 5)
      case Biome.StarWell => (4, 2, 4, 8)
    }
    def blob(tile: Tile): Unit = {
      var at = rng.pick(rng.pick(rooms).cells)
      (0 until rng.between(12, 28)).foreach { _ =>
        if(level(at) == Tile.Floor && !keep(at)) level(at) = tile
        val next = at + rng.pick(Pos.Cardinal)
        if(level.walkable(next)) at = next
      }
    }
    (0 until pools).foreach(_ => blob(Tile.Water))
    (0 until reeds).foreach(_ => blob(Tile.Reeds))

    // Furniture must not cut the floor in two.
    def place(tile: Tile): Unit = {
      val p = rng.pick(rng.pick(rooms).cells)
      val byDoor = Pos.Cardinal.exists(d => level(p + d) == Tile.Door)
      if(level(p) == Tile.Floor && !byDoor && !keep.exists(_.distance(p) <= 1)) {
        level(p) = tile
        val reach = Paths.distances(level.width, level.height, level.start)(level.walkable)
        if(level.positions.exists(q => level.walkable(q) && reach(level.index(q)) < 0)) level(p) = Tile.Floor
      }
    }
    (0 until shelves).foreach(_ => place(Tile.Shelf))
    (0 until pillars).foreach(_ => place(Tile.Pillar))
  }
}
