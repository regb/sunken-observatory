package observatory
package model

import scala.collection.mutable

/** What a player can do with a turn. `slot` is an index in the pack. */
enum Command {
  case Step(dx: Int, dy: Int)
  case Wait
  case PickUp
  case Descend
  case Ascend
  /** Takes a while, and finds what is hidden around. */
  case Search
  case Ability
  case Use(slot: Int)
  case Drop(slot: Int)
  case Throw(slot: Int, x: Int, y: Int)
  /** Uses a prism on a cell. */
  case Aim(slot: Int, x: Int, y: Int)
  /** Takes gear apart, towards a chart of tempering. */
  case Scrap(slot: Int)
}

object Command {
  def encode(command: Command): String = command match {
    case Step(dx, dy) => s"s$dx,$dy"
    case Wait => "w"
    case PickUp => "p"
    case Descend => "d"
    case Ascend => "a"
    case Search => "f"
    case Ability => "b"
    case Use(slot) => s"u$slot"
    case Drop(slot) => s"o$slot"
    case Throw(slot, x, y) => s"t$slot,$x,$y"
    case Aim(slot, x, y) => s"z$slot,$x,$y"
    case Scrap(slot) => s"c$slot"
  }

  def decode(text: String): Option[Command] = try {
    val numbers = if(text.length > 1) text.drop(1).split(",").map(_.toInt).toVector else Vector.empty
    (text.head, numbers) match {
      case ('s', Vector(dx, dy)) => Some(Step(dx, dy))
      case ('w', Vector()) => Some(Wait)
      case ('p', Vector()) => Some(PickUp)
      case ('d', Vector()) => Some(Descend)
      case ('a', Vector()) => Some(Ascend)
      case ('f', Vector()) => Some(Search)
      case ('b', Vector()) => Some(Ability)
      case ('u', Vector(slot)) => Some(Use(slot))
      case ('o', Vector(slot)) => Some(Drop(slot))
      case ('t', Vector(slot, x, y)) => Some(Throw(slot, x, y))
      case ('c', Vector(slot)) => Some(Scrap(slot))
      case ('z', Vector(slot, x, y)) => Some(Aim(slot, x, y))
      case _ => None
    }
  } catch {
    case _: NumberFormatException | _: NoSuchElementException => None
  }
}

/** What happened, for a view to show. The game does not wait for it. */
enum Event {
  case Moved(actor: Int, from: Pos, to: Pos)
  case Struck(attacker: Int, target: Int, at: Pos, damage: Int)
  case Missed(attacker: Int, target: Int, at: Pos)
  case Hurt(target: Int, at: Pos, damage: Int, by: Status)
  case Healed(target: Int, at: Pos, amount: Int)
  case Died(actor: Int, at: Pos)
  case Appeared(actor: Int, at: Pos)
  case Shot(from: Pos, to: Pos)
  case Thrown(item: Item, from: Pos, to: Pos)
  case Burst(at: Pos, fire: Boolean)
  case Teleported(actor: Int, to: Pos)
  case Took(item: Item)
  case Upgraded
  case LevelUp
  case Arrived(depth: Int)
  case Message(text: String)
  case Won
  case Lost(cause: String)
}

enum Outcome { case Playing, Won, Lost }

/** A whole game. Everything that happens follows from the seed and the commands
  * performed. A save holds where it has come to: see [[Codec]].
  */
final class Game private[model] (val seed: Long, val calling: Calling, fresh: Boolean) {
  def this(seed: Long, calling: Calling) = this(seed, calling, true)
  def this(seed: Long) = this(seed, Calling.Surveyor)

  val PackSize: Int = 16

  private[model] val rng = new Rng(seed)
  private[model] var ids = 0
  private def nextId(): Int = { ids += 1; ids }

  val player: Actor = new Actor(0, None, Pos(0, 0), calling.hp)
  var depth: Int = 0
  var level: Level = scala.compiletime.uninitialized
  /** The floors that were visited, the current one included. */
  val floors: mutable.Map[Int, Level] = mutable.Map.empty
  /** With the Lodestar taken, the water rises. */
  var hasLodestar: Boolean = false
  val MaxOil: Int = Tuning.MaxOil
  /** Turns the lamp still burns. */
  var oil: Int = Tuning.StartOil
  var coins: Int = 0
  /** What gear that was taken apart left. */
  var brass: Int = 0
  /** The turn from which the ability can be used. */
  var abilityReady: Int = 0
  var visible: Set[Pos] = Set.empty
  var experience: Int = 0
  var rank: Int = 1
  var outcome: Outcome = Outcome.Playing
  var turns: Int = 0

  val pack: mutable.ArrayBuffer[Item] = mutable.ArrayBuffer.empty
  var instrument: Item = Item(ItemKind.Dividers)
  var coat: Item = Item(ItemKind.OilCoat)
  var lens: Option[Item] = None

  /** The kinds of monsters that were seen. */
  val met: mutable.Set[MonsterKind] = mutable.Set.empty
  private[model] val known = mutable.HashSet.empty[ItemKind]
  private[model] var vialLooks = rng.shuffle(ItemKind.VialLooks)
  private[model] var chartLooks = rng.shuffle(ItemKind.ChartLooks)

  private val events = mutable.ArrayBuffer.empty[Event]
  private[model] var nextTick = 100
  private[model] var lastHurtBy = ""

  if(fresh) {
    pack += Item(ItemKind.Vial(VialEffect.Mending))
    known += ItemKind.Vial(VialEffect.Mending)
    calling match {
      case Calling.Surveyor =>
        known += ItemKind.Chart(ChartEffect.Atlas)
        pack += Item(ItemKind.Chart(ChartEffect.Atlas))
      case Calling.Diver => coins = 12
      case Calling.Lamplighter =>
        oil = MaxOil
        pack += Item(ItemKind.Oil)
    }
    arrive(1)
  }

  // ---- What a view asks ----

  def takeEvents(): Vector[Event] = {
    val result = events.toVector
    events.clear()
    result
  }

  def isKnown(kind: ItemKind): Boolean = kind match {
    case _: ItemKind.Vial | _: ItemKind.Chart => known(kind)
    case _ => true
  }

  def look(kind: ItemKind): Int = kind match {
    case ItemKind.Vial(effect) => ItemKind.VialLooks.indexOf(vialLooks(effect.ordinal))
    case ItemKind.Chart(effect) => ItemKind.ChartLooks.indexOf(chartLooks(effect.ordinal))
    case _ => 0
  }

  def title(item: Item): String = {
    val base = item.kind match {
      case ItemKind.Vial(effect) if !known(item.kind) => s"${vialLooks(effect.ordinal)} vial"
      case ItemKind.Chart(effect) if !known(item.kind) => s"chart of ${chartLooks(effect.ordinal)}"
      case kind => kind.title
    }
    val etching = item.etching.filter(_ => item.worn).map(_.title + " ").getOrElse("")
    etching + base + (if(item.upgrade > 0) s" +${item.upgrade}" else "") + (if(item.count > 1) s" x${item.count}" else "")
  }

  def about(item: Item): String =
    if(isKnown(item.kind))
      item.kind.about + item.etching.filter(_ => item.worn).map(" " + _.about).getOrElse("") +
        (if(item.kind.isInstanceOf[ItemKind.Prism]) s" ${item.charge} of ${item.maxCharge} left." else "")
    else "You will know what it does once it is used."

  def experienceNeeded: Int = Tuning.RankExperience * rank
  def armor: Int = armorOf(coat) + lensBonus(LensEffect.Warding, 2)
  def sight: Int = if(oil > 200) 8 + lensBonus(LensEffect.Farsight, 4) else if(oil > 0) 6 else 3
  /** Whether the way down is shut. */
  def sealedOff: Boolean = level.monsters.exists(m => m.alive && m.kind.exists(_.boss))
  def nearTrader: Boolean = level.positions.exists(p => level(p) == Tile.Trader && p.distance(player.pos) <= 2 && visible(p))
  def sellPrice(item: Item): Int = scala.math.max(1, Price.of(item) / 3)
  def abilityWait: Int = scala.math.max(0, abilityReady - turns)
  def price(at: Pos): Option[Int] = level.wares.get(at)

  def visibleMonsters: Vector[Actor] = level.monsters.filter(m => m.alive && visible(m.pos)).toVector

  /** The way to a cell through what has been explored, around what is dangerous. */
  def pathTo(to: Pos): Vector[Pos] = {
    def open(p: Pos): Boolean = {
      val i = if(level.contains(p)) level.index(p) else -1
      i >= 0 && level.explored(i) && level.walkable(p) && level.fire(i) == 0 && level.gas(i) == 0 &&
        !level.traps.get(p).exists(!_.hidden) && !(visible(p) && level.monsterAt(p).isDefined)
    }
    if((level.walkable(to) || level(to) == Tile.LockedDoor) && level.explored(level.index(to))) Paths.find(level.width, level.height, player.pos, to)(open)
    else Vector.empty
  }

  // ---- Saving ----

  def save: String = Codec.write(this)

  // ---- Playing ----

  /** Plays a turn. False when the command cannot be done, and no time passes. */
  def perform(command: Command): Boolean = {
    if(outcome != Outcome.Playing) return false
    val cost = execute(command)
    if(cost > 0) {
      turns += 1
      player.time += (if(player.has(Status.Hasted)) cost / 2 else cost)
      advance()
    }
    cost > 0
  }

  private def execute(command: Command): Int = command match {
    case Command.Wait => 100
    case Command.Step(dx, dy) =>
      if(dx.abs > 1 || dy.abs > 1 || (dx == 0 && dy == 0)) 0 else step(player.pos + Pos(dx, dy))
    case Command.PickUp => pickUp()
    case Command.Descend =>
      if(level(player.pos) != Tile.Stairs) 0
      else if(sealedOff) {
        say("The way down is shut while its keeper stands.")
        0
      } else {
        arrive(depth + 1)
        100
      }
    case Command.Ascend =>
      if(level(player.pos) != Tile.StairsUp) 0
      else if(depth > 1) {
        arrive(depth - 1)
        100
      } else if(hasLodestar) {
        outcome = Outcome.Won
        events += Event.Won
        100
      } else {
        say("Not without the Lodestar.")
        0
      }
    case Command.Search =>
      find(radius = 3)
      300
    case Command.Ability => useAbility()
    case Command.Use(slot) => if(pack.isDefinedAt(slot)) use(slot) else 0
    case Command.Drop(slot) =>
      if(!pack.isDefinedAt(slot)) 0
      else {
        val item = takeFrom(slot)
        if(nearTrader) {
          coins += sellPrice(item)
          say(s"You sell the ${title(item)} for ${sellPrice(item)} coins.")
        } else {
          level.drop(player.pos, item)
          say(s"You drop the ${title(item)}.")
        }
        100
      }
    case Command.Throw(slot, x, y) => if(pack.isDefinedAt(slot)) throwItem(slot, Pos(x, y)) else 0
    case Command.Scrap(slot) => if(pack.isDefinedAt(slot)) scrap(slot) else 0
    case Command.Aim(slot, x, y) => if(pack.isDefinedAt(slot)) aim(slot, Pos(x, y)) else 0
  }

  private def step(to: Pos): Int = level.monsterAt(to) match {
    case Some(monster) =>
      strike(player, monster)
      instrument.kind match {
        case kind: ItemKind.Instrument => kind.time + (if(instrument.has(Etching.Leaden)) 50 else 0)
        case _ => 100
      }
    case None if level(to) == Tile.LockedDoor =>
      val key = pack.indexWhere(_.kind == ItemKind.Key)
      if(key < 0) {
        say("It is locked.")
        0
      } else {
        val _ = takeFrom(key)
        level(to) = Tile.Door
        say("The key turns, and breaks.")
        100
      }
    case None if level.walkable(to) =>
      move(player, to)
      level.items.get(to).foreach { items =>
        say(s"Here: ${items.map(title).mkString(", ")}" + price(to).map(cost => s", for $cost coins.").getOrElse("."))
      }
      noticeTraps()
      100
    case None => 0
  }

  private def move(actor: Actor, to: Pos): Unit = {
    events += Event.Moved(actor.id, actor.pos, to)
    actor.pos = to
    level.traps.get(to).filter(_ => actor.isPlayer).foreach(spring(to, _))
  }

  private def spring(at: Pos, trap: PlacedTrap): Unit = {
    level.traps.remove(at)
    say(s"You set off a ${trap.kind.title}!")
    trap.kind match {
      case Trap.Cinder => burst(at)
      case Trap.Miasma => release(at, Tuning.VentGas)
      case Trap.Alarm =>
        level.monsters.foreach { monster =>
          monster.asleep = false
          monster.hunting = Some(at)
        }
    }
  }

  /** Finds everything hidden within a radius. */
  private def find(radius: Int): Unit = {
    val near = level.positions.filter(_.distance(player.pos) <= radius).toVector
    val traps = near.flatMap(p => level.traps.get(p).filter(_.hidden).map(p -> _))
    traps.foreach { case (p, trap) => level.traps(p) = trap.copy(hidden = false) }
    val doors = near.filter(level(_) == Tile.SecretDoor)
    doors.foreach(level(_) = Tile.Door)
    if(traps.nonEmpty) say("You find a trap.")
    if(doors.nonEmpty) say("You find a hidden door!")
    if(traps.isEmpty && doors.isEmpty) say("You find nothing.")
  }

  private def useAbility(): Int =
    if(abilityWait > 0) {
      say(s"Not yet: $abilityWait turns.")
      0
    } else {
      val used = calling match {
        case Calling.Surveyor =>
          level.positions.filter(p => p.distance(player.pos) <= 10 && (level.walkable(p) || p.neighbors.exists(level.walkable)))
            .foreach(p => level.explored(level.index(p)) = true)
          find(radius = 10)
          true
        case Calling.Diver =>
          if(level(player.pos) != Tile.Water) {
            say("There is no water here.")
            false
          } else {
            player.statuses(Status.Submerged) = 8
            player.statuses.remove(Status.Burning)
            level.monsters.foreach(_.hunting = None)
            say("You slip under.")
            true
          }
        case Calling.Lamplighter =>
          if(oil < Tuning.FlareOil) {
            say("There is not enough oil.")
            false
          } else {
            oil -= Tuning.FlareOil
            events += Event.Burst(player.pos, fire = true)
            visibleMonsters.foreach(_.statuses(Status.Stilled) = Tuning.FlareTurns)
            say("Your lamp flares.")
            true
          }
      }
      if(used) {
        abilityReady = turns + calling.rest
        100
      } else 0
    }

  private def noticeTraps(): Unit = {
    player.pos.neighbors.filter(level(_) == Tile.SecretDoor).foreach { p =>
      if(rng.percent(20)) {
        level(p) = Tile.Door
        say("A draft: there is a door here.")
      }
    }
    player.pos.neighbors.foreach { p =>
      level.traps.get(p).filter(_.hidden).foreach { trap =>
        if(rng.percent(60)) {
          level.traps(p) = trap.copy(hidden = false)
          say(s"You notice a ${trap.kind.title}.")
        }
      }
    }
  }

  private def pickUp(): Int = level.items.get(player.pos) match {
    case Some(item :: _) if price(player.pos).exists(_ > coins) =>
      say(s"The trader wants ${price(player.pos).getOrElse(0)} coins for the ${title(item)}. You have $coins.")
      0
    case Some(item :: rest) if item.kind == ItemKind.Coins =>
      if(rest.isEmpty) level.items.remove(player.pos) else level.items(player.pos) = rest
      coins += item.count
      events += Event.Took(item)
      say(s"You take ${item.count} coins.")
      100
    case Some(item :: rest) =>
      if(item.kind == ItemKind.Lodestar) {
        if(rest.isEmpty) level.items.remove(player.pos) else level.items(player.pos) = rest
        hasLodestar = true
        events += Event.Took(item)
        say("You take the Lodestar. Far below, something gives way: the water is rising!")
        100
      } else if(stow(item)) {
        if(rest.isEmpty) level.items.remove(player.pos) else level.items(player.pos) = rest
        level.wares.remove(player.pos).foreach(coins -= _)
        events += Event.Took(item)
        say(s"You take the ${title(item)}.")
        100
      } else {
        say("Your pack is full.")
        0
      }
    case _ => 0
  }

  private def stow(item: Item): Boolean = {
    val same = if(item.kind.stacks) pack.indexWhere(_.kind == item.kind) else -1
    if(same >= 0) {
      pack(same) = pack(same).copy(count = pack(same).count + item.count)
      true
    } else if(pack.size < PackSize) {
      pack += item
      true
    } else false
  }

  private def takeFrom(slot: Int): Item = {
    val item = pack(slot)
    if(item.count > 1) pack(slot) = item.copy(count = item.count - 1) else pack.remove(slot)
    item.copy(count = 1)
  }

  private def use(slot: Int): Int = pack(slot).kind match {
    case _: ItemKind.Instrument if instrument.cursed => stuck(instrument)
    case _: ItemKind.Coat if coat.cursed => stuck(coat)
    case _: ItemKind.Prism =>
      say("Aim it at something.")
      0
    case _: ItemKind.Instrument =>
      val worn = instrument
      instrument = takeFrom(slot).copy(worn = true)
      pack.insert(slot, worn)
      say(s"You hold the ${title(instrument)}.")
      100
    case _: ItemKind.Coat =>
      val worn = coat
      coat = takeFrom(slot).copy(worn = true)
      pack.insert(slot, worn)
      say(s"You put on the ${title(coat)}.")
      100
    case _: ItemKind.Lens =>
      val worn = lens
      lens = Some(takeFrom(slot))
      worn.foreach(pack.insert(slot, _))
      say("You look through the lens.")
      100
    case ItemKind.Vial(effect) =>
      val _ = takeFrom(slot)
      known += ItemKind.Vial(effect)
      say(s"You drink the vial of ${effect.title}.")
      effect match {
        case VialEffect.Mending => mend(player)
        case VialEffect.Cinder => burst(player.pos)
        case VialEffect.Miasma => release(player.pos, Tuning.VialGas)
        case VialEffect.Quickening => player.statuses(Status.Hasted) = 20
        case VialEffect.Vigor =>
          player.maxHp += 5
          player.hp += 5
          events += Event.Healed(player.id, player.pos, 5)
      }
      100
    case ItemKind.Chart(effect) =>
      val _ = takeFrom(slot)
      known += ItemKind.Chart(effect)
      say(s"You read the chart of ${effect.title}.")
      read(effect)
      100
    case ItemKind.Oil =>
      val _ = takeFrom(slot)
      oil = scala.math.min(MaxOil, oil + Tuning.OilFlask)
      say("You fill the lamp.")
      100
    case ItemKind.Key | ItemKind.Coins | ItemKind.Lodestar => 0
  }

  /** The brass that an item can be taken apart for. */
  def brassIn(item: Item): Int = item.kind match {
    case _: ItemKind.Instrument | _: ItemKind.Coat => 1 + item.upgrade + (if(item.etching.exists(!_.cursed)) 1 else 0)
    case _: ItemKind.Lens | _: ItemKind.Prism => 2 + item.upgrade
    case _ => 0
  }

  private def scrap(slot: Int): Int =
    if(brassIn(pack(slot)) == 0) 0
    else {
      val item = takeFrom(slot)
      brass += brassIn(item)
      say(s"You take the ${title(item)} apart.")
      if(brass >= Tuning.BrassForChart) {
        brass -= Tuning.BrassForChart
        val chart = Item(ItemKind.Chart(ChartEffect.Tempering))
        known += chart.kind
        if(!stow(chart)) level.drop(player.pos, chart)
        say("The brass is enough to etch a chart of tempering.")
      }
      100
    }

  private def stuck(item: Item): Int = {
    say(s"The ${title(item)} will not come off.")
    0
  }

  private def read(effect: ChartEffect): Unit = effect match {
    case ChartEffect.Atlas =>
      level.positions.foreach { p =>
        if(level.walkable(p) || p.neighbors.exists(level.walkable)) level.explored(level.index(p)) = true
      }
    case ChartEffect.Passage =>
      // Never into what is locked.
      val to = rng.pick(level.reachable.filter(p => level.monsterAt(p).isEmpty && p != player.pos))
      player.pos = to
      events += Event.Teleported(player.id, to)
    case ChartEffect.Tempering =>
      // What is cursed first.
      if(if(instrument.cursed != coat.cursed) instrument.cursed else rng.percent(60)) {
        instrument = tempered(instrument)
        say(s"Your ${title(instrument)} gleams.")
      } else {
        coat = tempered(coat)
        say(s"Your ${title(coat)} gleams.")
      }
      events += Event.Upgraded
    case ChartEffect.Stillness =>
      visibleMonsters.foreach(_.statuses(Status.Stilled) = 8)
    case ChartEffect.Undertow =>
      level.positions.filter(_.distance(player.pos) <= 3).foreach { p =>
        level(p) match {
          case Tile.Floor | Tile.Reeds | Tile.Ash =>
            level(p) = Tile.Water
            level.fire(level.index(p)) = 0
          case _ => ()
        }
      }
      visibleMonsters.filter(_.pos.distance(player.pos) <= 3).foreach { monster =>
        val away = Pos((monster.pos.x - player.pos.x).sign, (monster.pos.y - player.pos.y).sign)
        (0 until 3).foreach { _ =>
          val to = monster.pos + away
          if(level.walkable(to) && level.monsterAt(to).isEmpty) move(monster, to)
        }
        harm(monster, 3, "the undertow")
      }
  }

  private def tempered(item: Item): Item =
    item.copy(upgrade = item.upgrade + 1, etching = item.etching.filterNot(_.cursed))

  private def aim(slot: Int, target: Pos): Int = pack(slot).kind match {
    case ItemKind.Prism(effect) =>
      if(target == player.pos || !visible(target)) 0
      else if(pack(slot).charge <= 0) {
        say("The prism is dark.")
        0
      } else {
        pack(slot) = pack(slot).copy(charge = pack(slot).charge - 1)
        val line = Paths.line(player.pos, target)
        val direction = line.last - player.pos
        // On past the target, up to what stops light.
        val far = Paths.line(player.pos, Pos(player.pos.x + direction.x * 8, player.pos.y + direction.y * 8)).take(9).takeWhile(level.walkable)
        val struck = far.flatMap(level.monsterAt)
        events += Event.Shot(player.pos, struck.headOption.map(_.pos).orElse(far.lastOption).getOrElse(player.pos))
        val power = pack(slot).upgrade
        def scathe(monster: Actor, min: Int, max: Int): Unit = {
          val damage = rng.between(min, max) + power * 2
          events += Event.Struck(player.id, monster.id, monster.pos, damage)
          monster.hunting = Some(player.pos)
          harm(monster, damage, "a prism")
        }
        effect match {
          case PrismEffect.Frost => struck.headOption.foreach { monster =>
            scathe(monster, 3, 7)
            if(monster.alive) monster.statuses(Status.Stilled) = 4
          }
          case PrismEffect.Lance => struck.foreach(scathe(_, 4, 9))
          case PrismEffect.Tether => struck.headOption.foreach { monster =>
            val here = player.pos
            player.pos = monster.pos
            monster.pos = here
            events += Event.Teleported(player.id, player.pos)
            events += Event.Teleported(monster.id, monster.pos)
          }
          case PrismEffect.Gust => struck.headOption.foreach { monster =>
            val away = Pos((monster.pos.x - player.pos.x).sign, (monster.pos.y - player.pos.y).sign)
            var left = 4
            while(left > 0 && free(monster.pos + away)) {
              move(monster, monster.pos + away)
              left -= 1
            }
            scathe(monster, 1, 2 + left * 2)
          }
        }
        100
      }
    case _ => 0
  }

  private def throwItem(slot: Int, target: Pos): Int = {
    if(target == player.pos || !visible(target)) 0
    else {
      val item = takeFrom(slot)
      // It stops at the first thing in the way.
      val line = Paths.line(player.pos, target)
      val reached = line.takeWhile(level.walkable)
      val landed = reached.find(p => level.monsterAt(p).isDefined).orElse(reached.lastOption).getOrElse(player.pos)
      events += Event.Thrown(item, player.pos, landed)
      item.kind match {
        case ItemKind.Vial(effect) =>
          known += item.kind
          say(s"The vial of ${effect.title} shatters.")
          effect match {
            case VialEffect.Cinder => burst(landed)
            case VialEffect.Miasma => release(landed, Tuning.VialGas)
            case VialEffect.Mending => level.monsterAt(landed).foreach(mend)
            case VialEffect.Quickening => level.monsterAt(landed).foreach(_.statuses(Status.Hasted) = 20)
            case VialEffect.Vigor => ()
          }
        case _: ItemKind.Instrument =>
          level.monsterAt(landed).foreach(strike(player, _, thrown = Some(item)))
          level.drop(landed, item)
        case _ => level.drop(landed, item)
      }
      100
    }
  }

  // ---- Harm and healing ----

  private def burst(at: Pos): Unit = {
    events += Event.Burst(at, fire = true)
    (at +: at.neighbors).foreach(level.ignite(_, 5))
    (player +: level.monsters.toVector).filter(_.pos.distance(at) <= 1).foreach(ignite)
  }

  private def release(at: Pos, amount: Int): Unit = {
    events += Event.Burst(at, fire = false)
    level.addGas(at, amount)
  }

  private def ignite(actor: Actor): Unit =
    if(level(actor.pos) != Tile.Water && !actor.has(Status.Burning)) actor.statuses(Status.Burning) = 4

  private def mend(actor: Actor): Unit = {
    val amount = scala.math.min(actor.maxHp - actor.hp, actor.maxHp * 3 / 4)
    actor.hp += amount
    actor.statuses.remove(Status.Poisoned)
    events += Event.Healed(actor.id, actor.pos, amount)
  }

  private def lensBonus(effect: LensEffect, bonus: Int): Int =
    if(lens.exists(_.kind == ItemKind.Lens(effect))) bonus else 0

  private def armorOf(item: Item): Int = item.kind match {
    case coat: ItemKind.Coat => coat.armor + item.upgrade + (if(item.has(Etching.Steadfast)) 2 else 0)
    case _ => 0
  }

  private def strike(attacker: Actor, target: Actor, thrown: Option[Item] = None): Unit = {
    val (min, max) = attacker.kind match {
      case Some(kind) => (Tuning.mighty(kind.minDamage, depth), Tuning.mighty(kind.maxDamage, depth))
      case None =>
        val item = thrown.getOrElse(instrument)
        item.kind match {
          case held: ItemKind.Instrument => (held.min + item.upgrade, held.max + item.upgrade * 2)
          case _ => (1, 2)
        }
    }
    val skill = attacker.kind.map(_.skill).getOrElse(9 + rank + lensBonus(LensEffect.Keenness, 4))
    val evasion = target.kind.map(_.evasion).getOrElse(4 + rank / 2)
    val unaware = !target.isPlayer && (target.asleep || target.has(Status.Stilled) || attacker.has(Status.Submerged))
    if(attacker.isPlayer) { val _ = attacker.statuses.remove(Status.Submerged) }
    val chance = scala.math.max(25, scala.math.min(95, 75 + (skill - evasion) * 4))
    if(unaware || rng.percent(chance)) {
      val blow = rng.between(min, max) * (if(unaware) 2 else 1)
      val stopped = rng.between(0, target.kind.map(_.armor).getOrElse(armor))
      val damage = scala.math.max(if(unaware) 1 else 0, blow - stopped)
      events += Event.Struck(attacker.id, target.id, target.pos, damage)
      if(attacker.kind.exists(_.behavior == Behavior.Venomous) && damage > 0) target.statuses(Status.Poisoned) = Tuning.VenomTurns
      if(damage > 0) harm(target, damage, attacker.kind.map(_.named).getOrElse("yourself"))
      if(attacker.isPlayer && target.alive && rng.percent(25)) {
        if(instrument.has(Etching.Searing)) ignite(target)
        if(instrument.has(Etching.Ringing)) target.statuses(Status.Stilled) = 2
      }
      if(attacker.isPlayer && damage > 0 && instrument.has(Etching.Thirsting) && player.hp < player.maxHp) player.hp += 1
      if(target.isPlayer && attacker.alive && coat.has(Etching.Thorned) && attacker.pos.distance(player.pos) == 1) {
        events += Event.Struck(player.id, attacker.id, attacker.pos, 2)
        harm(attacker, 2, "thorns")
      }
      if(target.isPlayer && target.alive && attacker.alive && attacker.kind.exists(_.behavior == Behavior.Thief) && attacker.loot.isEmpty) steal(attacker)
    } else events += Event.Missed(attacker.id, target.id, target.pos)
    target.asleep = false
    if(attacker.isPlayer && coat.has(Etching.Beckoning)) level.monsters.filter(_.pos.distance(player.pos) <= 10).foreach(_.asleep = false)
    if(!target.isPlayer) target.hunting = Some(attacker.pos)
  }

  private def harm(target: Actor, damage: Int, by: String): Unit = if(target.alive) {
    target.hp = scala.math.max(0, target.hp - damage)
    target.asleep = false
    val _ = target.statuses.remove(Status.Stilled)
    if(target.isPlayer) lastHurtBy = by
    if(!target.alive) die(target)
    else if(target.kind.exists(_.behavior == Behavior.Splitter) && target.hp >= 4) split(target)
  }

  private def steal(thief: Actor): Unit = if(pack.nonEmpty) {
    val taken = takeFrom(rng.int(pack.size))
    thief.loot = Some(taken)
    say(s"The ${thief.title} takes your ${title(taken)}!")
  }

  private def split(actor: Actor): Unit = {
    val choices = actor.pos.neighbors.filter(free)
    if(choices.nonEmpty && level.monsters.size < 20) {
      val half = actor.hp / 2
      val other = new Actor(nextId(), actor.kind, rng.pick(choices), half)
      other.time = actor.time
      other.hunting = Some(player.pos)
      actor.hp -= half
      level.monsters += other
      events += Event.Appeared(other.id, other.pos)
    }
  }

  private def die(actor: Actor): Unit = {
    events += Event.Died(actor.id, actor.pos)
    actor.kind match {
      case None =>
        outcome = Outcome.Lost
        events += Event.Lost(lastHurtBy)
      case Some(kind) =>
        level.monsters -= actor
        say(s"${kind.named} is destroyed.")
        actor.loot.foreach(level.drop(actor.pos, _))
        if(kind == MonsterKind.Curator) level.drop(actor.pos, Item(ItemKind.Lodestar))
        else if(kind.boss) {
          level.drop(actor.pos, Generator.prism(rng))
          level.drop(actor.pos, Item(ItemKind.Vial(VialEffect.Vigor)))
          say("Below, a lock falls open.")
        }
        else if(rng.percent(30)) level.drop(actor.pos, Item(ItemKind.Coins, count = rng.between(2, 4 + depth * 2)))
        else if(rng.percent(15)) level.drop(actor.pos, Generator.randomItem(depth, rng))
        gain(kind.xp)
    }
  }

  private def gain(amount: Int): Unit = {
    experience += amount
    while(experience >= experienceNeeded) {
      experience -= experienceNeeded
      rank += 1
      player.maxHp += Tuning.RankHealth
      player.hp += Tuning.RankHealth
      events += Event.LevelUp
      say(s"You are now of rank $rank.")
    }
  }

  // ---- Time ----

  private def advance(): Unit = {
    while(outcome == Outcome.Playing && nextTurn < player.time) {
      val due = level.monsters.filter(_.alive).minByOption(_.time)
      if(due.forall(_.time >= nextTick)) {
        tick()
        nextTick += 100
      } else due.foreach(act)
    }
    look()
  }

  private def nextTurn: Int = level.monsters.foldLeft(nextTick)((time, monster) => scala.math.min(time, monster.time))

  private def tick(): Unit = {
    level.tick(rng)
    level.tide(nextTick / 100)
    if((nextTick / 100) % 40 == 0) pack.indices.foreach { i =>
      if(pack(i).kind.isInstanceOf[ItemKind.Prism] && pack(i).charge < pack(i).maxCharge) pack(i) = pack(i).copy(charge = pack(i).charge + 1)
    }
    (player +: level.monsters.toVector).foreach { actor =>
      if(actor.alive) {
        val here = level.index(actor.pos)
        if(level.fire(here) > 0) ignite(actor)
        if(level.gas(here) > 0) actor.statuses(Status.Poisoned) = scala.math.max(2, actor.statuses.getOrElse(Status.Poisoned, 0))
        if(actor.has(Status.Burning)) {
          if(level(actor.pos) == Tile.Water) actor.statuses.remove(Status.Burning)
          else {
            level.ignite(actor.pos, 2)
            wound(actor, rng.between(1, 3), Status.Burning)
          }
        }
        if(actor.has(Status.Poisoned)) wound(actor, 1, Status.Poisoned)
        actor.statuses.keys.toVector.sortBy(_.ordinal).foreach { status =>
          actor.statuses.get(status).foreach { left =>
            if(left <= 1) actor.statuses.remove(status) else actor.statuses(status) = left - 1
          }
        }
      }
    }
    burnOil()
    if(level(player.pos) != Tile.Water) { val _ = player.statuses.remove(Status.Submerged) }
    if(hasLodestar && player.alive) rise()
    if(player.alive && oil > 0 && (nextTick / 100) % Tuning.RegenTurns == 0 && player.hp < player.maxHp && !player.has(Status.Poisoned)) player.hp += 1
    if(player.alive) wander()
  }

  private def burnOil(): Unit = {
    val burns = calling != Calling.Lamplighter || (nextTick / 100) % 2 == 0
    if(oil > 0 && burns) {
      oil -= 1
      if(oil == 200) say("Your lamp burns low.")
      if(oil == 0) say("Your lamp goes out.")
    }
  }

  private def wound(actor: Actor, damage: Int, by: Status): Unit = if(actor.alive) {
    events += Event.Hurt(actor.id, actor.pos, damage, by)
    harm(actor, damage, by match {
      case Status.Burning => "flames"
      case _ => "poison"
    })
  }

  private def act(monster: Actor): Unit = {
    val kind = monster.kind.get
    val inWater = level(monster.pos) == Tile.Water
    val speed = kind.speed * (if((kind.behavior == Behavior.Swimmer || kind.behavior == Behavior.Leviathan) && inWater) 2 else 1) * (if(monster.has(Status.Hasted)) 2 else 1)
    monster.time += 10000 / speed
    monster.cooldown = scala.math.max(0, monster.cooldown - 1)

    val distance = monster.pos.distance(player.pos)
    val sees = canSeePlayer(monster)
    if(monster.has(Status.Stilled)) ()
    else if(monster.asleep) {
      if(sees && rng.percent(if(distance <= 3) 25 else 8)) {
        monster.asleep = false
        monster.hunting = Some(player.pos)
      }
    } else {
      if(sees) monster.hunting = Some(player.pos)
      kind.behavior match {
        case Behavior.Erratic if rng.percent(40) => wander(monster)
        case Behavior.Ranged if sees && distance > 1 && distance <= 6 && monster.cooldown == 0 =>
          monster.cooldown = 2
          events += Event.Shot(monster.pos, player.pos)
          strike(monster, player)
        case Behavior.Thief if monster.loot.nonEmpty => flee(monster)
        case Behavior.Beam if monster.aim.nonEmpty => fire(monster)
        case Behavior.Beam if sees && monster.cooldown == 0 => takeAim(monster)
        case Behavior.Kiter if sees && distance <= 2 && rng.percent(70) => flee(monster)
        case Behavior.Kiter if sees && distance <= 7 && monster.cooldown == 0 =>
          monster.cooldown = 1
          events += Event.Shot(monster.pos, player.pos)
          strike(monster, player)
        case Behavior.Leviathan if sees && monster.cooldown == 0 =>
          monster.cooldown = 5
          surge(monster)
        case Behavior.Summoner if sees && monster.cooldown == 0 =>
          monster.cooldown = Tuning.summonTurns(kind)
          summon(monster)
        case _ if sees && distance == 1 => strike(monster, player)
        case _ => hunt(monster)
      }
    }
  }

  private def canSeePlayer(monster: Actor): Boolean = !player.has(Status.Submerged) &&
    monster.pos.distance(player.pos) <= 9 &&
      (monster.pos.distance(player.pos) <= 1 || Paths.line(monster.pos, player.pos).init.forall(p => !blocksSight(p)))

  private def blocksSight(p: Pos): Boolean =
    level(p).opaque && !(level(p) == Tile.Door && (player.pos == p || level.monsterAt(p).isDefined))

  private def free(p: Pos): Boolean = level.walkable(p) && p != player.pos && level.monsterAt(p).isEmpty

  private def hunt(monster: Actor): Unit = monster.hunting match {
    case Some(to) if to == monster.pos =>
      monster.hunting = None
      if(monster.kind.exists(_.boss)) goHome(monster) else wander(monster)
    case Some(to) =>
      def open(p: Pos) = free(p) && (level.fire(level.index(p)) == 0 || monster.has(Status.Burning))
      Paths.find(level.width, level.height, monster.pos, to)(open).headOption.filter(free) match {
        case Some(next) => move(monster, next)
        case None => wander(monster)
      }
    case None if monster.kind.exists(_.boss) => goHome(monster)
    case None => if(rng.percent(50)) wander(monster)
  }

  /** A keeper that lost the player goes back to what it keeps. */
  private def goHome(monster: Actor): Unit = {
    def open(p: Pos) = free(p) && level.fire(level.index(p)) == 0
    if(monster.pos != level.exit)
      Paths.find(level.width, level.height, monster.pos, level.exit)(open).headOption.filter(free).foreach(move(monster, _))
  }

  private def wander(monster: Actor): Unit = {
    val choices = monster.pos.neighbors.filter(p => free(p) && level.fire(level.index(p)) == 0)
    if(choices.nonEmpty) move(monster, rng.pick(choices))
  }

  private def flee(monster: Actor): Unit = {
    val choices = monster.pos.neighbors.filter(p => free(p) && level.fire(level.index(p)) == 0)
    if(choices.nonEmpty) move(monster, choices.maxBy(_.distance(player.pos)))
    else if(monster.pos.distance(player.pos) == 1) strike(monster, player)
  }

  // What it aims at is shown, and can be stepped out of.
  private def takeAim(monster: Actor): Unit = {
    val d = player.pos - monster.pos
    monster.aim = Paths.line(monster.pos, Pos(monster.pos.x + d.x * 4, monster.pos.y + d.y * 4)).take(10).takeWhile(level.walkable)
    monster.cooldown = 3
    say(s"${monster.kind.get.named} turns to you.")
  }

  private def fire(monster: Actor): Unit = {
    val cells = monster.aim
    monster.aim = Vector.empty
    cells.lastOption.foreach(events += Event.Shot(monster.pos, _))
    (player +: level.monsters.toVector).filter(a => a.id != monster.id && cells.contains(a.pos)).foreach { hit =>
      val damage = scala.math.max(1, rng.between(Tuning.mighty(6, depth), Tuning.mighty(11, depth)) - (if(hit.isPlayer) rng.between(0, armor) else 0))
      events += Event.Struck(monster.id, hit.id, hit.pos, damage)
      harm(hit, damage, monster.kind.get.named)
    }
  }

  private def surge(monster: Actor): Unit = {
    (monster.pos +: monster.pos.neighbors).foreach { p =>
      if(level(p) == Tile.Floor || level(p) == Tile.Reeds || level(p) == Tile.Ash) {
        level(p) = Tile.Water
        level.fire(level.index(p)) = 0
      }
    }
    if(monster.pos.distance(player.pos) == 1) {
      val away = player.pos - monster.pos
      if(free(player.pos + away)) move(player, player.pos + away)
      strike(monster, player)
    } else hunt(monster)
  }

  private def summon(by: Actor): Unit = {
    val choices = by.pos.neighbors.filter(free)
    if(choices.nonEmpty) {
      val called = if(by.kind.contains(MonsterKind.Archivist)) MonsterKind.LanternBat else MonsterKind.InkSlug
      val slug = new Actor(nextId(), Some(called), rng.pick(choices), called.hp)
      slug.time = by.time
      slug.hunting = Some(player.pos)
      level.monsters += slug
      events += Event.Appeared(slug.id, slug.pos)
      say(s"${by.kind.get.named} calls something.")
    }
  }

  // ---- Places ----

  private def arrive(to: Int): Unit = {
    val down = to > depth
    // What is close and awake comes along.
    val followers =
      if(depth == 0) Vector.empty
      else level.monsters.filter(m => !m.asleep && !m.has(Status.Stilled) && m.pos.distance(player.pos) == 1 && !m.kind.exists(_.boss)).toVector
    if(depth > 0) {
      level.leftAt = player.time
      level.monsters --= followers
    }
    depth = to
    val visited = floors.get(to)
    level = visited.getOrElse {
      val made = Generator.level(to, rng, () => nextId())
      // The clocks of a new floor start with the player's.
      made.monsters.foreach(_.time += player.time)
      floors(to) = made
      made
    }
    visited.foreach(catchUp)
    player.pos = if(down) level.start else level.exit
    level.monsterAt(player.pos).foreach(place(_, player.pos))
    followers.foreach { monster =>
      level.monsters += monster
      place(monster, player.pos)
      monster.hunting = Some(player.pos)
    }
    player.statuses.remove(Status.Burning)
    nextTick = scala.math.max(nextTick, player.time)
    events += Event.Arrived(depth)
    say(s"${level.biome.title}, floor $depth.")
    look()
  }

  /** Puts a monster on a free cell, as close as possible. */
  private def place(monster: Actor, near: Pos): Unit = {
    val reach = Paths.distances(level.width, level.height, near)(level.walkable)
    level.positions.filter(p => reach(level.index(p)) > 0 && free(p)).minByOption(p => reach(level.index(p)))
      .foreach(monster.pos = _)
  }

  // Time passed while a floor was not played.
  private def catchUp(floor: Level): Unit = {
    val away = player.time - floor.leftAt
    if(away >= 1000) {
      floor.fire.indices.foreach(floor.fire(_) = 0)
      floor.gas.indices.foreach(floor.gas(_) = 0)
    }
    floor.monsters.foreach { monster =>
      monster.time += away
      monster.statuses.clear()
      monster.hp = scala.math.min(monster.maxHp, monster.hp + away / 1000)
      if(away >= 3000) monster.hunting = None
    }
    // Others have moved in.
    if(away >= 20000 && floor.monsters.size < 4) {
      val kind = Generator.monsterFor(depth, rng)
      val monster = new Actor(nextId(), Some(kind), floor.exit, Tuning.mighty(kind.hp, depth))
      monster.time = player.time + rng.int(100)
      val choices = floor.reachable.filter(p => floor.monsterAt(p).isEmpty && p != floor.start && p != floor.exit)
      monster.pos = rng.pick(choices)
      floor.monsters += monster
    }
  }

  /** Who lingers is found. */
  private def wander(): Unit =
    if((nextTick / 100) % Tuning.WanderTurns == 0 && level.wanderers < Tuning.MaxWanderers) {
      val from = level.reachable.filter(p => free(p) && !visible(p) && p.distance(player.pos) > 10)
      if(from.nonEmpty) {
        level.wanderers += 1
        val kind = Generator.monsterFor(depth, rng)
        val monster = new Actor(nextId(), Some(kind), rng.pick(from), Tuning.mighty(kind.hp, depth))
        monster.time = player.time
        monster.hunting = Some(player.pos)
        level.monsters += monster
        events += Event.Appeared(monster.id, monster.pos)
      }
    }

  /** The water comes after who took the Lodestar, and what lives in it. */
  private def rise(): Unit = {
    val shore = level.positions.filter { p =>
      (level(p) == Tile.Floor || level(p) == Tile.Reeds || level(p) == Tile.Ash) &&
        (p.distance(level.exit) <= 1 || Pos.Cardinal.exists(d => level(p + d) == Tile.Water))
    }.toVector
    if(shore.nonEmpty) (0 until 3).foreach { _ =>
      val p = rng.pick(shore)
      level(p) = Tile.Water
      level.fire(level.index(p)) = 0
    }
    if(rng.percent(3) && level.monsters.size < 14) {
      val from = level.positions.filter(p => level(p) == Tile.Water && free(p) && p.distance(player.pos) > 4).toVector
      if(from.nonEmpty) {
        val kind = if(depth >= 5 || rng.percent(30)) MonsterKind.ReedEel else MonsterKind.InkSlug
        val monster = new Actor(nextId(), Some(kind), rng.pick(from), Tuning.mighty(kind.hp, depth))
        monster.time = player.time
        monster.hunting = Some(player.pos)
        level.monsters += monster
        events += Event.Appeared(monster.id, monster.pos)
      }
    }
  }

  private[model] def look(): Unit = {
    visible = Fov.compute(player.pos, sight)(p => blocksSight(p) && p != player.pos).filter(level.contains)
    visible.foreach(p => level.explored(level.index(p)) = true)
    met ++= visibleMonsters.flatMap(_.kind)
  }

  private def say(text: String): Unit = events += Event.Message(text.take(1).toUpperCase + text.drop(1))
}

object Game {
  /** The game that was saved, unless the save makes no sense. */
  def restore(save: String): Option[Game] = Codec.read(save)
}
