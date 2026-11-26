package observatory
package model

import scala.collection.mutable

final class Actor(val id: Int, val kind: Option[MonsterKind], var pos: Pos, var maxHp: Int) {
  var hp: Int = maxHp
  /** When it acts next. */
  var time: Int = 0
  var asleep: Boolean = false
  /** Where it last knew the player to be. */
  var hunting: Option[Pos] = None
  var cooldown: Int = 0
  /** What it stole. */
  var loot: Option[Item] = None
  /** Cells it is about to strike. */
  var aim: Vector[Pos] = Vector.empty
  val statuses: mutable.Map[Status, Int] = mutable.Map.empty

  def isPlayer: Boolean = kind.isEmpty
  def alive: Boolean = hp > 0
  def has(status: Status): Boolean = statuses.contains(status)
  def title: String = kind.map(_.title).getOrElse("you")
}

final case class PlacedTrap(kind: Trap, hidden: Boolean)

final class Level(val width: Int, val height: Int, val biome: Biome) {
  private val tiles = Array.fill[Tile](width * height)(Tile.Rock)
  val explored: Array[Boolean] = Array.fill(width * height)(false)
  /** Turns a cell still burns, and how dense its miasma is. */
  val fire: Array[Int] = Array.fill(width * height)(0)
  val gas: Array[Int] = Array.fill(width * height)(0)
  val traps: mutable.Map[Pos, PlacedTrap] = mutable.Map.empty
  val items: mutable.Map[Pos, List[Item]] = mutable.Map.empty
  /** How many came looking for the player. */
  var wanderers: Int = 0
  /** What is for sale, and for how much. */
  val wares: mutable.Map[Pos, Int] = mutable.Map.empty
  val monsters: mutable.ArrayBuffer[Actor] = mutable.ArrayBuffer.empty
  /** Where the stairs up are, and the ones down, or what is at the very bottom. */
  var start: Pos = Pos(0, 0)
  var exit: Pos = Pos(0, 0)
  /** The clock of the game when it was last left. */
  var leftAt: Int = 0
  /** Cells that the tide has flooded, and will leave. */
  val tidal: mutable.Set[Pos] = mutable.Set.empty

  def contains(p: Pos): Boolean = p.x >= 0 && p.y >= 0 && p.x < width && p.y < height
  def index(p: Pos): Int = p.y * width + p.x
  def apply(p: Pos): Tile = if(contains(p)) tiles(index(p)) else Tile.Rock
  def update(p: Pos, tile: Tile): Unit = if(contains(p)) tiles(index(p)) = tile
  def positions: Iterator[Pos] = for(y <- Iterator.range(0, height); x <- Iterator.range(0, width)) yield Pos(x, y)
  def walkable(p: Pos): Boolean = apply(p).walkable

  /** The cells that can be walked to, whatever is locked or secret staying so. */
  def reachable: Vector[Pos] = {
    val reach = Paths.distances(width, height, start)(walkable)
    positions.filter(p => walkable(p) && reach(index(p)) >= 0).toVector
  }

  def monsterAt(p: Pos): Option[Actor] = monsters.find(m => m.pos == p && m.alive)

  /** Puts an item on a cell, over the others, but never over the Lodestar. */
  def drop(p: Pos, item: Item): Unit = items.getOrElse(p, Nil) match {
    case lodestar :: others if lodestar.kind == ItemKind.Lodestar => items(p) = lodestar :: item :: others
    case others => items(p) = item :: others
  }

  /** Sets a cell on fire, unless something there does not burn. */
  def ignite(p: Pos, turns: Int): Unit = if(contains(p)) apply(p) match {
    case Tile.Rock | Tile.Wall | Tile.Pillar | Tile.Water | Tile.Stairs | Tile.StairsUp | Tile.LockedDoor | Tile.SecretDoor | Tile.Trader => ()
    case _ => fire(index(p)) = scala.math.max(fire(index(p)), turns)
  }

  def addGas(p: Pos, amount: Int): Unit = if(contains(p) && !blocksGas(p)) gas(index(p)) += amount

  private def blocksGas(p: Pos): Boolean = !walkable(p)

  /** Water comes and goes on the floors that have tides. */
  def tide(clock: Int): Unit = if(biome == Biome.Cistern || biome == Biome.StarWell) clock % 80 match {
    case 20 | 28 =>
      val shore = positions.filter(p => apply(p) == Tile.Floor && Pos.Cardinal.exists(d => apply(p + d) == Tile.Water)).toVector
      shore.foreach { p =>
        update(p, Tile.Water)
        fire(index(p)) = 0
        tidal += p
      }
    case 60 =>
      tidal.foreach(p => if(apply(p) == Tile.Water) update(p, Tile.Floor))
      tidal.clear()
    case _ => ()
  }

  /** Lets fire spread and miasma drift for a turn. */
  def tick(rng: Rng): Unit = {
    val burning = positions.filter(p => fire(index(p)) > 0).toVector
    burning.foreach { p =>
      val i = index(p)
      fire(i) -= 1
      // Miasma burns off.
      if(gas(i) > 0) gas(i) = gas(i) / 2
      if(fire(i) == 0 && apply(p).flammable) update(p, Tile.Ash)
      Pos.Cardinal.map(p + _).foreach { near =>
        if(apply(near).flammable && fire(index(near)) == 0 && rng.percent(45)) ignite(near, 4)
      }
    }

    val before = gas.clone()
    positions.filter(p => before(index(p)) > 0).foreach { p =>
      val i = index(p)
      val open = Pos.Cardinal.map(p + _).filter(n => contains(n) && !blocksGas(n))
      val share = before(i) / (open.size + 1)
      open.foreach { n =>
        gas(i) -= share
        gas(index(n)) += share
      }
    }
    gas.indices.foreach(i => if(gas(i) > 0) gas(i) -= (if(rng.percent(50)) 1 else 0))
  }
}
