package observatory
package model

/** How a game ended. */
final case class Record(won: Boolean, calling: Calling, depth: Int, rank: Int, turns: Int, seed: Long, cause: String)

object Records {
  val Kept: Int = 12

  def of(game: Game, cause: String): Record =
    Record(game.outcome == Outcome.Won, game.calling, game.depth, game.rank, game.turns, game.seed, cause)

  /** Best first: the ones that were won, in fewer turns, then the deepest. */
  def add(records: Vector[Record], record: Record): Vector[Record] =
    (records :+ record).sortBy(r => (!r.won, if(r.won) r.turns else -r.depth * 1000 - r.rank)).take(Kept)

  // A record a line, and the cause last, as it can be anything.
  def write(records: Vector[Record]): String = records.map { r =>
    s"${if(r.won) 1 else 0};${r.calling.ordinal};${r.depth};${r.rank};${r.turns};${r.seed};${r.cause.replace("\n", " ")}"
  }.mkString("\n")

  def read(text: String): Vector[Record] = text.split("\n").toVector.flatMap { line =>
    line.split(";", 7) match {
      case Array(won, calling, depth, rank, turns, seed, cause) =>
        try Calling.values.lift(calling.toInt).map(Record(won == "1", _, depth.toInt, rank.toInt, turns.toInt, seed.toLong, cause))
        catch { case _: NumberFormatException => None }
      case _ => None
    }
  }
}

/** Something to learn, told once, when it first matters. */
enum Hint(val text: String) {
  case Walk extends Hint("Tap where you want to go, or use the arrows. Your lamp burns oil with every turn.")
  case Item extends Hint("Step on things and take them with G, or the button below.")
  case Monster extends Hint("Walk into a monster to strike it. What sleeps is struck twice as hard.")
  case Unknown extends Hint("You do not know what this is until you use it. Vials can also be thrown.")
  case Stairs extends Hint("The stairs lead on. Floors stay as you leave them.")
  case Keeper extends Hint("A keeper shuts the way down until it falls. Step out of what glows red.")
  case LowOil extends Hint("Your lamp burns low. Fill it with a flask: in the dark you see little, and do not heal.")
  case Locked extends Hint("A vault. Its key is somewhere on this floor.")
  case Trader extends Hint("Step on what the trader sells to buy it, and drop what you want to sell close by.")
  case Hurt extends Hint("You heal slowly while your lamp burns, but who lingers is found. A vial of mending is quicker.")
  case Flood extends Hint("Climb! The water rises behind you, and brings what lives in it.")
}

object Hint {
  /** The first that matters now and was not told. */
  def due(game: Game, told: Set[Hint]): Option[Hint] = {
    val level = game.level
    val seen = game.visible
    def near(tile: Tile) = seen.exists(level(_) == tile)
    def matters(hint: Hint): Boolean = hint match {
      case Walk => true
      case Flood => game.hasLodestar
      case Keeper => game.visibleMonsters.exists(_.kind.exists(_.boss))
      case Monster => game.visibleMonsters.nonEmpty
      case Hurt => game.player.hp * 2 < game.player.maxHp
      case LowOil => game.oil <= 200
      case Item => seen.exists(p => level.items.contains(p) && !level.wares.contains(p))
      case Unknown => game.pack.exists(item => !game.isKnown(item.kind))
      case Locked => near(Tile.LockedDoor)
      case Trader => near(Tile.Trader)
      case Stairs => near(Tile.Stairs)
    }
    // What is urgent comes first.
    Vector(Flood, Keeper, Monster, Hurt, LowOil, Walk, Item, Unknown, Locked, Trader, Stairs).find(hint => !told(hint) && matters(hint))
  }
}
