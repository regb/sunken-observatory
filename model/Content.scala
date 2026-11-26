package observatory
package model

// What the observatory holds. Adding a monster or an item is adding a case.

enum Biome(val title: String) {
  case Archive extends Biome("The Archive")
  case Orrery extends Biome("The Orrery")
  case Cistern extends Biome("The Cistern")
  case StarWell extends Biome("The Star-Well")
}

object Biome {
  val LastDepth: Int = 8
  def at(depth: Int): Biome = Biome.fromOrdinal(scala.math.min((depth - 1) / 2, Biome.values.length - 1))
}

enum Tile(val walkable: Boolean, val opaque: Boolean, val flammable: Boolean = false) {
  case Rock extends Tile(false, true)
  case Wall extends Tile(false, true)
  case Floor extends Tile(true, false)
  case Door extends Tile(true, true, flammable = true)
  case Water extends Tile(true, false)
  case Reeds extends Tile(true, false, flammable = true)
  case Ash extends Tile(true, false)
  case Shelf extends Tile(false, true, flammable = true)
  case Pillar extends Tile(false, true)
  case Stairs extends Tile(true, false)
  case StairsUp extends Tile(true, false)
  /** Opened by a key. */
  case LockedDoor extends Tile(false, true)
  /** Looks like a wall until it is found. */
  case SecretDoor extends Tile(false, true)
  /** Where the trader stands. */
  case Trader extends Tile(false, false)
}

enum Trap(val title: String) {
  case Cinder extends Trap("cinder vent")
  case Miasma extends Trap("miasma vent")
  case Alarm extends Trap("alarm bell")
}

enum Status(val title: String) {
  case Burning extends Status("burning")
  case Poisoned extends Status("poisoned")
  case Hasted extends Status("quickened")
  case Stilled extends Status("stilled")
  case Submerged extends Status("under water")
}

/** Who goes down. Each has something that only they can do.
  *
  * @param rest turns before it can be done again
  */
enum Calling(val title: String, val about: String, val hp: Int, val ability: String, val rest: Int) {
  case Surveyor extends Calling("Surveyor", "Takes bearings: learns the ground nearby, its traps and its secrets.", 27, "Bearings", 45)
  case Diver extends Calling("Diver", "Dives in water: loses pursuers, and strikes out of it twice as hard.", 32, "Dive", 25)
  case Lamplighter extends Calling("Lamplighter", "Burns less oil, and can spend some on a flare that stills all who see it.", 22, "Flare", 50)
}

enum Behavior(val about: String) {
  case Melee extends Behavior("Comes straight at you.")
  case Erratic extends Behavior("Flies about.")
  case Ranged extends Behavior("Shoots from a distance.")
  case Swimmer extends Behavior("Quicker in water.")
  case Venomous extends Behavior("Poisons.")
  case Summoner extends Behavior("Calls others.")
  case Thief extends Behavior("Takes something and runs.")
  case Splitter extends Behavior("Breaks in two when it is hurt.")
  case Kiter extends Behavior("Shoots, and keeps away.")
  case Beam extends Behavior("Aims for a turn, then burns everything in the line.")
  case Leviathan extends Behavior("Brings the water along.")
}

/** @param speed 100 acts as often as a player. */
enum MonsterKind(
    val title: String, val depth: Int, val hp: Int, val minDamage: Int, val maxDamage: Int,
    val armor: Int, val skill: Int, val evasion: Int, val speed: Int, val xp: Int, val behavior: Behavior
) {
  case BrassCrab extends MonsterKind("brass crab", 1, 8, 1, 3, 1, 8, 2, 100, 2, Behavior.Melee)
  case LanternBat extends MonsterKind("lantern bat", 1, 5, 1, 2, 0, 8, 7, 160, 3, Behavior.Erratic)
  case InkSlug extends MonsterKind("ink slug", 2, 12, 1, 4, 0, 9, 2, 70, 4, Behavior.Venomous)
  case DrownedScholar extends MonsterKind("drowned scholar", 3, 16, 2, 6, 1, 11, 5, 100, 6, Behavior.Melee)
  case Sentry extends MonsterKind("sentry automaton", 4, 18, 2, 5, 2, 11, 3, 100, 8, Behavior.Ranged)
  case ReedEel extends MonsterKind("reed eel", 5, 20, 3, 7, 1, 13, 8, 110, 9, Behavior.Swimmer)
  case TideSpirit extends MonsterKind("tide spirit", 6, 22, 3, 8, 2, 14, 9, 100, 11, Behavior.Venomous)
  case Guardian extends MonsterKind("orrery guardian", 7, 36, 5, 10, 4, 14, 4, 80, 15, Behavior.Melee)
  case Curator extends MonsterKind("the Curator", 99, 80, 6, 12, 3, 16, 6, 100, 60, Behavior.Summoner)
  case Mudlark extends MonsterKind("mudlark", 3, 14, 1, 3, 0, 12, 10, 120, 6, Behavior.Thief)
  case Mercury extends MonsterKind("mercury blob", 4, 20, 2, 5, 0, 11, 4, 90, 7, Behavior.Splitter)
  case GlassArcher extends MonsterKind("glass archer", 5, 14, 2, 6, 0, 13, 7, 100, 9, Behavior.Kiter)
  case Archivist extends MonsterKind("the Archivist", 99, 22, 1, 4, 0, 9, 4, 100, 16, Behavior.Summoner)
  case Engine extends MonsterKind("the Great Orrery", 99, 48, 4, 8, 4, 12, 2, 80, 30, Behavior.Beam)
  case Leviathan extends MonsterKind("the Leviathan", 99, 64, 4, 10, 2, 14, 6, 100, 45, Behavior.Leviathan)

  /** One at the end of every other floor, which keeps the stairs shut. */
  def boss: Boolean = depth == 99
  /** With its article. */
  def named: String = if(title.startsWith("the ")) title else s"the $title"

}

enum VialEffect(val title: String, val about: String) {
  case Mending extends VialEffect("mending", "Heals most wounds and cures poison.")
  case Cinder extends VialEffect("cinders", "Bursts into flames. Better thrown.")
  case Miasma extends VialEffect("miasma", "Releases a poisonous cloud. Better thrown.")
  case Quickening extends VialEffect("quickening", "You act twice as often for a while.")
  case Vigor extends VialEffect("vigor", "Makes you hardier for good.")
}

enum ChartEffect(val title: String, val about: String) {
  case Atlas extends ChartEffect("the atlas", "Shows the whole floor.")
  case Passage extends ChartEffect("passage", "Takes you somewhere else on the floor.")
  case Tempering extends ChartEffect("tempering", "Improves your instrument or your coat.")
  case Stillness extends ChartEffect("stillness", "Stills everything in sight.")
  case Undertow extends ChartEffect("the undertow", "Floods the ground, and shoves back what stands near.")
}

enum LensEffect(val title: String, val about: String) {
  case Farsight extends LensEffect("farsight", "You see further.")
  case Warding extends LensEffect("warding", "Turns some blows aside.")
  case Keenness extends LensEffect("keenness", "Your blows land more often.")
}

/** Bolts of light, from a prism that slowly fills again. */
enum PrismEffect(val title: String, val about: String) {
  case Frost extends PrismEffect("frost", "Harms, and stills for a moment.")
  case Lance extends PrismEffect("the lance", "Harms everything in a line.")
  case Tether extends PrismEffect("the tether", "Trades places with who it strikes.")
  case Gust extends PrismEffect("the gust", "Throws back, and harder against a wall.")
}

/** Marks on gear, good and bad. What is cursed cannot be taken off. */
enum Etching(val title: String, val about: String, val forCoats: Boolean, val cursed: Boolean) {
  case Searing extends Etching("searing", "Its blows can set on fire.", false, false)
  case Thirsting extends Etching("thirsting", "Its blows give back some health.", false, false)
  case Ringing extends Etching("ringing", "Its blows can still.", false, false)
  case Leaden extends Etching("leaden", "Cursed: every blow takes longer.", false, true)
  case Thorned extends Etching("thorned", "Who strikes you is hurt.", true, false)
  case Steadfast extends Etching("steadfast", "Stops more.", true, false)
  case Beckoning extends Etching("beckoning", "Cursed: it wakes what sleeps.", true, true)
}

enum ItemKind(val title: String, val depth: Int, val about: String) {
  /** @param time how long a blow takes, 100 being a turn */
  case Instrument(name: String, from: Int, min: Int, max: Int, time: Int)
      extends ItemKind(name, from, s"Deals $min to $max damage." + (if(time > 100) " Slow." else if(time < 100) " Quick." else ""))
  case Coat(name: String, from: Int, armor: Int) extends ItemKind(name, from, s"Stops up to $armor damage.")
  case Lens(effect: LensEffect) extends ItemKind(s"lens of ${effect.title}", 2, effect.about)
  case Vial(effect: VialEffect) extends ItemKind(s"vial of ${effect.title}", 1, effect.about)
  case Chart(effect: ChartEffect) extends ItemKind(s"chart of ${effect.title}", 1, effect.about)
  case Prism(effect: PrismEffect) extends ItemKind(s"prism of ${effect.title}", 2, effect.about)
  case Oil extends ItemKind("flask of oil", 1, "Fills the lamp. Without light you hardly see.")
  case Key extends ItemKind("vault key", 1, "Opens a locked door, once.")
  case Coins extends ItemKind("coins", 1, "Somebody down here may still take them.")
  case Lodestar extends ItemKind("the Lodestar", 99, "What you came down for.")

  def stacks: Boolean = this match {
    case _: Vial | _: Chart | Oil | Key | Coins => true
    case _ => false
  }
}

object ItemKind {
  val Dividers: ItemKind.Instrument = ItemKind.Instrument("pair of dividers", 1, 1, 5, 100)
  val OilCoat: ItemKind.Coat = ItemKind.Coat("oilskin coat", 1, 1)

  val instruments: Vector[ItemKind] = Vector(
    Dividers,
    ItemKind.Instrument("plumb bob", 2, 1, 4, 60),
    ItemKind.Instrument("brass sextant", 3, 2, 8, 100),
    ItemKind.Instrument("meridian rod", 4, 5, 13, 150),
    ItemKind.Instrument("astrolabe blade", 6, 4, 12, 100)
  )
  val coats: Vector[ItemKind] = Vector(
    OilCoat,
    ItemKind.Coat("quilted coat", 2, 2),
    ItemKind.Coat("diving coat", 4, 4),
    ItemKind.Coat("brass carapace", 6, 6)
  )
  val lenses: Vector[ItemKind] = LensEffect.values.toVector.map(ItemKind.Lens(_))
  val prisms: Vector[ItemKind] = PrismEffect.values.toVector.map(ItemKind.Prism(_))
  val vials: Vector[ItemKind] = VialEffect.values.toVector.map(ItemKind.Vial(_))
  val charts: Vector[ItemKind] = ChartEffect.values.toVector.map(ItemKind.Chart(_))

  /** Every kind there is, in an order that saves rely on. */
  val all: Vector[ItemKind] = instruments ++ coats ++ lenses ++ vials ++ charts ++
    prisms ++ Vector(ItemKind.Oil, ItemKind.Key, ItemKind.Coins, ItemKind.Lodestar)

  /** What unknown vials and charts look like, dealt out anew in every game. */
  val VialLooks: Vector[String] = Vector("amber", "teal", "violet", "milky", "rust")
  val ChartLooks: Vector[String] = Vector("the Heron", "the Anvil", "the Twins", "the Ladder", "the Moth")
}

object Price {
  def of(item: Item): Int = item.kind match {
    case _: ItemKind.Vial => 18
    case _: ItemKind.Chart => 22
    case _: ItemKind.Lens => 45
    case _: ItemKind.Prism => 60
    case kind: ItemKind.Instrument => 12 + kind.depth * 9 + item.upgrade * 15
    case kind: ItemKind.Coat => 12 + kind.depth * 9 + item.upgrade * 15
    case _ => 15
  }
}

/** @param charge how often a prism can be used before it has to fill again
  * @param worn whether it was ever worn, which shows what is etched on it
  */
final case class Item(
    kind: ItemKind, upgrade: Int = 0, count: Int = 1,
    etching: Option[Etching] = None, charge: Int = 0, worn: Boolean = false
) {
  def maxCharge: Int = 2 + upgrade
  def cursed: Boolean = etching.exists(_.cursed)
  def has(wanted: Etching): Boolean = etching.contains(wanted)
}
