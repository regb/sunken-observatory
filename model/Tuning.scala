package observatory
package model

/** The numbers that decide how hard the game is. `bazel run :balance` measures
  * what they make of it.
  */
object Tuning {
  val StartOil: Int = 600
  val MaxOil: Int = 1000
  val OilFlask: Int = 280
  /** Chance of a second flask on a floor. */
  val ExtraOil: Int = 20

  /** What a flare burns, and for how long it stills. */
  val FlareOil: Int = 80
  val FlareTurns: Int = 5

  /** Turns to get a point of health back, as long as the lamp burns. */
  val RegenTurns: Int = 20
  /** What a rank takes, for each rank so far, and what it gives. */
  val RankExperience: Int = 10
  val RankHealth: Int = 3

  /** Brass for a chart of tempering. */
  val BrassForChart: Int = 20

  /** Turns between monsters that come looking for who lingers, and how many. */
  val WanderTurns: Int = 110
  val MaxWanderers: Int = 6

  /** Percent added to the damage and the health of monsters, by floor. */
  val Might: Vector[Int] = Vector(-20, -10, 10, 25, 48, 66, 88, 105)
  def mighty(value: Int, depth: Int): Int = value + (value * Might(scala.math.max(0, scala.math.min(Might.size, depth) - 1)) + 50) / 100

  /** How dense the cloud of a vial and of a vent is, and how long poison lasts. */
  val VialGas: Int = 55
  val VentGas: Int = 40
  val VenomTurns: Int = 3

  /** Turns between what a summoner calls. */
  def summonTurns(kind: MonsterKind): Int = if(kind == MonsterKind.Archivist) 18 else 8
}
