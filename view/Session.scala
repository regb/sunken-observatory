package observatory
package core

import scala.collection.mutable

import sgl.engine.gameobject._
import sgl.engine.gameobject.extras.{Animation, ParticleSpec, Particles, UiScroll}
import sgl.engine.gameobject.render2d.{Camera2D, SpriteRenderer, Texture2D, Tilemap}
import sgl.engine.gameobject.spatial2d._
import observatory.model._
import sgl.geometry.Vec2
import sgl.util.Loader

enum Track { case Surface, Depths }
enum SoundName { case Step, Hit, Miss, Hurt, Pickup, Mend, Burst, Descend, Rank }

/** What the game uses of the platform it runs on. */
trait Services {
  def screen: Vec2
  def loadSheet(context: GameContext)(using Scope): Loader[Texture2D]
  def play(sound: SoundName): Unit
  /** Loops a piece, unless it already does. */
  def music(track: Track): Unit
  def settings: Settings
  def changeSettings(settings: Settings): Unit
  def records: Vector[Record]
  def addRecord(record: Record): Unit
  /** The hints that were told, in any game. */
  def told: Set[Hint]
  def tell(hint: Hint): Unit
  def savedGame: Option[String]
  def saveGame(save: String): Unit
  def newSeed(): Long
}

/** What the player is doing. */
enum Mode {
  case Playing
  case Pack(selected: Option[Int])
  case Aiming(slot: Int)
  case Journal
  case Menu
  case Over
}

/** What a scene shows of one actor. It has a queue of movements to play. */
final class ActorView(obj: TypedObject[Spatial2D & SpriteRenderer], val actor: Actor, color: Color, sheet: Texture2D)
    extends ComponentOf(obj) {
  private val spatial = obj.spatial
  private val renderer = obj.get[SpriteRenderer]
  private val sprites: Animation[Sprite] = actor.kind.map(Sprite.animated).getOrElse(Animation(Vector(Sprite.Player), 1f))
  private val moves = mutable.Queue.empty[ActorView.Move]
  private var elapsed = 0f
  private var flash = 0f
  private var dying = -1f
  private var shown = 0f
  private var age = 0f

  def position: Vec2 = spatial.local.position
  def busy: Boolean = moves.nonEmpty
  def lag: Int = moves.size

  def walk(from: Pos, to: Pos): Unit = moves += ActorView.Move(ActorView.center(from), ActorView.center(to), 0.11f, bump = false)
  def bump(from: Pos, toward: Pos): Unit = {
    val a = ActorView.center(from)
    moves += ActorView.Move(a, a + (ActorView.center(toward) - a) * 0.45f, 0.14f, bump = true)
  }
  def place(at: Pos): Unit = {
    moves.clear()
    spatial.local = spatial.local.copy(position = ActorView.center(at))
  }
  def hit(): Unit = flash = 1f
  def die(): Unit = dying = 0f

  def update(dt: Float, inSight: Boolean): Unit = {
    // Catches up when there is more to show than time to show it.
    var left = dt * scala.math.max(1f, moves.size / 2f)
    while(moves.nonEmpty && left > 0f) {
      val move = moves.head
      val used = scala.math.min(left, move.seconds - elapsed)
      elapsed += used
      left -= used
      val t = scala.math.min(1f, elapsed / move.seconds)
      val at = if(move.bump) (if(t < 0.5f) t * 2f else 2f - t * 2f) else t
      spatial.local = spatial.local.copy(position = move.from + (move.to - move.from) * at)
      if(elapsed >= move.seconds) {
        val _ = moves.dequeue()
        elapsed = 0f
      }
    }
    flash = scala.math.max(0f, flash - dt * 5f)
    shown = scala.math.max(0f, scala.math.min(1f, shown + (if(inSight) dt else -dt) * 6f))
    if(dying >= 0f) {
      dying += dt * 3f
      if(dying >= 1f) this.destroy()
    }
    val alpha = shown * (if(dying >= 0f) 1f - scala.math.min(1f, dying) else 1f)
    val resting = if(actor.asleep || actor.has(Status.Stilled)) 0.6f else 1f
    def mix(from: Float, to: Float, t: Float) = from + (to - from) * t
    val tinted = Palette.shade(color, resting)
    renderer.visual = sprites.at(if(actor.asleep) 0f else age).visual(sheet)
    renderer.color = Color(mix(tinted.red, 1f, flash), mix(tinted.green, 0.3f, flash), mix(tinted.blue, 0.25f, flash), alpha)
    spatial.local = spatial.local.copy(scale = {
      val size = 1f + flash * 0.15f + (if(dying > 0f) dying * 0.4f else 0f)
      // Breathing, each in its own time.
      age += dt
      val breath = if(dying >= 0f || actor.has(Status.Stilled)) 0f else scala.math.sin(age * (if(actor.asleep) 1.5 else 3.2) + actor.id).toFloat * 0.035f
      Vec2(size - breath * 0.5f, size + breath)
    })
  }
}

object ActorView {
  private final case class Move(from: Vec2, to: Vec2, seconds: Float, bump: Boolean)
  def center(p: Pos): Vec2 = Vec2(p.x * 16f + 8f, p.y * 16f + 8f)
}

/** Something short-lived: rising text, sparks, bolts. */
final class Effect(val seconds: Float, val text: Option[String], val color: Color, var at: Vec2, val velocity: Vec2, val size: Float) {
  var age: Float = 0f
}

/** Everything a game being played has that is not in the rules. */
final class Session(val game: Game, val services: Services, val sheet: Texture2D) {
  val controls: Controls = new Controls
  var mode: Mode = Mode.Playing
  val views: mutable.Map[Int, ActorView] = mutable.Map.empty
  val particles: Particles = new Particles
  val effects: mutable.ArrayBuffer[Effect] = mutable.ArrayBuffer.empty
  val log: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty
  val journal: UiScroll = new UiScroll
  /** The direction key being held, for how long, and whether it made a step. */
  var heldDirection: Option[Pos] = None
  var heldFor: Float = 0f
  var heldStepped: Boolean = false
  /** Where the player is headed, one step a turn. */
  var travel: Vector[Pos] = Vector.empty
  var endTravel: Option[Command] = None
  var pan: Vec2 = Vec2(0f, 0f)
  var zoom: Float = 0f
  var shake: Float = 0f
  var smoulderDue: Float = 0f
  var dirty: Boolean = true
  /** Seconds since the last message, to fade them. */
  var quiet: Float = 0f
  var fade: Float = 1f
  var ending: Option[String] = None
  /** The hint being told, and for how many more seconds. */
  var hint: Option[Hint] = None
  var hintLeft: Float = 0f
  /** Taps that no panel or button took. */
  var taps: Vector[Vec2] = Vector.empty
  var gestures: Vector[Gesture] = Vector.empty

  var camera: Camera2D = scala.compiletime.uninitialized
  var cameraSpatial: Spatial2D = scala.compiletime.uninitialized
  var ground: Tilemap = scala.compiletime.uninitialized
  var objects: Tilemap = scala.compiletime.uninitialized
  var air: Tilemap = scala.compiletime.uninitialized

  /** Pixels of the screen for a unit of the interface. */
  def unit: Float = scala.math.max(1f, (scala.math.min(services.screen.x / 400f, services.screen.y / 300f) * 2f).round / 2f)
  def busy: Boolean = views.valuesIterator.exists(_.lag > 1) || views.get(game.player.id).exists(_.busy)
  def settled: Boolean = !views.valuesIterator.exists(_.busy)

  def say(text: String): Unit = {
    log += text
    quiet = 0f
    if(log.size > 60) log.remove(0, log.size - 60)
  }

  def float(text: String, at: Vec2, color: Color): Unit =
    effects += new Effect(0.8f, Some(text), color, at - Vec2(0f, 8f), Vec2(0f, -18f), 8f)

  def sparks(at: Vec2, color: Color, count: Int, speed: Float): Unit =
    particles.emit(
      ParticleSpec(
        minSeconds = 0.25f, maxSeconds = 0.55f, minSpeed = speed * 0.4f, maxSpeed = speed, gravity = Vec2(0f, 60f), drag = 1.5f,
        startSize = 2.4f, endSize = 0.8f, startColor = color, endColor = color.withAlpha(0f)
      ),
      at, count
    )

  /** What rises from a tile in the air. */
  def smoulder(at: Vec2, color: Color): Unit =
    particles.emit(
      ParticleSpec(
        minSeconds = 0.5f, maxSeconds = 1.1f, minSpeed = 6f, maxSpeed = 16f, direction = -scala.math.Pi.toFloat / 2f, spread = 0.6f,
        startSize = 2.2f, endSize = 0.6f, startColor = color, endColor = color.withAlpha(0f)
      ),
      at, 1
    )

  def bolt(from: Vec2, to: Vec2, color: Color): Unit = {
    val seconds = 0.16f
    effects += new Effect(seconds, None, color, from, (to - from) * (1f / seconds), 4f)
  }
}
