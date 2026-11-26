package observatory
package core

import sgl.{Input, InputActions}
import sgl.engine.gameobject._
import sgl.engine.gameobject.render2d._
import sgl.engine.gameobject.spatial2d._
import observatory.model._
import sgl.geometry.Vec2
import sgl.util.Loader

/** A game being played. Its systems take turns at the same [[Session]]. */
final class PlayScene(game: Game) extends AsyncScene[Texture2D] {
  override def preload(context: GameContext)(using Scope): Loader[Texture2D] =
    context.global[Services].loadSheet(context)

  override def load(context: GameContext, sheet: Texture2D)(using scope: Scope): Unit = {
    val session = new Session(game, context.global[Services], sheet)
    context.setGlobal(session)
    Input.setInputProcessor(session.controls, Set(InputActions.Back))
    scope.onDestroy(() => Input.clearInputProcessor())

    val camera = context.spawn2D("camera", Transform2D(ActorView.center(game.player.pos)))
      .attach(new Camera2D(_, Vec2(320f, 200f)))
    session.camera = camera.get[Camera2D]
    session.cameraSpatial = camera.spatial
    session.ground = context.spawn2D("ground").attach(new Tilemap(_, Vector.empty, z = 0f)).get[Tilemap]
    session.objects = context.spawn2D("objects").attach(new Tilemap(_, Vector.empty, z = 1f)).get[Tilemap]
    session.air = context.spawn2D("air").attach(new Tilemap(_, Vector.empty, z = 3f)).get[Tilemap]

    session.services.music(Track.Depths)
    Turns.showFloor(context, session)
    Turns.present(context, session)

    context.addSystem(Stage.PreUpdate, GestureSystem)
    context.addSystem(Stage.Update, TurnSystem)
    context.addSystem(Stage.PostUpdate, ShowSystem)
    context.addSystem(Stage.PreRender, MapSystem)
    context.addSystem(Stage.Render, EffectRenderSystem)
    context.addSystem(Stage.Render, HudSystem)
  }

  // A phone may never come back from this.
  override def pause(context: GameContext)(using Scope): Unit = context.global[Session].store()
  override def unload(context: GameContext)(using Scope): Unit = context.global[Session].store()

  extension (session: Session) private def store(): Unit =
    session.services.saveGame(if(session.game.outcome == Outcome.Playing) session.game.save else "")
}

object GestureSystem extends GameSystem {
  override val id: SystemId = SystemId("observatory.gestures")

  override def run(context: GameContext)(using Scope): Unit = {
    val session = context.global[Session]
    session.gestures = session.controls.take()
  }
}

/** Turns what the player wants into commands, one turn at a time. */
object TurnSystem extends GameSystem {
  override val id: SystemId = SystemId("observatory.turns")

  override def run(context: GameContext)(using Scope): Unit = {
    val session = context.global[Session]
    val game = session.game
    session.gestures.foreach {
      case Gesture.Pan(by) =>
        if(session.mode == Mode.Playing || session.mode.isInstanceOf[Mode.Aiming]) {
          val scale = session.camera.viewportSize.x / session.services.screen.x
          session.pan = session.pan - by * scale
        }
      case Gesture.Zoom(factor) => if(session.mode == Mode.Playing || session.mode.isInstanceOf[Mode.Aiming]) session.zoom = scala.math.max(1f, scala.math.min(8f, session.zoom * factor))
      case Gesture.Key(key) => pressed(context, session, key)
      case Gesture.Back => back(context, session)
      case _: Gesture.Tap => ()
    }

    val taps = session.taps
    session.taps = Vector.empty
    taps.foreach { tap =>
      val world = session.camera.screenToWorld(tap, session.services.screen)
      tapped(context, session, Pos((world.x / 16f).floor.toInt, (world.y / 16f).floor.toInt))
    }

    // A key walks on when it is held, but not when it is just slow to come up.
    val held = heldDirection
    if(held != session.heldDirection) {
      session.heldDirection = held
      session.heldFor = 0f
      session.heldStepped = false
    } else session.heldFor += context.dt

    if(session.mode == Mode.Playing && !session.busy && game.outcome == Outcome.Playing) {
      held.filter(_ => session.settled && (!session.heldStepped || session.heldFor > 0.25f)) match {
        case Some(direction) =>
          session.heldStepped = true
          session.travel = Vector.empty
          val _ = Turns.perform(context, session, Command.Step(direction.x, direction.y))
        case None => travelOn(context, session)
      }
    }
  }

  private val Directions: Vector[(Input.Keys.Key, Pos)] = Vector(
    Input.Keys.Up -> Pos(0, -1), Input.Keys.W -> Pos(0, -1), Input.Keys.Down -> Pos(0, 1), Input.Keys.S -> Pos(0, 1),
    Input.Keys.Left -> Pos(-1, 0), Input.Keys.A -> Pos(-1, 0), Input.Keys.Right -> Pos(1, 0), Input.Keys.D -> Pos(1, 0),
    Input.Keys.Q -> Pos(-1, -1), Input.Keys.E -> Pos(1, -1), Input.Keys.Z -> Pos(-1, 1), Input.Keys.C -> Pos(1, 1)
  )

  private def heldDirection: Option[Pos] = Directions.find(d => Input.isKeyPressed(d._1)).map(_._2)

  private def pressed(context: GameContext, session: Session, key: Input.Keys.Key)(using Scope): Unit = {
    val acts = session.mode == Mode.Playing && !session.busy
    key match {
      case Input.Keys.Escape => back(context, session)
      case Input.Keys.I => session.mode match {
        case Mode.Playing => session.mode = Mode.Pack(None)
        case Mode.Pack(_) => session.mode = Mode.Playing
        case _ => ()
      }
      case Input.Keys.J => session.mode match {
        case Mode.Playing => session.mode = Mode.Journal
        case Mode.Journal => session.mode = Mode.Playing
        case _ => ()
      }
      case Input.Keys.Space => if(acts) { val _ = Turns.perform(context, session, Command.Wait) }
      case Input.Keys.G | Input.Keys.Enter | Input.Keys.NumpadEnter =>
        if(session.mode == Mode.Over) session.leave(context) else if(acts) Turns.interact(context, session)
      case Input.Keys.F => if(acts) { val _ = Turns.perform(context, session, Command.Search) }
      case Input.Keys.R => if(acts) { val _ = Turns.perform(context, session, Command.Ability) }
      case number: Input.Keys.NumKey => if(acts) Turns.quickUse(context, session, number.toInt - 1)
      case _ => ()
    }
  }

  private def back(context: GameContext, session: Session): Unit = session.mode match {
    case Mode.Pack(Some(_)) => session.mode = Mode.Pack(None)
    case Mode.Pack(None) | Mode.Aiming(_) | Mode.Journal | Mode.Menu => session.mode = Mode.Playing
    case Mode.Playing => if(session.travel.nonEmpty) session.travel = Vector.empty else session.mode = Mode.Menu
    case Mode.Over => session.leave(context)
  }

  private def tapped(context: GameContext, session: Session, at: Pos)(using Scope): Unit = {
    val game = session.game
    session.mode match {
      case Mode.Aiming(slot) =>
        val prism = game.pack.lift(slot).exists(_.kind.isInstanceOf[ItemKind.Prism])
        val command = if(prism) Command.Aim(slot, at.x, at.y) else Command.Throw(slot, at.x, at.y)
        if(Turns.perform(context, session, command) || prism) session.mode = Mode.Playing
        else session.say("You cannot throw it there.")
      case Mode.Playing =>
        session.pan = Vec2(0f, 0f)
        if(session.travel.nonEmpty) session.travel = Vector.empty
        else if(at == game.player.pos) {
          if(!session.busy) Turns.interact(context, session)
        } else {
          session.travel = game.pathTo(at)
          val takes = game.level.items.contains(at) && game.level.monsterAt(at).isEmpty
          session.endTravel = if(takes) Some(Command.PickUp) else None
        }
      case _ => ()
    }
  }

  private def travelOn(context: GameContext, session: Session)(using Scope): Unit = session.travel match {
    case next +: rest =>
      val game = session.game
      val before = game.visibleMonsters.map(_.id).toSet
      val health = game.player.hp
      val direction = next - game.player.pos
      val attacks = game.level.monsterAt(next).isDefined
      session.travel = Vector.empty
      if(Turns.perform(context, session, Command.Step(direction.x, direction.y)) && !attacks) {
        // Anything new is worth stopping for.
        val surprised = game.visibleMonsters.exists(m => !before(m.id)) || game.player.hp < health
        if(!surprised && game.player.pos == next) {
          if(rest.nonEmpty) session.travel = rest
          else session.endTravel.foreach { command =>
            session.endTravel = None
            val _ = Turns.perform(context, session, command)
          }
        }
      }
    case _ => ()
  }
}

/** Plays a turn and shows what came of it. */
object Turns {
  def perform(context: GameContext, session: Session, command: Command)(using Scope): Boolean = {
    val done = session.game.perform(command)
    if(done) session.pan = Vec2(0f, 0f)
    // Even what could not be done may have something to say.
    present(context, session)
    done
  }

  /** What there is to do right here. */
  def interact(context: GameContext, session: Session)(using Scope): Unit = {
    val game = session.game
    val command =
      if(game.level.items.contains(game.player.pos)) Command.PickUp
      else if(game.level(game.player.pos) == Tile.Stairs) Command.Descend
      else if(game.level(game.player.pos) == Tile.StairsUp) Command.Ascend
      else Command.Wait
    val _ = perform(context, session, command)
  }

  def quickUse(context: GameContext, session: Session, index: Int)(using Scope): Unit =
    if(session.game.pack.lift(index).exists(_.kind.isInstanceOf[ItemKind.Prism])) session.mode = Mode.Aiming(index)
    else if(session.game.pack.isDefinedAt(index)) { val _ = perform(context, session, Command.Use(index)) }

  /** Makes the objects of a floor anew. */
  def showFloor(context: GameContext, session: Session)(using Scope): Unit = {
    session.views.valuesIterator.foreach(_.destroy())
    session.views.clear()
    show(context, session, session.game.player)
    session.game.level.monsters.foreach(show(context, session, _))
    session.cameraSpatial.local = session.cameraSpatial.local.copy(position = ActorView.center(session.game.player.pos))
    session.particles.clear()
    session.fade = 1f
    session.dirty = true
  }

  private def show(context: GameContext, session: Session, actor: Actor)(using Scope): Unit = {
    val color = actor.kind.map(Palette.of).getOrElse(Palette.Text)
    val sprite = actor.kind.map(Sprite.of).getOrElse(Sprite.Player)
    val view = context.spawn2D(actor.title, Transform2D(ActorView.center(actor.pos)))
      .attach(new SpriteRenderer(_, sprite.visual(session.sheet), 16f, 16f, z = 2f, color = color.withAlpha(0f)))
      .attach(new ActorView(_, actor, color, session.sheet))
      .get[ActorView]
    session.views(actor.id) = view
  }

  def present(context: GameContext, session: Session)(using Scope): Unit = {
    val game = session.game
    val services = session.services
    val player = game.player.id
    def seen(at: Pos) = game.visible(at)
    def view(id: Int) = session.views.get(id)
    game.takeEvents().foreach {
      case Event.Moved(id, from, to) =>
        view(id).foreach { v => if(seen(from) || seen(to) || id == player) v.walk(from, to) else v.place(to) }
        if(id == player) services.play(SoundName.Step)
      case Event.Struck(by, target, at, damage) =>
        session.views.get(by).foreach(v => if(v.actor.pos.distance(at) == 1) v.bump(v.actor.pos, at))
        if(seen(at)) {
          view(target).foreach(_.hit())
          val color = if(target == player) Palette.Red else Palette.Text
          session.float(damage.toString, ActorView.center(at), color)
          session.sparks(ActorView.center(at), color, 6, 50f)
          if(target == player) session.shake = scala.math.min(1f, damage / 6f)
          services.play(if(target == player) SoundName.Hurt else SoundName.Hit)
        }
      case Event.Missed(by, _, at) =>
        session.views.get(by).foreach(v => if(v.actor.pos.distance(at) == 1) v.bump(v.actor.pos, at))
        if(seen(at)) {
          session.float("miss", ActorView.center(at), Palette.Dim)
          services.play(SoundName.Miss)
        }
      case Event.Hurt(target, at, damage, by) =>
        if(seen(at)) {
          view(target).foreach(_.hit())
          session.float(damage.toString, ActorView.center(at), if(by == Status.Burning) Palette.Gold else Palette.Gas)
          if(target == player) services.play(SoundName.Hurt)
        }
      case Event.Healed(_, at, amount) =>
        if(seen(at)) {
          session.float(s"+$amount", ActorView.center(at), Palette.Green)
          session.sparks(ActorView.center(at), Palette.Green, 10, 30f)
          services.play(SoundName.Mend)
        }
      case Event.Died(id, at) =>
        if(seen(at)) session.sparks(ActorView.center(at), view(id).flatMap(_.actor.kind).map(Palette.of).getOrElse(Palette.Text), 14, 70f)
        view(id).foreach(_.die())
        session.views.remove(id)
      case Event.Appeared(id, at) =>
        game.level.monsters.find(_.id == id).foreach(show(context, session, _))
        if(seen(at)) session.sparks(ActorView.center(at), Palette.of(MonsterKind.InkSlug), 12, 40f)
      case Event.Shot(from, to) => session.bolt(ActorView.center(from), ActorView.center(to), Palette.Gold)
      case Event.Thrown(item, from, to) => session.bolt(ActorView.center(from), ActorView.center(to), Palette.of(game, item))
      case Event.Burst(at, fire) =>
        session.sparks(ActorView.center(at), if(fire) Palette.Gold else Palette.Gas, 24, 90f)
        session.shake = 0.8f
        services.play(SoundName.Burst)
      case Event.Teleported(id, to) =>
        view(id).foreach(_.place(to))
        session.travel = Vector.empty
        session.sparks(ActorView.center(to), Palette.Blue, 18, 60f)
        services.play(SoundName.Descend)
      case Event.Took(_) => services.play(SoundName.Pickup)
      case Event.Upgraded =>
        session.sparks(ActorView.center(game.player.pos), Palette.Gold, 18, 50f)
        services.play(SoundName.Rank)
      case Event.LevelUp =>
        session.float("rank up", ActorView.center(game.player.pos) - Vec2(0f, 8f), Palette.Gold)
        services.play(SoundName.Rank)
      case Event.Arrived(_) =>
        showFloor(context, session)
        session.travel = Vector.empty
        services.play(SoundName.Descend)
        services.saveGame(game.save)
      case Event.Message(text) => session.say(text)
      case Event.Won => finish(session, "You bring the Lodestar back to the surface.", "")
      case Event.Lost(cause) => finish(session, s"You were undone by $cause.", cause)
    }
    session.dirty = true
    if(services.settings.hints && session.hintLeft <= 0f && game.outcome == Outcome.Playing)
      Hint.due(game, services.told).foreach { hint =>
        services.tell(hint)
        session.hint = Some(hint)
        session.hintLeft = 9f
      }
  }

  private def finish(session: Session, text: String, cause: String): Unit = {
    session.services.addRecord(Records.of(session.game, cause))
    session.ending = Some(text)
    session.mode = Mode.Over
    session.travel = Vector.empty
    session.services.saveGame("")
  }
}

extension (session: Session) def leave(context: GameContext): Unit = context.runtime.loadScene(TitleScene)

/** Moves what is shown along: actors, effects and the camera. */
object ShowSystem extends GameSystem {
  override val id: SystemId = SystemId("observatory.show")

  // About three particles a second from every tile that burns or is clouded.
  private def smoulder(session: Session, dt: Float): Unit = {
    val level = session.game.level
    session.smoulderDue += dt
    if(session.smoulderDue >= 0.33f) {
      session.smoulderDue = 0f
      session.game.visible.foreach { p =>
        val i = level.index(p)
        if(level.fire(i) > 0) session.smoulder(ActorView.center(p) + Vec2(((p.x * 7 + p.y * 3) % 9) - 4f, 4f), Palette.Gold)
        else if(level.gas(i) > 1) session.smoulder(ActorView.center(p) + Vec2(((p.x * 5 + p.y * 11) % 11) - 5f, 2f), Palette.Gas)
      }
    }
  }

  override def run(context: GameContext)(using Scope): Unit = {
    val session = context.global[Session]
    val dt = context.dt
    // The dying ones are only known to the world.
    context.world.index[ActorView].foreach { view =>
      view.update(dt, view.actor.isPlayer || !view.actor.alive || session.game.visible(view.actor.pos))
    }
    session.particles.update(dt)
    smoulder(session, dt)
    session.effects.foreach { effect =>
      effect.age += dt
      effect.at = effect.at + effect.velocity * dt
    }
    session.effects.filterInPlace(effect => effect.age < effect.seconds)
    session.shake = scala.math.max(0f, session.shake - dt * 4f)
    session.fade = scala.math.max(0f, session.fade - dt * 2.5f)
    session.quiet += dt
    session.hintLeft = scala.math.max(0f, session.hintLeft - dt)

    val screen = session.services.screen
    if(session.zoom == 0f) session.zoom = scala.math.max(1f, scala.math.min(screen.x / 320f, screen.y / 208f).round.toFloat)
    // Whole pixels of the screen for each of the art, so that they stay even.
    val zoom = scala.math.max(1f, session.zoom.round.toFloat)
    session.camera.viewportSize = screen * (1f / zoom)

    session.views.get(session.game.player.id).foreach { player =>
      val target = player.position + session.pan
      val at = session.cameraSpatial.local.position
      val next = at + (target - at) * scala.math.min(1f, dt * 9f)
      session.cameraSpatial.local = session.cameraSpatial.local.copy(position = next)
    }
    if(session.shake > 0f) {
      val t = context.global[ElapsedTime].seconds
      val by = Vec2(scala.math.sin(t * 71).toFloat, scala.math.cos(t * 57).toFloat) * (session.shake * 2f)
      val local = session.cameraSpatial.local
      session.cameraSpatial.local = local.copy(position = local.position + by)
    }
  }
}

/** Builds the tiles of the floor when they change. */
object MapSystem extends GameSystem {
  override val id: SystemId = SystemId("observatory.map")
  private var lastBeat = -1

  override def run(context: GameContext)(using Scope): Unit = {
    val session = context.global[Session]
    // Water and flames move a few times a second.
    val beat = (context.global[ElapsedTime].seconds * 4).toInt
    if(session.dirty || beat != lastBeat) {
      session.dirty = false
      lastBeat = beat
      build(session, beat)
    }
  }

  private def build(session: Session, beat: Int): Unit = {
    val game = session.game
    val level = game.level
    val sheet = session.sheet
    val stone = Palette.of(level.biome)
    val ground = Vector.newBuilder[TileQuad]
    val objects = Vector.newBuilder[TileQuad]
    val air = Vector.newBuilder[TileQuad]

    level.positions.foreach { p =>
      val i = level.index(p)
      if(level.explored(i)) {
        val inSight = game.visible(p)
        val distance = scala.math.sqrt(((p.x - game.player.pos.x) * (p.x - game.player.pos.x) + (p.y - game.player.pos.y) * (p.y - game.player.pos.y)).toDouble)
        val light = if(inSight) scala.math.max(0.55f, 1.05f - (distance / (game.sight * 1.6)).toFloat) else 0.32f
        val x = p.x * 16f
        val y = p.y * 16f
        // Always the same for a cell, whatever the order.
        val variety = ((p.x * 73856093) ^ (p.y * 19349663)).abs
        def lit(color: Color) = Palette.shade(color, light)
        def add(sprite: Sprite, color: Color): Unit = ground += sprite.quad(sheet, x, y, lit(color))
        def base(color: Color): Unit = add(Sprite.Solid, color)
        def floor(): Unit = {
          base(stone.ground)
          if(variety % 5 == 0) add(Sprite.Dots(variety / 5 % Sprite.Dots.size), Palette.shade(stone.floor, 0.6f))
        }

        level(p) match {
          case Tile.Rock => ()
          case Tile.Wall | Tile.SecretDoor => wall(level, p, sheet, stone, lit, ground)
          case Tile.Floor => floor()
          case Tile.Door =>
            floor()
            val open = p == game.player.pos || level.monsterAt(p).exists(m => game.visible(m.pos))
            add(if(open) Sprite.OpenDoor else Sprite.ClosedDoor, stone.face)
          case Tile.LockedDoor =>
            floor()
            add(Sprite.LockedDoor, Palette.Gold)
          case Tile.Trader =>
            base(stone.ground)
            add(Sprite.Trader, Palette.Blue)
          case Tile.Water =>
            base(Palette.Water)
            if((variety + beat / 2) % 3 == 0) add(Sprite.Waves, Palette.Blue)
          case Tile.Reeds =>
            base(stone.ground)
            add(Sprite.Reeds(variety % Sprite.Reeds.size), Palette.Green)
          case Tile.Ash =>
            base(Palette.Night)
            add(Sprite.Dots(variety % Sprite.Dots.size), Palette.Dim)
          case Tile.Shelf =>
            base(stone.ground)
            add(Sprite.Shelf, stone.face)
          case Tile.Pillar =>
            base(stone.ground)
            add(Sprite.Pillar, stone.face)
          case Tile.Stairs =>
            base(stone.ground)
            add(Sprite.StairsDown, Palette.Gold)
          case Tile.StairsUp =>
            base(stone.ground)
            add(Sprite.StairsUp, Palette.Text)
        }

        level.traps.get(p).filter(!_.hidden).foreach { trap =>
          val color = trap.kind match {
            case Trap.Cinder => Palette.Red
            case Trap.Miasma => Palette.Gas
            case Trap.Alarm => Palette.Blue
          }
          objects += Sprite.Vent.quad(sheet, x, y, lit(color))
        }
        level.items.get(p).flatMap(_.headOption).foreach { item =>
          objects += Sprite.of(item).quad(sheet, x, y, lit(Palette.of(game, item)))
        }
        if(inSight) {
          if(level.fire(i) > 0) {
            val flicker = if((variety + beat) % 2 == 0) Palette.Gold else Palette.Red
            air += Sprite.Solid.quad(sheet, x, y, Palette.Red.withAlpha(0.25f))
            air += Sprite.Flame.quad(sheet, x, y, flicker)
          }
          if(level.gas(i) > 0)
            air += Sprite.Solid.quad(sheet, x, y, Palette.Gas.withAlpha(scala.math.min(0.5f, 0.12f + level.gas(i) * 0.04f)))
        }
      }
    }
    level.monsters.foreach(_.aim.filter(game.visible).foreach { p =>
      air += Sprite.Solid.quad(sheet, p.x * 16f, p.y * 16f, Palette.Red.withAlpha(if(beat % 2 == 0) 0.45f else 0.25f))
    })
    if(session.travel.nonEmpty) session.travel.foreach { p =>
      objects += Sprite.Solid.quad(sheet, p.x * 16f + 7f, p.y * 16f + 7f, Palette.Gold.withAlpha(0.5f), 2f, 2f)
    }
    session.ground.quads = ground.result()
    session.objects.quads = objects.result()
    session.air.quads = air.result()
  }

  // A wall shows its face to what is south of it, and an edge on other sides.
  private def wall(
      level: Level, p: Pos, sheet: Texture2D, stone: Palette.Stone, lit: Color => Color,
      out: scala.collection.mutable.Builder[TileQuad, Vector[TileQuad]]
  ): Unit = {
    val x = p.x * 16f
    val y = p.y * 16f
    def open(dx: Int, dy: Int): Boolean = level(p + Pos(dx, dy)) match {
      case Tile.Wall | Tile.Rock | Tile.SecretDoor => false
      case _ => true
    }
    def part(color: Color, px: Float, py: Float, w: Float, h: Float): Unit =
      out += Sprite.Solid.quad(sheet, x + px, y + py, lit(color), w, h)
    if(open(0, 1)) {
      part(Palette.shade(stone.face, 0.35f), 0f, 0f, 16f, 16f)
      out += Sprite.Bricks.quad(sheet, x, y, lit(stone.face))
    } else {
      part(stone.wall, 0f, 0f, 16f, 16f)
      if(open(0, -1)) part(stone.face, 0f, 0f, 16f, 2f)
      if(open(-1, 0)) part(stone.face, 0f, 0f, 2f, 16f)
      if(open(1, 0)) part(stone.face, 14f, 0f, 2f, 16f)
    }
  }
}
