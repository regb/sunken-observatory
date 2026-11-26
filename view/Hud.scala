package observatory
package core

import sgl.engine.gameobject._
import sgl.engine.gameobject.extras.{UiBox, UiInput}
import sgl.engine.gameobject.render2d.{Painter2D, Render2D, Text2D}
import observatory.model._
import sgl.geometry.Vec2

object EffectRenderSystem extends GameSystem {
  override val id: SystemId = SystemId("observatory.effects")

  override def run(context: GameContext)(using Scope): Unit = {
    val session = context.global[Session]
    val draw = context.global[Render2D]
    def square(painter: Painter2D, at: Vec2, size: Float, color: Color): Unit =
      painter.quad(at, at + Vec2(size, 0f), at + Vec2(size, size), at + Vec2(0f, size), color)

    draw.world(3.5f)(session.particles.paint)
    draw.world(4f) { painter =>
      session.effects.foreach { effect =>
        val left = 1f - effect.age / effect.seconds
        effect.text match {
          case None => square(painter, effect.at - Vec2(effect.size, effect.size) * 0.5f, effect.size, effect.color.withAlpha(left))
          case Some(text) =>
            val at = effect.at - Vec2(Text2D.width(text, effect.size, bold = true) / 2f, effect.size * 0.7f)
            painter.text(text, at + Vec2(0.5f, 0.5f), effect.size, Palette.Night.withAlpha(left), bold = true)
            painter.text(text, at, effect.size, effect.color.withAlpha(left), bold = true)
        }
      }
      // Who is hurt, and who is asleep.
      session.game.visibleMonsters.foreach { monster =>
        session.views.get(monster.id).foreach { view =>
          if(monster.hp < monster.maxHp) {
            val at = view.position + Vec2(-6f, 8f)
            painter.quad(at, at + Vec2(12f, 0f), at + Vec2(12f, 1.5f), at + Vec2(0f, 1.5f), Palette.Night)
            val w = 12f * monster.hp / monster.maxHp
            painter.quad(at, at + Vec2(w, 0f), at + Vec2(w, 1.5f), at + Vec2(0f, 1.5f), Palette.Red)
          }
          if(monster.asleep) painter.text("z", view.position + Vec2(4f, -12f), 7f, Palette.Dim, bold = true)
        }
      }
    }
  }
}

/** The interface over the map. */
object HudSystem extends GameSystem {
  override val id: SystemId = SystemId("observatory.hud")

  override def run(context: GameContext)(using Scope): Unit = {
    val session = context.global[Session]
    val draw = context.global[Render2D]
    val middle = session.services.screen * 0.5f
    val input = new UiInput(
      session.gestures.collect { case Gesture.Tap(at) => at },
      session.controls.hover,
      // In a list, the wheel and a drag scroll.
      session.gestures.collect {
        case Gesture.Pan(by) => (session.controls.hover.getOrElse(middle), -by.y)
        case Gesture.Zoom(factor) => (session.controls.hover.getOrElse(middle), if(factor < 1f) 40f * session.unit else -40f * session.unit)
      }
    )
    def layer() = new Ui(session.services.screen, session.unit, session.sheet, input)

    if(session.fade > 0f) {
      val veil = layer()
      veil.fill(veil.everywhere, Palette.Night.withAlpha(session.fade))
      veil.submit(draw, 5)
    }

    val top = layer()
    session.mode match {
      case Mode.Pack(selected) => pack(context, session, top, selected)
      case Mode.Over => over(context, session, top)
      case Mode.Journal => journal(session, top)
      case Mode.Menu => menu(context, session, top)
      case Mode.Aiming(_) => aiming(session, top)
      case Mode.Playing => ()
    }
    session.mode match {
      case Mode.Pack(_) | Mode.Journal | Mode.Menu | Mode.Over => top.block(top.everywhere)
      case _ => ()
    }
    top.submit(draw, 20)

    val hud = layer()
    status(session, hud)
    minimap(session, hud)
    messages(session, hud)
    hint(session, hud)
    if(session.mode == Mode.Playing) actions(context, session, hud)
    hud.submit(draw, 10)

    session.taps = session.taps ++ input.remainingTaps
  }

  private def hint(session: Session, ui: Ui): Unit = session.hint.filter(_ => session.hintLeft > 0f).foreach { hint =>
    val u = ui.unit
    val width = scala.math.min(230f * u, ui.screen.x - 8f * u)
    val lines = Text2D.wrap(hint.text, width - 16f * u, 8f * u)
    val box = UiBox((ui.screen.x - width) / 2f, 66f * u, width, lines.size * 11f * u + 10f * u)
    ui.panel(box)
    lines.zipWithIndex.foreach { case (line, i) => ui.centered(line, UiBox(box.x, box.y + 4f * u + i * 11f * u, width, 12f * u), 8f * u, Palette.Gold) }
    // A tap on it is a sign that it was read.
    if(ui.tapped(box)) session.hintLeft = 0f
  }

  private def menu(context: GameContext, session: Session, ui: Ui): Unit = {
    val u = ui.unit
    ui.fill(ui.everywhere, Palette.Night.withAlpha(0.6f))
    val box = UiBox(ui.screen.x / 2f - 110f * u, ui.screen.y / 2f - 84f * u, 220f * u, 168f * u)
    ui.panel(box)
    ui.centered("Paused", box.copy(height = 24f * u), 11f * u, Palette.Gold, bold = true)
    Settings.rows(ui, UiBox(box.x + 12f * u, box.y + 28f * u, box.width - 24f * u, 72f * u), session.services)
    if(ui.button(UiBox(box.x + 12f * u, box.y + 108f * u, box.width - 24f * u, 22f * u), "Keep going", size = 9f * u)) session.mode = Mode.Playing
    if(ui.button(UiBox(box.x + 12f * u, box.y + 136f * u, box.width - 24f * u, 22f * u), "Save and leave", size = 9f * u)) session.leave(context)
  }

  private def status(session: Session, ui: Ui): Unit = {
    val game = session.game
    val u = ui.unit
    val box = UiBox(4f * u, 4f * u, 122f * u, 52f * u)
    ui.panel(box)
    ui.icon(Sprite.Heart, box.x + 4f * u, box.y + 4f * u, 12f * u, Palette.Red)
    val bar = UiBox(box.x + 20f * u, box.y + 4f * u, 96f * u, 11f * u)
    ui.bar(bar, game.player.hp.toFloat / game.player.maxHp, Palette.Red)
    ui.centered(s"${game.player.hp} / ${game.player.maxHp}", bar, 7f * u, bold = true)
    ui.bar(UiBox(bar.x, bar.y + 13f * u, bar.width, 3f * u), game.experience.toFloat / game.experienceNeeded, Palette.Gold)
    ui.text(s"Rank ${game.rank}   Armor ${game.armor}", box.x + 20f * u, box.y + 20f * u, 7f * u)
    ui.text(s"${game.level.biome.title}, floor ${game.depth}", box.x + 20f * u, box.y + 30f * u, 7f * u, Palette.Dim)
    ui.icon(Sprite.Flame, box.x + 4f * u, box.y + 37f * u, 11f * u, if(game.oil > 200) Palette.Gold else Palette.Red)
    ui.bar(UiBox(bar.x, box.y + 42f * u, 60f * u, 4f * u), game.oil.toFloat / game.MaxOil, Palette.Gold)
    ui.text(s"${game.coins} coins", bar.x + 64f * u, box.y + 38f * u, 7f * u, Palette.Gold)
    val states = game.player.statuses.keys.toVector.sortBy(_.ordinal).map(_.title)
    if(states.nonEmpty) ui.text(states.mkString(", "), box.x, box.y + box.height + 2f * u, 8f * u, Palette.Gold, bold = true)
    ui.block(box)
  }

  private def minimap(session: Session, ui: Ui): Unit = {
    val game = session.game
    val level = game.level
    val cell = 2f * ui.unit
    val box = UiBox(ui.screen.x - level.width * cell - 6f * ui.unit, 4f * ui.unit, level.width * cell + 2f * ui.unit, level.height * cell + 2f * ui.unit)
    ui.fill(box, Palette.Ink.withAlpha(0.8f))
    def dot(p: Pos, color: Color): Unit =
      ui.fill(UiBox(box.x + ui.unit + p.x * cell, box.y + ui.unit + p.y * cell, cell, cell), color)
    level.positions.foreach { p =>
      if(level.explored(level.index(p))) level(p) match {
        case Tile.Rock => ()
        case Tile.Wall | Tile.SecretDoor | Tile.Pillar | Tile.Shelf => dot(p, Palette.Raised)
        case Tile.Stairs => dot(p, Palette.Gold)
        case Tile.StairsUp => dot(p, Palette.Text)
        case Tile.Water => dot(p, Palette.Water)
        case Tile.LockedDoor => dot(p, Palette.Gold)
        case Tile.Trader => dot(p, Palette.Blue)
        case Tile.Door => dot(p, Palette.of(level.biome).face)
        case _ => dot(p, Palette.of(level.biome).floor)
      }
    }
    level.items.keysIterator.filter(p => level.explored(level.index(p))).foreach(dot(_, Palette.Green))
    game.visibleMonsters.foreach(m => dot(m.pos, Palette.Red))
    dot(game.player.pos, Palette.Text)
    ui.block(box)
  }

  private def messages(session: Session, ui: Ui): Unit = {
    val u = ui.unit
    val alpha = if(session.quiet < 5f) 1f else scala.math.max(0.35f, 6f - session.quiet)
    val width = scala.math.min(220f * u, ui.screen.x - 12f * u)
    val lines = session.log.takeRight(5).toVector.flatMap(Text2D.wrap(_, width, 8f * u)).takeRight(5)
    lines.zipWithIndex.foreach { case (line, i) =>
      val y = ui.screen.y - 36f * u - (lines.size - i) * 11f * u
      ui.text(line, 6f * u + u, y + u, 8f * u, Palette.Night.withAlpha(alpha))
      ui.text(line, 6f * u, y, 8f * u, Palette.Text.withAlpha(if(i == lines.size - 1) alpha else alpha * 0.7f))
    }
  }

  private def actions(context: GameContext, session: Session, ui: Ui)(using Scope): Unit = {
    val game = session.game
    val u = ui.unit
    val size = 26f * u
    val y = ui.screen.y - size - 4f * u
    (0 until 5).foreach { i =>
      val box = UiBox(4f * u + i * (size + 3f * u), y, size, size)
      if(ui.slot(box, game.pack.lift(i).map(describe(game, _))) && !session.busy) Turns.quickUse(context, session, i)
      ui.block(box)
    }
    def right(i: Int, w: Float) = UiBox(ui.screen.x - 4f * u - (i + 1) * (w + 3f * u), y, w, size)
    val w = 44f * u
    def above(i: Int) = UiBox(ui.screen.x - 4f * u - (i + 1) * (w + 3f * u), y - 19f * u, w, 16f * u)
    if(ui.button(right(0, w), "Pack")) session.mode = Mode.Pack(None)
    if(ui.button(right(1, w), "Wait") && !session.busy) { val _ = Turns.perform(context, session, Command.Wait) }
    val ready = game.abilityWait == 0
    val ability = if(ready) game.calling.ability else s"${game.calling.ability} ${game.abilityWait}"
    if(ui.button(above(0), ability, enabled = ready) && !session.busy) { val _ = Turns.perform(context, session, Command.Ability) }
    if(ui.button(above(3), "Menu")) session.mode = Mode.Menu
    if(ui.button(above(2), "Journal")) session.mode = Mode.Journal
    if(ui.button(above(1), "Search") && !session.busy) { val _ = Turns.perform(context, session, Command.Search) }
    val here =
      if(game.level.items.contains(game.player.pos)) Some(game.price(game.player.pos).map(cost => s"Buy $cost").getOrElse("Take"))
      else if(game.level(game.player.pos) == Tile.Stairs) Some("Descend")
      else if(game.level(game.player.pos) == Tile.StairsUp) Some(if(game.depth > 1) "Ascend" else "Leave")
      else None
    here.foreach(label => if(ui.button(right(2, w), label) && !session.busy) Turns.interact(context, session))
  }

  private def describe(game: Game, item: Item): (Sprite, Color, String) =
    (Sprite.of(item), Palette.of(game, item), if(item.kind.isInstanceOf[ItemKind.Prism]) item.charge.toString else if(item.count > 1) item.count.toString else "")

  private def aiming(session: Session, ui: Ui): Unit = {
    val u = ui.unit
    val box = UiBox(ui.screen.x / 2f - 70f * u, ui.screen.y - 64f * u, 140f * u, 24f * u)
    ui.panel(box)
    ui.centered("Aim it where?", box.copy(width = 96f * u), 8f * u)
    if(ui.button(UiBox(box.x + 98f * u, box.y + 4f * u, 38f * u, 16f * u), "Cancel", size = 7f * u)) session.mode = Mode.Playing
    ui.block(box)
  }

  private def pack(context: GameContext, session: Session, ui: Ui, selected: Option[Int])(using Scope): Unit = {
    val game = session.game
    val u = ui.unit
    ui.fill(ui.everywhere, Palette.Night.withAlpha(0.55f))
    val box = UiBox(ui.screen.x / 2f - 150f * u, ui.screen.y / 2f - 100f * u, 300f * u, 200f * u)
    ui.panel(box)
    ui.text("Pack", box.x + 10f * u, box.y + 6f * u, 11f * u, Palette.Gold, bold = true)
    if(ui.button(UiBox(box.x + box.width - 42f * u, box.y + 6f * u, 36f * u, 15f * u), "Close", size = 7f * u)) session.mode = Mode.Playing

    val size = 30f * u
    val worn = Vector(Some(game.instrument), Some(game.coat), game.lens)
    val left = box.x + 10f * u
    ui.text("Worn", left, box.y + 24f * u, 7f * u, Palette.Dim)
    worn.zipWithIndex.foreach { case (item, i) =>
      val _ = ui.slot(UiBox(left + i * (size + 3f * u), box.y + 35f * u, size, size), item.map(describe(game, _)))
    }
    (0 until game.PackSize).foreach { i =>
      val cell = UiBox(left + (i % 4) * (size + 3f * u), box.y + 70f * u + (i / 4) * (size + 2f * u), size, size)
      if(ui.slot(cell, game.pack.lift(i).map(describe(game, _)), selected = selected.contains(i))) session.mode = Mode.Pack(Some(i))
    }

    val x = box.x + 152f * u
    val width = 140f * u
    selected.filter(game.pack.isDefinedAt) match {
      case Some(slot) =>
        val item = game.pack(slot)
        ui.icon(Sprite.of(item), x, box.y + 30f * u, 32f * u, Palette.of(game, item))
        Text2D.wrap(game.title(item).capitalize, width - 38f * u, 9f * u, bold = true).zipWithIndex.foreach { case (line, i) =>
          ui.text(line, x + 38f * u, box.y + 32f * u + i * 12f * u, 9f * u, Palette.Text, bold = true)
        }
        Text2D.wrap(game.about(item), width, 8f * u).zipWithIndex.foreach { case (line, i) =>
          ui.text(line, x, box.y + 72f * u + i * 11f * u, 8f * u, Palette.Dim)
        }
        val prism = item.kind.isInstanceOf[ItemKind.Prism]
        val verb = item.kind match {
          case _: ItemKind.Prism => "Aim"
          case _: ItemKind.Vial => "Drink"
          case _: ItemKind.Chart => "Read"
          case ItemKind.Oil => "Fill the lamp"
          case ItemKind.Key => "For a vault"
          case _ => "Wear"
        }
        val brass = game.brassIn(item)
        // Four buttons where there is something to take apart.
        val step = if(brass > 0) 19f * u else 24f * u
        def button(i: Int, label: String) =
          ui.button(UiBox(x, box.y + 118f * u + i * step, width, step - 3f * u), label) && !session.busy
        if(brass > 0 && button(3, s"Take apart for $brass brass")) {
          session.mode = Mode.Pack(None)
          val _ = Turns.perform(context, session, Command.Scrap(slot))
        }
        if(button(0, verb)) {
          session.mode = if(prism) Mode.Aiming(slot) else Mode.Playing
          if(!prism) { val _ = Turns.perform(context, session, Command.Use(slot)) }
        }
        if(button(1, "Throw")) session.mode = Mode.Aiming(slot)
        if(button(2, if(game.nearTrader) s"Sell for ${game.sellPrice(item)}" else "Drop")) {
          session.mode = Mode.Pack(None)
          val _ = Turns.perform(context, session, Command.Drop(slot))
        }
      case None =>
        val lines = Vector(
          s"Holding: ${game.title(game.instrument)}", s"Wearing: ${game.title(game.coat)}",
          s"Lens: ${game.lens.map(game.title).getOrElse("none")}", "", s"Brass ${game.brass} of ${Tuning.BrassForChart}", s"Seed ${game.seed}", s"Turn ${game.turns}"
        )
        lines.flatMap(line => if(line.isEmpty) Vector("") else Text2D.wrap(line, width, 8f * u)).zipWithIndex.foreach { case (line, i) =>
          ui.text(line, x, box.y + 35f * u + i * 12f * u, 8f * u, Palette.Dim)
        }
    }
  }

  /** What was found out, which lasts for the game, and everything that was said. */
  private def journal(session: Session, ui: Ui): Unit = {
    val game = session.game
    val u = ui.unit
    ui.fill(ui.everywhere, Palette.Night.withAlpha(0.55f))
    val box = UiBox(ui.screen.x / 2f - 150f * u, ui.screen.y / 2f - 105f * u, 300f * u, 210f * u)
    ui.panel(box)
    ui.text("Journal", box.x + 10f * u, box.y + 6f * u, 11f * u, Palette.Gold, bold = true)
    if(ui.button(UiBox(box.x + box.width - 42f * u, box.y + 6f * u, 36f * u, 15f * u), "Close", size = 7f * u)) session.mode = Mode.Playing
    val x = box.x + 10f * u
    val width = box.width - 26f * u
    ui.scroll(UiBox(x, box.y + 26f * u, box.width - 18f * u, box.height - 34f * u), session.journal) { top =>
      var y = top
      def heading(text: String): Unit = {
        ui.text(text, x, y, 9f * u, Palette.Gold, bold = true)
        y += 14f * u
      }
      def entry(sprite: Sprite, color: Color, title: String, about: String): Unit = {
        ui.icon(sprite, x, y, 14f * u, color)
        ui.text(title, x + 19f * u, y - u, 8f * u)
        y += scala.math.max(18f * u, 10f * u + ui.paragraph(about, x + 19f * u, y + 9f * u, width - 19f * u, 7f * u, Palette.Dim))
      }
      heading(s"${game.calling.title}, rank ${game.rank}")
      y += ui.paragraph(game.calling.about, x, y, width, 7f * u, Palette.Dim) + 6f * u
      val kinds = (ItemKind.vials ++ ItemKind.charts)
      heading(s"Vials and charts: ${kinds.count(game.isKnown)} of ${kinds.size} known")
      kinds.filter(game.isKnown).foreach { kind =>
        entry(Sprite.of(Item(kind)), Palette.of(game, Item(kind)), kind.title.capitalize, kind.about)
      }
      y += 4f * u
      heading(s"Met: ${game.met.size} of ${MonsterKind.values.length}")
      MonsterKind.values.filter(game.met).foreach { kind =>
        entry(Sprite.of(kind), Palette.of(kind), kind.title.capitalize, s"${kind.hp} health, ${kind.minDamage} to ${kind.maxDamage} damage. ${kind.behavior.about}")
      }
      y += 4f * u
      heading("What happened")
      session.log.reverseIterator.foreach(line => y += ui.paragraph(line, x, y, width, 7f * u, Palette.Dim) + u)
      y - top
    }
  }

  private def over(context: GameContext, session: Session, ui: Ui): Unit = {
    val u = ui.unit
    val game = session.game
    ui.fill(ui.everywhere, Palette.Night.withAlpha(0.7f))
    val box = UiBox(ui.screen.x / 2f - 120f * u, ui.screen.y / 2f - 60f * u, 240f * u, 120f * u)
    ui.panel(box)
    val won = game.outcome == Outcome.Won
    ui.centered(if(won) "The way home" else "Lost below", box.copy(height = 34f * u), 16f * u, if(won) Palette.Gold else Palette.Red, bold = true)
    session.ending.foreach(text => ui.centered(text, box.copy(y = box.y + 34f * u, height = 16f * u), 8f * u))
    ui.centered(
      s"Floor ${game.depth}, rank ${game.rank}, ${game.turns} turns", box.copy(y = box.y + 52f * u, height = 14f * u), 8f * u, Palette.Dim
    )
    if(ui.button(UiBox(box.x + 60f * u, box.y + 84f * u, 120f * u, 24f * u), "Back to the surface", size = 9f * u)) session.leave(context)
  }
}
