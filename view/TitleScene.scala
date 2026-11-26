package observatory
package core

import sgl.Input
import sgl.engine.gameobject._
import sgl.engine.gameobject.extras.{UiBox, UiInput}
import sgl.engine.gameobject.render2d.{Render2D, Texture2D}
import observatory.model.{Calling, Game, Record}
import sgl.geometry.Vec2
import sgl.util.Loader

object TitleScene extends AsyncScene[Texture2D] {
  override def preload(context: GameContext)(using Scope): Loader[Texture2D] =
    context.global[Services].loadSheet(context)

  override def load(context: GameContext, sheet: Texture2D)(using scope: Scope): Unit = {
    val services = context.global[Services]
    val controls = new Controls
    Input.setInputProcessor(controls)
    scope.onDestroy(() => Input.clearInputProcessor())
    context.addSystem(Stage.Render, new TitleSystem(services, controls, sheet))
  }
}

private final class TitleSystem(services: Services, controls: Controls, sheet: Texture2D) extends GameSystem {
  override val id: SystemId = SystemId("observatory.title")

  private enum Page { case Start, Settings, Records }
  private var page = Page.Start
  private val saved = services.savedGame.flatMap(Game.restore)
  private val records = services.records

  override def run(context: GameContext)(using Scope): Unit = {
    val gestures = controls.take()
    val keys = gestures.collect { case Gesture.Key(key) => key }
    val screen = services.screen
    val unit = scala.math.max(1f, scala.math.min(screen.x / 400f, screen.y / 300f))
    val ui = new Ui(screen, unit, sheet, new UiInput(gestures.collect { case Gesture.Tap(at) => at }, controls.hover))
    // Browsers only let sounds start after something was done.
    if(gestures.nonEmpty) services.music(Track.Surface)

    stars(ui, context.global[ElapsedTime].seconds)
    val game = page match {
      case Page.Start => start(ui, keys)
      case Page.Settings =>
        val box = opened(ui, keys, "Settings")
        Settings.rows(ui, UiBox(box.x, box.y + 12f * unit, box.width, 80f * unit), services)
        None
      case Page.Records =>
        list(ui, opened(ui, keys, "Records"))
        None
    }
    ui.submit(context.global[Render2D], 0)
    game.foreach(chosen => context.runtime.loadScene(new PlayScene(chosen)))
  }

  private def stars(ui: Ui, time: Double): Unit = (0 until 90).foreach { i =>
    val x = ((i * 7919) % 1000) / 1000f * ui.screen.x
    val y = ((i * 6007) % 1000) / 1000f * ui.screen.y
    val glow = 0.3f + 0.3f * scala.math.sin(time * 1.5 + i).toFloat
    ui.fill(UiBox(x, y, ui.unit * 1.5f, ui.unit * 1.5f), Palette.Text.withAlpha(glow))
  }

  private def entered(keys: Vector[Input.Keys.Key]): Boolean =
    keys.exists(key => key == Input.Keys.Enter || key == Input.Keys.NumpadEnter)

  /** The game to play, once one is chosen. */
  private def start(ui: Ui, keys: Vector[Input.Keys.Key]): Option[Game] = {
    val u = ui.unit
    val screen = ui.screen
    ui.icon(Sprite(32, 10), screen.x / 2f - 20f * u, screen.y * 0.06f, 40f * u, Palette.Gold)
    def line(y: Float) = UiBox(0f, y, screen.x, 40f * u)
    ui.centered("Sunken Observatory", line(screen.y * 0.16f + 26f * u), 26f * u, Palette.Text, bold = true)
    ui.centered("The Lodestar lies eight floors down. Bring it back.", line(screen.y * 0.16f + 52f * u), 9f * u, Palette.Dim)

    val width = 140f * u
    def row(i: Int) = UiBox((screen.x - width) / 2f, screen.y * 0.52f + i * 28f * u, width, 24f * u)
    val continued = saved.filter(_ => ui.button(row(0), "Continue", size = 10f * u) || entered(keys))

    val top = row(if(saved.isDefined) 1 else 0).y
    ui.centered("Go down as a", UiBox(0f, top, screen.x, 12f * u), 8f * u, Palette.Dim)
    val third = 104f * u
    val chosen = Calling.values.toVector.filter { calling =>
      val box = UiBox(screen.x / 2f + (calling.ordinal - 1.5f) * (third + 6f * u) + 3f * u, top + 14f * u, third, 24f * u)
      if(controls.hover.exists(box.contains))
        ui.centered(calling.about, UiBox(0f, top + 42f * u, screen.x, 12f * u), 8f * u, Palette.Text)
      ui.button(box, calling.title, size = 10f * u)
    }.headOption.orElse {
      if(keys.contains(Input.Keys.N) || (saved.isEmpty && entered(keys))) Some(Calling.Surveyor) else None
    }

    def footer(y: Float, text: String): Unit = ui.centered(text, UiBox(0f, screen.y - y * u, screen.x, 12f * u), 7f * u, Palette.Dim)
    footer(40f, "Tap or click to walk and fight - arrows or WASD, Q E Z C for diagonals")
    footer(28f, "Space waits - F searches - R uses your ability - G takes, buys or takes the stairs - I pack - J journal")
    if(ui.button(UiBox(8f * u, 8f * u, 60f * u, 18f * u), "Settings")) page = Page.Settings
    if(ui.button(UiBox(screen.x - 68f * u, 8f * u, 60f * u, 18f * u), "Records")) page = Page.Records

    continued.orElse(chosen.map(calling => new Game(services.newSeed(), calling)))
  }

  /** A page over the stars, with a way back. Returns what is left of it. */
  private def opened(ui: Ui, keys: Vector[Input.Keys.Key], title: String): UiBox = {
    val u = ui.unit
    val box = UiBox(ui.screen.x / 2f - 150f * u, ui.screen.y / 2f - 100f * u, 300f * u, 200f * u)
    ui.panel(box)
    ui.text(title, box.x + 12f * u, box.y + 6f * u, 11f * u, Palette.Gold, bold = true)
    if(ui.button(UiBox(box.x + box.width - 52f * u, box.y + 6f * u, 46f * u, 16f * u), "Back") || keys.contains(Input.Keys.Escape))
      page = Page.Start
    UiBox(box.x + 12f * u, box.y + 30f * u, box.width - 24f * u, box.height - 36f * u)
  }

  private def list(ui: Ui, box: UiBox): Unit = {
    val u = ui.unit
    if(records.isEmpty) ui.text("Nobody has gone down yet.", box.x, box.y, 8f * u, Palette.Dim)
    records.zipWithIndex.foreach { case (record, i) =>
      val y = box.y + i * 13.5f * u
      ui.text(record.calling.title, box.x, y, 8f * u)
      ui.text(describe(record), box.x + 66f * u, y, 8f * u, if(record.won) Palette.Gold else Palette.Dim)
    }
  }

  private def describe(record: Record): String =
    if(record.won) s"brought the Lodestar back in ${record.turns} turns, rank ${record.rank}"
    else s"lost on floor ${record.depth}, rank ${record.rank}, to ${record.cause}"
}
