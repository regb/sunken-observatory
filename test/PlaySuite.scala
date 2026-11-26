package observatory

import java.nio.file.Paths

import org.scalatest.funsuite.AnyFunSuite
import _root_.sgl.{CapturedFrame, DesktopSystemProvider, Input, MemorySaveComponent}
import _root_.sgl.desktop.DesktopFrameIO
import _root_.observatory.model.{Bot, Command, Game, Hint, Outcome, Pos, Records, Rng, Tile}
import _root_.sgl.headless.opengl.HeadlessOpenGLApp
import _root_.sgl.util.NoLoggingProvider

/** Plays the real game, through input and without a window. */
class PlaySuite extends AnyFunSuite {

  private trait TestApp extends HeadlessOpenGLApp
      with core.AbstractApp
      with core.GeneratedAssetsProvider
      with DesktopSystemProvider
      with NoLoggingProvider
      with MemorySaveComponent {
    override val frameDimension: (Int, Int) = (960, 640)
    override protected def newGameSeed: Long = 11L
  }

  private def frames(app: TestApp, count: Int): Unit = (0 until count).foreach(_ => app.step(1.0 / 60.0))

  private def press(app: TestApp, key: Input.Keys.Key, frameCount: Int = 30): Unit = {
    app.Controller.keyDown(key)
    frames(app, 1)
    app.Controller.keyUp(key)
    frames(app, frameCount)
  }

  private def tap(app: TestApp, x: Int, y: Int): Unit = {
    app.Controller.mouseMoved(x, y)
    app.Controller.mouseDown(x, y, Input.MouseButtons.Left)
    app.Controller.mouseUp(x, y, Input.MouseButtons.Left)
    frames(app, 20)
  }

  private def capture(app: TestApp, name: String): CapturedFrame = {
    val pending = app.captureNextFrame()
    app.step(1.0 / 60.0)
    val frame = pending.value.flatMap(_.toOption).getOrElse(fail("Frame capture did not complete"))
    Option(java.lang.System.getenv("TEST_UNDECLARED_OUTPUTS_DIR")).foreach { directory =>
      DesktopFrameIO.writePng(frame, Paths.get(directory, name + ".png"))
    }
    frame
  }

  private def colors(frame: CapturedFrame): Int =
    (0 until frame.width * frame.height).map { i =>
      val o = i * CapturedFrame.BytesPerPixel
      (frame.rgba(o), frame.rgba(o + 1), frame.rgba(o + 2))
    }.distinct.size

  /** Through the menu, whose button to leave is in the middle of the screen. */
  private def leave(app: TestApp): Unit = {
    press(app, Input.Keys.Escape)
    val _ = capture(app, "menu")
    tap(app, 480, 446)
    frames(app, 10)
  }

  private def saved(app: TestApp): Option[Game] = app.Save.getString("observatory").flatMap(Game.restore)

  test("a game is started, played, left and continued") {
    object app extends TestApp
    app.start()
    try {
      // The sheet loads on another thread.
      (0 until 100).foreach { _ =>
        Thread.sleep(10)
        frames(app, 1)
      }
      assert(colors(capture(app, "title")) > 20)
      press(app, Input.Keys.N)
      frames(app, 60)
      assert(colors(capture(app, "start")) > 50)

      // As many steps as the same commands performed by the rules.
      val start = new Game(11L)
      val steps = Vector(Input.Keys.Right -> Pos(1, 0), Input.Keys.Down -> Pos(0, 1), Input.Keys.Left -> Pos(-1, 0), Input.Keys.Up -> Pos(0, -1))
      steps.foreach { case (key, d) =>
        val done = start.perform(Command.Step(d.x, d.y))
        press(app, key)
        assert(done)
      }
      press(app, Input.Keys.Space)
      assert(start.perform(Command.Wait))

      app.Controller.mouseMoved(480, 320)
      app.Controller.mouseScrolled(0f, -2f, Input.ScrollUnits.Notches)
      frames(app, 30)
      val _ = capture(app, "zoomed")
      app.Controller.mouseScrolled(0f, 2f, Input.ScrollUnits.Notches)
      frames(app, 10)

      press(app, Input.Keys.I)
      tap(app, 230, 260)
      val _ = capture(app, "pack")
      press(app, Input.Keys.Escape)
      press(app, Input.Keys.Escape)

      // Leaving saves, from the menu.
      leave(app)
      frames(app, 30)
      val continued = saved(app).getOrElse(fail("Nothing was saved"))
      assert(continued.save == start.save)
      val _ = capture(app, "title-continue")
      press(app, Input.Keys.Enter)
      frames(app, 60)
      val _ = capture(app, "continued")
    } finally app.stop()
  }

  test("a tap walks to where it points") {
    // A game that starts with nobody around, and room to the right.
    val seed = (11L to 60L).find { candidate =>
      val game = new Game(candidate)
      val steps = (1 to 3).map(i => game.player.pos + Pos(i, 0))
      steps.forall(p => game.level(p) == Tile.Floor && game.visible(p) && !game.level.items.contains(p) && !game.level.traps.contains(p)) && {
        steps.indices.forall(_ => game.perform(Command.Step(1, 0)) && game.visibleMonsters.isEmpty)
      }
    }.getOrElse(fail("No such game"))
    object app extends TestApp {
      override protected def newGameSeed: Long = seed
    }
    app.start()
    try {
      (0 until 100).foreach { _ =>
        Thread.sleep(10)
        frames(app, 1)
      }
      press(app, Input.Keys.N)
      frames(app, 60)
      // Three tiles to the right of the player, who is in the middle: 48 pixels a tile.
      tap(app, 480 + 3 * 48, 320)
      frames(app, 120)
      leave(app)
      val game = saved(app).getOrElse(fail("Nothing was saved"))
      val from = new Game(seed).player.pos
      assert(game.player.pos == from + Pos(3, 0))
      assert(game.turns == 3)
    } finally app.stop()
  }

  private val Keys: Map[Pos, Input.Keys.Key] = Map(
    Pos(0, -1) -> Input.Keys.Up, Pos(0, 1) -> Input.Keys.Down, Pos(-1, 0) -> Input.Keys.Left, Pos(1, 0) -> Input.Keys.Right,
    Pos(-1, -1) -> Input.Keys.Q, Pos(1, -1) -> Input.Keys.E, Pos(-1, 1) -> Input.Keys.Z, Pos(1, 1) -> Input.Keys.C
  )
  private val Numbers = Vector(Input.Keys.Num1, Input.Keys.Num2, Input.Keys.Num3, Input.Keys.Num4, Input.Keys.Num5)

  test("whole games played by key are the ones the rules play") {
    object app extends TestApp {
      private var games = 0L
      override protected def newGameSeed: Long = {
        games += 1
        games + 100L
      }
    }
    app.start()
    try {
      (0 until 100).foreach { _ =>
        Thread.sleep(10)
        frames(app, 1)
      }
      (101L to 102L).foreach { seed =>
        press(app, Input.Keys.N)
        frames(app, 30)
        val shadow = new Game(seed)
        val choices = new Rng(seed)
        var turn = 0
        while(turn < 120 && shadow.outcome == Outcome.Playing) {
          // What has no key is played as waiting.
          val command = Bot.next(shadow, choices) match {
            case Command.Throw(_, _, _) | Command.Aim(_, _, _) | Command.Drop(_) | Command.Scrap(_) => Command.Wait
            case Command.Use(slot) if slot >= Numbers.size => Command.Wait
            case other => other
          }
          val key = command match {
            case Command.Step(dx, dy) => Keys(Pos(dx, dy))
            case Command.Use(slot) => Numbers(slot)
            case Command.PickUp | Command.Descend | Command.Ascend => Input.Keys.G
            case Command.Search => Input.Keys.F
            case Command.Ability => Input.Keys.R
            case _ => Input.Keys.Space
          }
          assert(shadow.perform(command))
          press(app, key, frameCount = 18)
          turn += 1
          if(turn % 25 == 0) { val _ = capture(app, s"game-$seed-$turn") }
        }
        val _ = capture(app, s"game-$seed-end")
        // To continue on another platform, and compare.
        Option(java.lang.System.getenv("TEST_UNDECLARED_OUTPUTS_DIR")).foreach { directory =>
          java.nio.file.Files.write(Paths.get(directory, s"game-$seed.save"), shadow.save.getBytes("UTF-8"))
        }
        if(shadow.outcome == Outcome.Playing) {
          press(app, Input.Keys.J)
          val top = capture(app, s"journal-$seed")
          app.Controller.mouseMoved(480, 320)
          app.Controller.mouseScrolled(0f, 1f, Input.ScrollUnits.Notches)
          frames(app, 5)
          assert(!java.util.Arrays.equals(top.rgba, capture(app, s"journal-$seed-scrolled").rgba))
          press(app, Input.Keys.Escape)
        }
        // A game that has ended leaves nothing to continue.
        if(shadow.outcome == Outcome.Playing) leave(app) else press(app, Input.Keys.Escape)
        if(shadow.outcome == Outcome.Playing) assert(saved(app).map(_.save) == Some(shadow.save), seed)
        else assert(app.Save.getString("observatory") == Some(""), seed)
        // How it ended is kept.
        if(shadow.outcome != Outcome.Playing)
          assert(app.Save.getString("observatory-records").map(Records.read).exists(_.exists(r => r.seed == seed && r.turns == shadow.turns)))
        info(s"seed $seed: $turn turns, floor ${shadow.depth}, ${shadow.outcome}")
      }
    } finally app.stop()
  }

  test("a key that is slow to come up makes one step, and one that is held walks") {
    val seed = (11L to 200L).find { candidate =>
      val game = new Game(candidate)
      (1 to 4).forall(_ => game.perform(Command.Step(1, 0)) && game.visibleMonsters.isEmpty && game.level(game.player.pos) == Tile.Floor)
    }.getOrElse(fail("No such game"))
    def turns(held: Int): Int = {
      object app extends TestApp {
        override protected def newGameSeed: Long = seed
      }
      app.start()
      try {
        (0 until 100).foreach { _ =>
          Thread.sleep(10)
          frames(app, 1)
        }
        press(app, Input.Keys.N)
        frames(app, 30)
        app.Controller.keyDown(Input.Keys.Right)
        frames(app, held)
        app.Controller.keyUp(Input.Keys.Right)
        frames(app, 30)
        leave(app)
        saved(app).map(_.turns).getOrElse(fail("Nothing was saved"))
      } finally app.stop()
    }
    // 0.2 and 0.5 seconds.
    assert(turns(12) == 1)
    assert(turns(30) >= 2)
  }

  test("touch walks, pans and zooms") {
    val seed = (11L to 60L).find { candidate =>
      val game = new Game(candidate)
      val step = game.player.pos + Pos(1, 0)
      game.level(step) == Tile.Floor && !game.level.items.contains(step) && game.visibleMonsters.isEmpty && !game.level.traps.contains(step)
    }.getOrElse(fail("No such game"))
    object app extends TestApp {
      override protected def newGameSeed: Long = seed
    }
    app.start()
    try {
      (0 until 100).foreach { _ =>
        Thread.sleep(10)
        frames(app, 1)
      }
      press(app, Input.Keys.N)
      frames(app, 60)
      val touch = app.Controller
      val before = capture(app, "touch-before")
      // Two fingers apart are closer.
      touch.touchDown(400, 320, 0)
      touch.touchDown(560, 320, 1)
      touch.touchMoved(300, 320, 0)
      touch.touchMoved(660, 320, 1)
      touch.touchUp(300, 320, 0)
      touch.touchUp(660, 320, 1)
      frames(app, 30)
      val zoomed = capture(app, "touch-zoomed")
      assert(!java.util.Arrays.equals(before.rgba, zoomed.rgba))
      // And back.
      touch.touchDown(300, 320, 0)
      touch.touchDown(660, 320, 1)
      touch.touchMoved(400, 320, 0)
      touch.touchMoved(560, 320, 1)
      touch.touchUp(400, 320, 0)
      touch.touchUp(560, 320, 1)
      frames(app, 30)
      // A drag looks around, and is no step.
      touch.touchDown(480, 320, 0)
      touch.touchMoved(480, 240, 0)
      touch.touchUp(480, 240, 0)
      frames(app, 40)
      assert(!java.util.Arrays.equals(before.rgba, capture(app, "touch-panned").rgba))
      // A tap on the next tile, seen where the drag left it.
      touch.touchDown(480 + 48, 240, 0)
      touch.touchUp(480 + 48, 240, 0)
      frames(app, 60)

      leave(app)
      frames(app, 10)
      val game = saved(app).getOrElse(fail("Nothing was saved"))
      assert(game.turns == 1 && game.player.pos == new Game(seed).player.pos + Pos(1, 0))
    } finally app.stop()
  }

  test("settings and hints are kept, and hints are told once") {
    object app extends TestApp
    app.start()
    try {
      (0 until 100).foreach { _ =>
        Thread.sleep(10)
        frames(app, 1)
      }
      // Settings at the top left; the first volume of sounds, then back.
      tap(app, 60, 36)
      val before = capture(app, "settings")
      assert(app.Save.getString("observatory-settings").isEmpty)
      val unit = 640f / 300f
      val x = 480 - 150 * unit + 12 * unit + (300 - 24) * unit - 5 * 20 * unit + 9 * unit
      tap(app, x.toInt, (320 - 100 * unit + 30 * unit + 12 * unit + 10 * unit).toInt)
      assert(app.Save.getString("observatory-settings").contains("0 2 1"))
      assert(!java.util.Arrays.equals(before.rgba, capture(app, "settings-silent").rgba))
      press(app, Input.Keys.Escape)
      // Records at the top right.
      tap(app, 900, 36)
      val _ = capture(app, "records")
      press(app, Input.Keys.Escape)

      press(app, Input.Keys.N)
      frames(app, 30)
      val _ = capture(app, "hint")
      val told = app.Save.getString("observatory-hints").getOrElse("")
      assert(told.nonEmpty && told.split(" ").forall(name => Hint.values.exists(_.toString == name)))
      leave(app)
      press(app, Input.Keys.N)
      frames(app, 30)
      assert(app.Save.getString("observatory-hints").exists(_.split(" ").distinct.length >= told.split(" ").length))
    } finally app.stop()
  }

  test("translucent drawings leave the frame opaque") {
    object app extends TestApp
    app.start()
    try {
      (0 until 100).foreach { _ =>
        Thread.sleep(10)
        frames(app, 1)
      }
      press(app, Input.Keys.N)
      press(app, Input.Keys.I)
      val frame = capture(app, "opaque")
      assert((0 until frame.width * frame.height).forall(i => frame.rgba(i * CapturedFrame.BytesPerPixel + 3) == -1))
    } finally app.stop()
  }
}
