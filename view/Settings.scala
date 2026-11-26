package observatory
package core

import sgl.engine.gameobject.extras.UiBox

/** Volumes go from 0 to 4. */
final case class Settings(sound: Int = 3, music: Int = 2, hints: Boolean = true) {
  def soundVolume: Float = sound / 4f
  def musicVolume: Float = music / 4f * 0.7f
  def write: String = s"$sound $music ${if(hints) 1 else 0}"
}

object Settings {
  val Max: Int = 4

  def read(text: String): Option[Settings] = text.split(" ") match {
    case Array(sound, music, hints) =>
      try {
        def volume(value: String) = scala.math.max(0, scala.math.min(Max, value.toInt))
        Some(Settings(volume(sound), volume(music), hints == "1"))
      } catch { case _: NumberFormatException => None }
    case _ => None
  }

  /** Rows to change the settings with, from the top of a box. */
  def rows(ui: Ui, box: UiBox, services: Services): Unit = {
    val u = ui.unit
    val settings = services.settings
    def row(i: Int) = UiBox(box.x, box.y + i * 24f * u, box.width, 20f * u)
    def volume(i: Int, label: String, value: Int)(change: Int => Settings): Unit = {
      val at = row(i)
      ui.text(label, at.x, at.y + 4f * u, 9f * u)
      (0 to Max).foreach { level =>
        val step = UiBox(at.x + at.width - (Max + 1 - level) * 20f * u, at.y, 18f * u, at.height)
        if(ui.button(step, level.toString, size = 8f * u)) services.changeSettings(change(level))
        if(level == value) ui.fill(UiBox(step.x, step.y + step.height - 2f * u, step.width, 2f * u), Palette.Gold)
      }
    }
    volume(0, "Sounds", settings.sound)(value => settings.copy(sound = value))
    volume(1, "Music", settings.music)(value => settings.copy(music = value))
    val at = row(2)
    ui.text("Hints", at.x, at.y + 4f * u, 9f * u)
    if(ui.button(UiBox(at.x + at.width - 58f * u, at.y, 56f * u, at.height), if(settings.hints) "On" else "Off"))
      services.changeSettings(settings.copy(hints = !settings.hints))
  }
}
