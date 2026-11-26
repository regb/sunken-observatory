package observatory
package core

import sgl.engine.gameobject.Color
import sgl.engine.gameobject.extras
import sgl.engine.gameobject.extras.{UiBox, UiInput, UiTheme}
import sgl.engine.gameobject.render2d.Texture2D
import sgl.geometry.Vec2

/** The engine's interface, with the icons of the sheet. */
final class Ui(val screen: Vec2, val unit: Float, sheet: Texture2D, input: UiInput) extends extras.Ui(input, Ui.theme(unit)) {
  val everywhere: UiBox = UiBox(0f, 0f, screen.x, screen.y)

  def icon(sprite: Sprite, x: Float, y: Float, size: Float, color: Color): Unit = paint(sprite.paint(_, sheet, x, y, size, color))

  /** A button showing an icon, if there is one. */
  def slot(box: UiBox, shown: Option[(Sprite, Color, String)], selected: Boolean = false): Boolean = {
    surface(box, enabled = shown.isDefined)
    if(selected) fill(UiBox(box.x, box.y + box.height - 2f * unit, box.width, 2f * unit), Palette.Gold)
    shown.foreach { case (sprite, color, label) =>
      val size = box.width * 0.72f
      icon(sprite, box.x + (box.width - size) / 2f, box.y + (box.height - size) / 2f, size, color)
      if(label.nonEmpty) text(label, box.x + 2f * unit, box.y + unit, 6f * unit, Palette.Gold, bold = true)
    }
    shown.isDefined && tapped(box)
  }
}

object Ui {
  def theme(unit: Float): UiTheme = UiTheme(
    Palette.Panel, Palette.Raised, Palette.Raised, Palette.Hover, Palette.Ink, Palette.Text, Palette.Dim,
    textSize = 8f * unit
  )
}
