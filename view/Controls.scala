package observatory
package core

import scala.collection.mutable

import sgl.{Input, InputActions, InputProcessor}
import sgl.geometry.Vec2

/** What a player did, whatever they did it with. */
enum Gesture {
  case Tap(at: Vec2)
  case Pan(by: Vec2)
  /** Closer when above one. */
  case Zoom(factor: Float)
  case Key(key: Input.Keys.Key)
  case Back
}

/** Turns mouse, touch and keys into gestures. */
final class Controls extends InputProcessor {
  private val gestures = mutable.ArrayBuffer.empty[Gesture]
  private val pointers = mutable.LinkedHashMap.empty[Int, Vec2]
  private var start = Vec2(0f, 0f)
  private var dragging = false
  private val DragDistance = 12f
  private val Mouse = -1

  var hover: Option[Vec2] = None

  def take(): Vector[Gesture] = {
    val result = gestures.toVector
    gestures.clear()
    result
  }

  private def down(id: Int, at: Vec2): Unit = {
    pointers(id) = at
    if(pointers.size == 1) {
      start = at
      dragging = false
    } else dragging = true
  }

  private def moved(id: Int, at: Vec2): Unit = pointers.get(id).foreach { before =>
    if(pointers.size == 1) {
      if(!dragging && (at - start).norm > DragDistance) dragging = true
      if(dragging) gestures += Gesture.Pan(at - before)
    } else {
      val other = pointers.iterator.find(_._1 != id).get._2
      val was = (before - other).norm
      if(was > 1f) gestures += Gesture.Zoom((at - other).norm / was)
    }
    pointers(id) = at
  }

  private def up(id: Int, at: Vec2): Unit = if(pointers.remove(id).isDefined) {
    if(pointers.isEmpty && !dragging) gestures += Gesture.Tap(at)
  }

  override def mouseDown(x: Int, y: Int, button: Input.MouseButtons.MouseButton): Boolean = {
    if(button == Input.MouseButtons.Left) down(Mouse, Vec2(x.toFloat, y.toFloat))
    true
  }
  override def mouseMoved(x: Int, y: Int): Boolean = {
    hover = Some(Vec2(x.toFloat, y.toFloat))
    moved(Mouse, Vec2(x.toFloat, y.toFloat))
    true
  }
  override def mouseUp(x: Int, y: Int, button: Input.MouseButtons.MouseButton): Boolean = {
    if(button == Input.MouseButtons.Left) up(Mouse, Vec2(x.toFloat, y.toFloat))
    true
  }
  override def mouseScrolled(amountX: Float, amountY: Float, unit: Input.ScrollUnits.ScrollUnit): Boolean = {
    val notches = unit match {
      case Input.ScrollUnits.Notches => amountY
      case Input.ScrollUnits.Pixels => amountY / 100f
      case Input.ScrollUnits.Lines => amountY / 3f
      case Input.ScrollUnits.Pages => amountY * 8f
    }
    if(notches != 0f) gestures += Gesture.Zoom(scala.math.pow(1.25, -notches).toFloat)
    true
  }

  override def touchDown(x: Int, y: Int, pointer: Int): Boolean = {
    hover = None
    down(pointer, Vec2(x.toFloat, y.toFloat))
    true
  }
  override def touchMoved(x: Int, y: Int, pointer: Int): Boolean = {
    moved(pointer, Vec2(x.toFloat, y.toFloat))
    true
  }
  override def touchUp(x: Int, y: Int, pointer: Int): Boolean = {
    up(pointer, Vec2(x.toFloat, y.toFloat))
    true
  }

  override def keyDown(key: Input.Keys.Key): Boolean = {
    gestures += Gesture.Key(key)
    true
  }
  override def systemAction(action: InputActions.Action): Boolean = {
    gestures += Gesture.Back
    true
  }
}
