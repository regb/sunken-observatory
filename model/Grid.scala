package observatory
package model

import scala.collection.mutable

/** A cell of the grid. `y` grows downwards. */
final case class Pos(x: Int, y: Int) {
  def +(d: Pos): Pos = Pos(x + d.x, y + d.y)
  def -(d: Pos): Pos = Pos(x - d.x, y - d.y)

  /** How many steps away, where a diagonal one counts as one. */
  def distance(to: Pos): Int = scala.math.max((x - to.x).abs, (y - to.y).abs)
  def neighbors: Vector[Pos] = Pos.Directions.map(this + _)
}

object Pos {
  val Cardinal: Vector[Pos] = Vector(Pos(0, -1), Pos(1, 0), Pos(0, 1), Pos(-1, 0))
  val Directions: Vector[Pos] = Cardinal ++ Vector(Pos(1, -1), Pos(1, 1), Pos(-1, 1), Pos(-1, -1))
}

/** The only source of randomness of a game, the same on every platform. It is
  * SplitMix64, whose whole state is one number that a save can hold.
  */
final class Rng(seed: Long) {
  var state: Long = seed

  private def next(): Long = {
    state += 0x9e3779b97f4a7c15L
    var z = state
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)
  }

  /** Between 0 and n, excluded. */
  def int(n: Int): Int = ((next() >>> 33) % n).toInt
  /** Between both, included. */
  def between(min: Int, max: Int): Int = min + int(max - min + 1)
  def percent(chance: Int): Boolean = int(100) < chance
  def pick[A](choices: IndexedSeq[A]): A = choices(int(choices.size))
  def shuffle[A](values: Seq[A]): Vector[A] = {
    val result = values.toArray[Any]
    var i = result.length - 1
    while(i > 0) {
      val j = int(i + 1)
      val swapped = result(i)
      result(i) = result(j)
      result(j) = swapped
      i -= 1
    }
    result.toVector.asInstanceOf[Vector[A]]
  }
}

object Paths {

  /** Steps from `from` to every cell within reach, -1 for the others. A cell
    * that is not `open` is reached but not walked through.
    */
  def distances(width: Int, height: Int, from: Pos)(open: Pos => Boolean): Array[Int] = {
    val result = Array.fill(width * height)(-1)
    val queue = mutable.Queue(from)
    result(from.y * width + from.x) = 0
    while(queue.nonEmpty) {
      val at = queue.dequeue()
      val next = result(at.y * width + at.x) + 1
      if(next == 1 || open(at)) Pos.Directions.foreach { direction =>
        val to = at + direction
        if(to.x >= 0 && to.y >= 0 && to.x < width && to.y < height && result(to.y * width + to.x) < 0) {
          result(to.y * width + to.x) = next
          queue.enqueue(to)
        }
      }
    }
    result
  }

  /** The cells to walk through, without `from` and with `to`, which does not
    * have to be open itself. Empty when there is no way.
    */
  def find(width: Int, height: Int, from: Pos, to: Pos)(open: Pos => Boolean): Vector[Pos] = {
    val distance = distances(width, height, to)(open)
    def at(p: Pos): Int =
      if(p.x < 0 || p.y < 0 || p.x >= width || p.y >= height) -1 else distance(p.y * width + p.x)
    if(at(from) < 0) Vector.empty
    else {
      val path = Vector.newBuilder[Pos]
      var current = from
      while(current != to) {
        current = current.neighbors.filter(p => at(p) == at(current) - 1 && (p == to || open(p))).head
        path += current
      }
      path.result()
    }
  }

  /** The cells of a straight line, without `from` and with `to`. */
  def line(from: Pos, to: Pos): Vector[Pos] = {
    val steps = from.distance(to)
    Vector.tabulate(steps) { i =>
      val t = (i + 1).toDouble / steps
      Pos(scala.math.round(from.x + (to.x - from.x) * t).toInt, scala.math.round(from.y + (to.y - from.y) * t).toInt)
    }
  }
}

/** What can be seen from a cell, by recursive shadow casting. */
object Fov {
  private val Octants = Vector(
    (1, 0, 0, 1), (0, 1, 1, 0), (0, -1, 1, 0), (-1, 0, 0, 1),
    (-1, 0, 0, -1), (0, -1, -1, 0), (0, 1, -1, 0), (1, 0, 0, -1)
  )

  def compute(origin: Pos, radius: Int)(opaque: Pos => Boolean): Set[Pos] = {
    val visible = mutable.HashSet(origin)

    def cast(row: Int, startSlope: Double, endSlope: Double, octant: (Int, Int, Int, Int)): Unit = {
      val (xx, xy, yx, yy) = octant
      var start = startSlope
      var distance = row
      var blocked = false
      while(distance <= radius && !blocked && start >= endSlope) {
        var nextStart = start
        var dx = -distance
        while(dx <= 0) {
          val left = (dx - 0.5) / (-distance + 0.5)
          val right = (dx + 0.5) / (-distance - 0.5)
          if(start >= right && endSlope <= left) {
            val p = Pos(origin.x + dx * xx - distance * xy, origin.y + dx * yx - distance * yy)
            if(dx * dx + distance * distance <= radius * radius + radius) visible += p
            if(blocked) {
              if(opaque(p)) nextStart = right
              else {
                blocked = false
                start = nextStart
              }
            } else if(opaque(p) && distance < radius) {
              blocked = true
              cast(distance + 1, start, left, octant)
              nextStart = right
            }
          }
          dx += 1
        }
        distance += 1
      }
    }

    Octants.foreach(cast(1, 1.0, 0.0, _))
    visible.toSet
  }
}
