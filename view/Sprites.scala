package observatory
package core

import sgl.engine.gameobject.Color
import sgl.engine.gameobject.extras.Animation
import sgl.engine.gameobject.render2d.{Painter2D, SpriteVisual, Texture2D, TileQuad}
import observatory.model._
import sgl.geometry.Vec2

/** A cell of the tile sheet. */
final case class Sprite(column: Int, row: Int) {
  // Slightly inside of the cell, so that neighbors never bleed in.
  val u0: Float = (column * 16 + 0.02f) / Sprite.SheetWidth
  val v0: Float = (row * 16 + 0.02f) / Sprite.SheetHeight
  val u1: Float = (column * 16 + 15.98f) / Sprite.SheetWidth
  val v1: Float = (row * 16 + 15.98f) / Sprite.SheetHeight

  def visual(sheet: Texture2D): SpriteVisual = SpriteVisual.Texture(sheet, u0, v0, u1, v1)

  def quad(sheet: Texture2D, x: Float, y: Float, color: Color, width: Float = 16f, height: Float = 16f): TileQuad =
    TileQuad(sheet, x, y, width, height, u0, v0, u1, v1, color = color)

  def paint(painter: Painter2D, sheet: Texture2D, x: Float, y: Float, size: Float, color: Color): Unit =
    painter.image(sheet, Vec2(x, y), Vec2(x + size, y), Vec2(x + size, y + size), Vec2(x, y + size), u0, v0, u1, v1, color)
}

/** Which cell of Kenney's 1-Bit Pack stands for what. */
object Sprite {
  val SheetWidth: Float = 768f
  val SheetHeight: Float = 352f

  val Solid: Sprite = Sprite(8, 5)
  val Dots: Vector[Sprite] = Vector(Sprite(1, 0), Sprite(2, 0))
  val Bricks: Sprite = Sprite(10, 17)
  val Reeds: Vector[Sprite] = Vector(Sprite(5, 0), Sprite(6, 0), Sprite(0, 2))
  val Waves: Sprite = Sprite(18, 5)
  val LockedDoor: Sprite = Sprite(0, 9)
  val Trader: Sprite = Sprite(26, 9)
  val ClosedDoor: Sprite = Sprite(3, 9)
  val OpenDoor: Sprite = Sprite(2, 9)
  val Shelf: Sprite = Sprite(3, 7)
  val Pillar: Sprite = Sprite(18, 6)
  val StairsDown: Sprite = Sprite(3, 6)
  val StairsUp: Sprite = Sprite(21, 1)
  val Flame: Sprite = Sprite(28, 11)
  val Vent: Sprite = Sprite(24, 14)
  val Player: Sprite = Sprite(28, 0)
  val Heart: Sprite = Sprite(39, 10)
  val Sleep: Sprite = Sprite(37, 13)

  /** The ones that the sheet has a second look of. */
  def animated(kind: MonsterKind): Animation[Sprite] = kind match {
    case MonsterKind.TideSpirit => Animation(Vector(of(kind), Sprite(27, 6)), 0.35f)
    case MonsterKind.BrassCrab => Animation(Vector(of(kind), Sprite(24, 5)), 0.6f)
    case _ => Animation(Vector(of(kind)), 1f)
  }

  def of(kind: MonsterKind): Sprite = kind match {
    case MonsterKind.BrassCrab => Sprite(25, 5)
    case MonsterKind.LanternBat => Sprite(26, 8)
    case MonsterKind.InkSlug => Sprite(27, 8)
    case MonsterKind.DrownedScholar => Sprite(29, 6)
    case MonsterKind.Sentry => Sprite(31, 6)
    case MonsterKind.ReedEel => Sprite(28, 8)
    case MonsterKind.TideSpirit => Sprite(26, 6)
    case MonsterKind.Guardian => Sprite(30, 6)
    case MonsterKind.Curator => Sprite(25, 8)
    case MonsterKind.Mudlark => Sprite(31, 8)
    case MonsterKind.Mercury => Sprite(28, 6)
    case MonsterKind.GlassArcher => Sprite(28, 5)
    case MonsterKind.Archivist => Sprite(24, 2)
    case MonsterKind.Engine => Sprite(27, 5)
    case MonsterKind.Leviathan => Sprite(29, 8)
  }

  def of(item: Item): Sprite = item.kind match {
    case kind: ItemKind.Instrument =>
      Vector(Sprite(32, 6), Sprite(37, 7), Sprite(32, 7), Sprite(34, 4), Sprite(32, 8))(ItemKind.instruments.indexOf(kind))
    case kind: ItemKind.Coat =>
      Vector(Sprite(32, 1), Sprite(33, 1), Sprite(34, 1), Sprite(36, 1))(ItemKind.coats.indexOf(kind))
    case _: ItemKind.Lens => Sprite(43, 6)
    case _: ItemKind.Vial => Sprite(32, 13)
    case _: ItemKind.Chart => Sprite(33, 15)
    case _: ItemKind.Prism => Sprite(33, 10)
    case ItemKind.Oil => Sprite(34, 13)
    case ItemKind.Key => Sprite(32, 11)
    case ItemKind.Coins => Sprite(41, 4)
    case ItemKind.Lodestar => Sprite(32, 10)
  }
}

object Palette {
  def rgb(value: Int, alpha: Float = 1f): Color =
    Color(((value >> 16) & 0xff) / 255f, ((value >> 8) & 0xff) / 255f, (value & 0xff) / 255f, alpha)

  val Night: Color = rgb(0x0b0d17)
  val Ink: Color = rgb(0x141827)
  val Panel: Color = rgb(0x1b2136, 0.96f)
  val Raised: Color = rgb(0x2b3353)
  val Hover: Color = rgb(0x3c4770)
  val Text: Color = rgb(0xe8e4d4)
  val Dim: Color = rgb(0x8f96ad)
  val Gold: Color = rgb(0xf2c14e)
  val Red: Color = rgb(0xe4572e)
  val Green: Color = rgb(0x76b041)
  val Blue: Color = rgb(0x4d9de0)
  val Water: Color = rgb(0x2e6f95)
  val Gas: Color = rgb(0x9bc53d)

  final case class Stone(wall: Color, face: Color, floor: Color, ground: Color)

  def of(biome: Biome): Stone = biome match {
    case Biome.Archive => Stone(rgb(0x33283a), rgb(0xa37c5b), rgb(0x6b5a55), rgb(0x1d1820))
    case Biome.Orrery => Stone(rgb(0x3b3320), rgb(0xc59b48), rgb(0x6f6444), rgb(0x1e1b14))
    case Biome.Cistern => Stone(rgb(0x1b3a3f), rgb(0x5aa39b), rgb(0x3f6a68), rgb(0x10201f))
    case Biome.StarWell => Stone(rgb(0x2a2350), rgb(0x8f7fe0), rgb(0x54498f), rgb(0x141128))
  }

  def of(kind: MonsterKind): Color = kind match {
    case MonsterKind.BrassCrab => rgb(0xe0a34a)
    case MonsterKind.LanternBat => rgb(0xf2d16b)
    case MonsterKind.InkSlug => rgb(0x9a7bd1)
    case MonsterKind.DrownedScholar => rgb(0xb9d6c8)
    case MonsterKind.Sentry => rgb(0xd98e4a)
    case MonsterKind.ReedEel => rgb(0x6fc27a)
    case MonsterKind.TideSpirit => rgb(0x79c7f0)
    case MonsterKind.Guardian => rgb(0xf0b13c)
    case MonsterKind.Curator => rgb(0xe05780)
    case MonsterKind.Mudlark => rgb(0x9c8465)
    case MonsterKind.Mercury => rgb(0xc3d0de)
    case MonsterKind.GlassArcher => rgb(0x9fe8e0)
    case MonsterKind.Archivist => rgb(0xe8c99a)
    case MonsterKind.Engine => rgb(0xffb52e)
    case MonsterKind.Leviathan => rgb(0x2fd0a5)
  }

  private val PrismColors = Vector(0x9fd8ff, 0xff6b6b, 0xd59bf6, 0xb8f2c8).map(rgb(_))
  private val VialColors = Vector(0xf2a541, 0x3fc1b0, 0xb07fe8, 0xe9e6dd, 0xc65b3c).map(rgb(_))

  def of(game: Game, item: Item): Color = item.kind match {
    case kind: ItemKind.Vial => VialColors(game.look(kind))
    case _: ItemKind.Chart => rgb(0xe3d7b0)
    case _: ItemKind.Lens => rgb(0x8fd3ff)
    case ItemKind.Lodestar => rgb(0xfff3b0)
    case ItemKind.Oil => rgb(0xe8a33d)
    case ItemKind.Prism(effect) => PrismColors(effect.ordinal)
    case ItemKind.Key | ItemKind.Coins => Gold
    case _ if item.worn && item.cursed => rgb(0xd0455f)
    case _ => if(item.upgrade > 0) rgb(0x9fe08a) else rgb(0xc9ccd8)
  }

  def shade(color: Color, by: Float): Color = Color(color.red * by, color.green * by, color.blue * by, color.alpha)
}
