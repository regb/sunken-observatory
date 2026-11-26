package observatory
package core

import scala.collection.mutable

import sgl.{AudioProvider, SaveComponent}
import sgl.engine.gameobject._
import sgl.engine.gameobject.render2d.{Render2DModule, Texture2D}
import observatory.model.{Hint, Record, Records}
import sgl.geometry.Vec2
import sgl.util.Loader

/** A turn-based dungeon crawl down a flooded tower, on the GameObject engine. */
trait AbstractApp extends OpenGLGameObjectApp with AssetsProvider {
  this: AudioProvider with SaveComponent =>

  private val SaveName = "observatory"

  /** Where a new game comes from. Fixed by tests. */
  protected def newGameSeed: Long = System.currentTimeMillis

  private object services extends Services {
    private val sounds = mutable.Map.empty[SoundName, Loader[Audio.Sound]]

    def startLoading(): Unit = SoundName.values.foreach { name =>
      sounds(name) = Audio.loadSound(name match {
        case SoundName.Step => Assets.audio.step
        case SoundName.Hit => Assets.audio.hit
        case SoundName.Miss => Assets.audio.miss
        case SoundName.Hurt => Assets.audio.hurt
        case SoundName.Pickup => Assets.audio.pickup
        case SoundName.Mend => Assets.audio.mend
        case SoundName.Burst => Assets.audio.burst
        case SoundName.Descend => Assets.audio.descend
        case SoundName.Rank => Assets.audio.rank
      })
    }

    private val tracks = mutable.Map.empty[Track, Loader[Audio.Music]]
    private var playing: Option[Track] = None
    private var current: Settings = null
    private var toldHints: Set[Hint] = null

    def loadMusic(): Unit = Track.values.foreach { track =>
      val loading = Audio.loadMusic(track match {
        case Track.Surface => Assets.audio.surface
        case Track.Depths => Assets.audio.depths
      })
      loading.foreach { music =>
        music.setLooping(true)
        music.setVolume(settings.musicVolume)
      }
      tracks(track) = loading
    }

    private def loaded(track: Track): Option[Audio.Music] = tracks.get(track).flatMap(_.value).flatMap(_.toOption)

    override def music(track: Track): Unit = if(!playing.contains(track)) loaded(track).foreach { music =>
      playing.flatMap(loaded).foreach(_.stop())
      playing = Some(track)
      music.setVolume(settings.musicVolume)
      music.play()
    }
    def pauseMusic(): Unit = playing.flatMap(loaded).foreach(_.pause())
    def resumeMusic(): Unit = playing.flatMap(loaded).foreach(_.play())

    override def settings: Settings = {
      if(current == null) current = Save.getString(SaveName + "-settings").flatMap(Settings.read).getOrElse(Settings())
      current
    }
    override def changeSettings(settings: Settings): Unit = {
      current = settings
      Save.putString(SaveName + "-settings", settings.write)
      playing.flatMap(loaded).foreach(_.setVolume(settings.musicVolume))
    }

    override def records: Vector[Record] = Save.getString(SaveName + "-records").map(Records.read).getOrElse(Vector.empty)
    override def addRecord(record: Record): Unit = Save.putString(SaveName + "-records", Records.write(Records.add(records, record)))

    override def told: Set[Hint] = {
      if(toldHints == null) toldHints = Save.getString(SaveName + "-hints").map(_.split(" ").toVector).getOrElse(Vector.empty)
        .flatMap(name => Hint.values.find(_.toString == name)).toSet
      toldHints
    }
    override def tell(hint: Hint): Unit = {
      toldHints = told + hint
      Save.putString(SaveName + "-hints", toldHints.toVector.map(_.toString).sorted.mkString(" "))
    }

    override def screen: Vec2 = Vec2(Window.width.toFloat, Window.height.toFloat)
    override def loadSheet(context: GameContext)(using Scope): Loader[Texture2D] = {
      val renderer = context.engine.renderer
      renderer.whenReady(renderer.loadTexture2D(Assets.drawable.tiles))
    }
    // A sound that could not load is one that is not heard.
    override def play(sound: SoundName): Unit =
      sounds.get(sound).flatMap(_.value).flatMap(_.toOption).foreach { loaded =>
        if(settings.sound > 0) { val _ = loaded.play(settings.soundVolume * 0.7f) }
      }
    override def savedGame: Option[String] = Save.getString(SaveName).filter(_.nonEmpty)
    override def saveGame(save: String): Unit = Save.putString(SaveName, save)
    override def newSeed(): Long = newGameSeed
  }

  override def pause(): Unit = {
    services.pauseMusic()
    super.pause()
  }
  override def resume(): Unit = {
    super.resume()
    services.resumeMusic()
  }

  override def configure(builder: GameBuilder): Unit = {
    services.startLoading()
    services.loadMusic()
    builder
      .install(Render2DModule)
      .setGlobal(ClearColor(Palette.Night))
      .setGlobal[Services](services)
      .setInitialScene(TitleScene)
  }
}
