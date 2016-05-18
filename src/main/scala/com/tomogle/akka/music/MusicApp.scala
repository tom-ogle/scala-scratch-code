package com.tomogle.akka.music

import akka.actor.{Actor, ActorSystem, Props}
import com.tomogle.akka.music.MusicController.{Play, Stop}
import com.tomogle.akka.music.MusicPlayer.{StartMusic, StopMusic}

object MusicPlayer {
  sealed trait PlayMessage
  case object StopMusic extends PlayMessage
  case object StartMusic extends PlayMessage
}
class MusicPlayer extends Actor {

  private var musicPlaying = false

  override def receive: Receive = {
    case StopMusic => if(musicPlaying) {
      println("Stopping music")
      musicPlaying = false
    } else {
      println("Music is not playing")
    }
    case StartMusic =>
      if(musicPlaying) {
        println("Music is already playing")
      } else {
        musicPlaying = true
        println("Starting music")
        // Only one actor can be created with the same name
        val musicController = context.actorOf(MusicController.props, "music-controller")
        musicController ! Play
      }
    case _ => println("Unknown message")
  }
}

object MusicController {
  sealed trait ControllerMessage
  case object Play extends ControllerMessage
  case object Stop extends ControllerMessage

  def props = Props[MusicController]
}
class MusicController extends Actor {
  override def receive: Receive = {
    case Play => println("Started music")
    case Stop => println("Stopped music")
    case _ => println("Unknown message")
  }
}


object MusicApp extends App {

  val system = ActorSystem("music-system")
  val musicPlayer = system.actorOf(Props[MusicPlayer], "Music-Player")
  musicPlayer ! StopMusic
  musicPlayer ! StartMusic
  musicPlayer ! StartMusic
  musicPlayer ! StopMusic
  system.terminate()
}
