package bloxorz

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import scala.annotation.tailrec
import java.util.Scanner
import javafx.scene.paint.Color
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.Platform
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.control.Label
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.HBox
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Circle
import scalafx.scene.shape.Rectangle
import akka.actor.ActorRef
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.KeyCode
import akka.util.BoundedBlockingQueue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue

class Level(val filePath: String)
    extends GameDef
    with FileParserTerrain
    with Playable

// sealed trait Message
// case class BlockStateMessage(b: Block) extends Message
// case class MoveMessage(m: Move) extends Message
// case class RegisterGameMessage() extends Message
// case class GameResultMessage(gr: GameResult) extends Message

// class GameActor(level: Level, ui: ActorRef) extends Actor {
//   ui ! RegisterGameMessage

//   def reportBlockState(block: Block) = {
//     ui ! BlockStateMessage(block)
//   }

//   val queue: BlockingQueue[Move] = new LinkedBlockingQueue

//   def nextMove = {
//     queue.take()
//   }

//   level.play(reportBlockState, nextMove)

//   def receive = {
//     case MoveMessage(move) => queue.put(move)
//     case _                 =>
//   }
// }

// class UIActor extends Actor {
//   private var game: Option[ActorRef] = None

//   def receive = {
//     case RegisterGameMessage => game = Some(sender)
//     case BlockStateMessage(block) =>
//       Platform.runLater { GraphicalUI show block }
//     case MoveMessage(move) =>
//       game match {
//         case Some(g) => g ! MoveMessage(move)
//         case None    =>
//       }
//     // case UpdateMainVariant(variant: Variant) =>
//     //   Platform.runLater {
//     //     GraphicalUI.mainVariantLabel.text = "Main Variant = (%s)" format variant.show
//     //   }
//   }
// }

object GraphicalUI extends JFXApp {
  val level = new Level("/home/murtaugh/master/fp/levels/1")

  val blockColor = Color.RED
  val fieldToColorMap = Map[Field, Color](
    Start -> Color.BLUE,
    Goal -> Color.GREEN,
    Normal -> Color.LIGHTBLUE,
    Weak -> Color.LIGHTSALMON,
    Nil -> Color.LIGHTGREEN
  )

  val queue: BlockingQueue[Move] = new LinkedBlockingQueue

  val gameThread = new Thread {
    def reportBlockState(block: Block) {
      Platform.runLater(show(block))
    }

    def nextMove = {
      queue.take()
    }

    override def run() {
      try {
        level.play(reportBlockState, nextMove) match {
          case Win  => println("You win!")
          case Lose => println("You lose!")
        }
      } catch {
        case e: IllegalMonitorStateException =>
          println("gameThread was waiting for a move")
      }
    }
  }
  gameThread.start()

  // val akka = ActorSystem("bloxorz")
  // val uiActor = akka.actorOf(Props[UIActor], name = "ui")
  // val gameActor =
  //   akka.actorOf(Props(new GameActor(level, uiActor)), name = "game")

  stage = new JFXApp.PrimaryStage {
    title = "Bloxorz"
    width = 400
    height = 400

    scene = new Scene(400, 400) {
      content = gamePane
      onKeyPressed = (e: KeyEvent) => {
        e.code match {
          // case KeyCode.W => uiActor ! MoveMessage(Up)
          // case KeyCode.S => uiActor ! MoveMessage(Down)
          // case KeyCode.A => uiActor ! MoveMessage(Left)
          // case KeyCode.D => uiActor ! MoveMessage(Right)
          case KeyCode.W => queue.put(Up)
          case KeyCode.S => queue.put(Down)
          case KeyCode.A => queue.put(Left)
          case KeyCode.D => queue.put(Right)
          case _         =>
        }
      }
    }

    // width onChange (show())
    // height onChange (show())
  }

  lazy val gamePane: Pane = new Pane {
    children = showBoard(400, 400, level.startBlock)
  }

  def show(block: Block): Unit = {
    gamePane.children = showBoard(400, 400, block)
  }

  def showBoard(
      paneWidth: Double,
      paneHeight: Double,
      block: Block
  ) = {
    val Block(b1, b2) = block
    for (i <- 0 until level.vector.size;
         j <- 0 until level.vector.head.size)
      yield
        new Rectangle {
          width = paneWidth / level.vector.size
          height = width()
          x = j * width.get()
          y = i * height.get()
          fill = b1 == Pos(i, j) || b2 == Pos(i, j) match {
            case true  => blockColor
            case false => fieldToColorMap(level.vector(i)(j))
          }
        }
  }

  override def stopApp(): Unit = {
    gameThread.stop()
  }

  // override def stopApp = {
  //   akka stop gameActor
  //   akka stop uiActor
  //   akka.terminate
  // }
}
