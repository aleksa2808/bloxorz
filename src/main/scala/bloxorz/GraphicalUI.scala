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
import java.io.File
import scalafx.scene.control.ComboBox

class Level(val filePath: String)
    extends GameDef
    with FileParserTerrain
    with Playable

class GameThread(
    level: Level,
    reportBlockState: Block => Unit,
    getNextMove: () => Move,
    endCallback: GameResult => Unit
) extends Thread {
  override def run() {
    try {
      val gameResult = level.play(reportBlockState, getNextMove())
      endCallback(gameResult)
    } catch {
      case e: IllegalMonitorStateException =>
        println("gameThread was waiting for a move")
    }
  }
}

object GraphicalUI extends JFXApp {
  val blockColor = Color.RED
  val fieldToColorMap = Map[Field, Color](
    Start -> Color.BLUE,
    Goal -> Color.GREEN,
    Normal -> Color.LIGHTBLUE,
    Weak -> Color.LIGHTSALMON,
    Nil -> Color.LIGHTGREEN
  )

  stage = new JFXApp.PrimaryStage {
    title = "Bloxorz"
    width = 400
    height = 400

    val menuScene = new Scene(400, 400) {
      val playButton = new Button("Play")
      playButton.layoutX = 20
      playButton.layoutY = 20
      playButton.onAction = (e: ActionEvent) => {
        val level = new Level(levelNameFileMap(levelCBox.value()))

        val moveQueue: BlockingQueue[Move] = new LinkedBlockingQueue
        val playScene = gameScene(level, moveQueue)
        playScene.onKeyPressed = (e: KeyEvent) => {
          e.code match {
            case KeyCode.W => moveQueue.put(Up)
            case KeyCode.S => moveQueue.put(Down)
            case KeyCode.A => moveQueue.put(Left)
            case KeyCode.D => moveQueue.put(Right)
            case _         =>
          }
        }
        setScene(playScene)
      }

      val levelLabel = new Label("Level:")
      levelLabel.layoutX = 20
      levelLabel.layoutY = 50

      def getListOfFiles(dir: File): List[File] =
        dir.listFiles.filter(_.isFile).toList

      val levelFiles =
        getListOfFiles(new File("/home/murtaugh/master/fp/levels"))
          .sortBy(f => f.getName())
      assert(!levelFiles.isEmpty)

      val levelNameFileMap =
        levelFiles.map(f => f.getName() -> f.getCanonicalPath()).toMap
      val levels = levelNameFileMap.keySet.toList.sorted

      val levelCBox = new ComboBox(levels)
      levelCBox.layoutX = 20
      levelCBox.layoutY = 80
      levelCBox.getSelectionModel().selectFirst()

      content = List(playButton, levelLabel, levelCBox)
    }

    def gameScene(level: Level, moveQueue: BlockingQueue[Move]): Scene =
      new Scene(400, 400) {
        new GameThread(
          level,
          block => Platform.runLater(show(block)),
          () => moveQueue.take(),
          gameResult => {
            Thread.sleep(500)
            gameResult match {
              case Win =>
                Platform.runLater(setScene(menuScene)) // TODO: next level
              case Lose => Platform.runLater(setScene(menuScene))
            }
          }
        ).start()

        content = gamePane

        val squareSize = 400.0 / level.vector.size

        val blockRect = new Rectangle {
          width = squareSize * (1 + (level.startBlock.b2.col - level.startBlock.b1.col))
          height = squareSize * (1 + (level.startBlock.b2.row - level.startBlock.b1.row))
          x = level.startBlock.b1.col * squareSize
          y = level.startBlock.b1.row * squareSize
          fill = blockColor
        }

        lazy val gamePane: Pane = new Pane {
          children = makeBoard(squareSize, level)
        }

        def show(block: Block): Unit = {
          val Block(b1, b2) = block

          blockRect.width = squareSize * (1 + (b2.col - b1.col))
          blockRect.height = squareSize * (1 + (b2.row - b1.row))
          blockRect.x = b1.col * squareSize
          blockRect.y = b1.row * squareSize

          gamePane.children = makeBoard(squareSize, level) :+ blockRect
        }
      }

    def setScene(s: Scene) = {
      scene = s
    }
    setScene(menuScene)

    // width onChange (show())
    // height onChange (show())
  }

  def makeBoard(
      squareSize: Double,
      level: Level
  ) = {
    for (i <- 0 until level.vector.size;
         j <- 0 until level.vector.head.size)
      yield
        new Rectangle {
          width = squareSize
          height = squareSize
          x = j * squareSize
          y = i * squareSize
          fill = fieldToColorMap(level.vector(i)(j))
        }
  }

  override def stopApp(): Unit = {
    // gameThread.stop()
  }
}
