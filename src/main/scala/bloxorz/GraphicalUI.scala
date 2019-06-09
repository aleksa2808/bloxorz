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
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.VBox

class FileLevel(val filePath: String) extends GameDef with FileParserTerrain

class GameThread(
    level: GameDef,
    reportBlockState: Block => Unit,
    getNextMove: () => Move,
    endCallback: GameResult => Unit
) extends Thread {
  override def run() {
    try {
      val gameResult = Player.play(level, reportBlockState, getNextMove())
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

  val levelNameFileMap = new File("/home/murtaugh/master/fp/levels").listFiles
    .filter(_.isFile)
    .toList
    .sortBy(f => f.getName())
    .map(f => f.getName() -> f.getCanonicalPath())
    .toMap

  val levelNames = levelNameFileMap.keySet.toList.sorted
  assert(!levelNames.isEmpty)

  def awsdInput(e: KeyEvent): Option[Move] = {
    e.code match {
      case KeyCode.W => Some(Up)
      case KeyCode.S => Some(Down)
      case KeyCode.A => Some(Left)
      case KeyCode.D => Some(Right)
      case _         => None
    }
  }

  stage = new JFXApp.PrimaryStage {
    title = "Bloxorz"
    width = 400
    height = 400

    val menuScene: Scene = new Scene(width(), height()) {
      val playButton = new Button("Play") {
        layoutX = 20
        layoutY = 20
        onAction = (e: ActionEvent) => {
          val levels = levelNames
            .dropWhile(_ != levelCBox.value())
            .map(lName => new FileLevel(levelNameFileMap(lName)))
          // new Level(levelNameFileMap(levelCBox.value()))

          val playScene = setupPlayScene(levels)
          setScene(playScene)
        }
      }

      val solveButton: Button = new Button("Solve") {
        layoutX = 80
        layoutY = 20
        onAction = (e: ActionEvent) => {
          val level = new FileLevel(levelNameFileMap(levelCBox.value()))
          new Solver(level).solution match {
            case Some(s) => {
              val moveQueue: BlockingQueue[Move] = new LinkedBlockingQueue
              val solveScene: Scene = gameScene(
                level,
                moveQueue,
                List()
              )

              // Solver thread
              val solverThread = new Thread {
                override def run() {
                  for (m <- s) {
                    Thread.sleep(400)
                    moveQueue.put(m)
                  }
                }
              }
              solverThread.setDaemon(true)
              solverThread.start()

              setScene(solveScene)
            }
            case None => println("Level isn't solvable")
          }
        }
      }

      val editButton: Button = new Button("Edit") {
        layoutX = 140
        layoutY = 20
        onAction = (e: ActionEvent) => {
          val pickedLevel = new FileLevel(levelNameFileMap(levelCBox.value()))
          val editScene: Scene = makeEditScene(pickedLevel)
          setScene(editScene)
        }
      }

      val levelLabel = new Label("Level:") {
        layoutX = 20
        layoutY = 50
      }

      val levelCBox = new ComboBox(levelNames) {
        layoutX = 20
        layoutY = 80
      }
      levelCBox.getSelectionModel().selectFirst()

      content = List(playButton, solveButton, editButton, levelLabel, levelCBox)
    }

    def setupPlayScene(levels: List[GameDef]) = {
      val moveQueue: BlockingQueue[Move] = new LinkedBlockingQueue
      val playScene: Scene = gameScene(
        levels.head,
        moveQueue,
        levels.tail
      )
      playScene.onKeyPressed = (e: KeyEvent) => {
        awsdInput(e) match {
          case Some(move) => moveQueue.put(move)
          case _          =>
        }
      }
      playScene
    }

    def gameScene(
        level: GameDef,
        moveQueue: BlockingQueue[Move],
        nextLevelList: List[GameDef]
    ): Scene =
      new Scene(400, 400) {
        val gameThread =
          new GameThread(
            level,
            block => Platform.runLater(moveBlock(block)),
            moveQueue.take,
            gameResult => {
              Thread.sleep(500)
              gameResult match {
                case Win =>
                  nextLevelList match {
                    case List() => Platform.runLater(setScene(menuScene))
                    case head :: tail =>
                      Platform.runLater {
                        val scene = gameScene(head, moveQueue, tail)
                        scene.onKeyPressed = (e: KeyEvent) => {
                          awsdInput(e) match {
                            case Some(move) => moveQueue.put(move)
                            case _          =>
                          }
                        }
                        setScene(scene)
                      }
                  }
                case Lose => Platform.runLater(setScene(menuScene))
              }
            }
          )
        gameThread.setDaemon(true)
        gameThread.start()

        lazy val squareSize = width() / level.vector.head.size

        lazy val board =
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

        lazy val blockRect = new Rectangle {
          width = squareSize * (1 + (level.startBlock.b2.col - level.startBlock.b1.col))
          height = squareSize * (1 + (level.startBlock.b2.row - level.startBlock.b1.row))
          x = level.startBlock.b1.col * squareSize
          y = level.startBlock.b1.row * squareSize
          fill = blockColor
        }

        lazy val gamePane: Pane = new Pane {
          children = board :+ blockRect
        }

        content = gamePane

        def moveBlock(block: Block): Unit = {
          val Block(b1, b2) = block
          blockRect.width = squareSize * (1 + (b2.col - b1.col))
          blockRect.height = squareSize * (1 + (b2.row - b1.row))
          blockRect.x = b1.col * squareSize
          blockRect.y = b1.row * squareSize
        }
      }

    def makeEditScene(level: GameDef): Scene =
      new Scene(400, 500) {
        val editActions = Map[String, EditAction](
          "Remove tile" -> RemoveTile,
          "Add tile" -> AddTile,
          "Set normal to weak" -> ReplaceNormalWithWeak,
          "Set weak to normal" -> ReplaceWeakWithNormal,
          "Place start tile" -> PlaceStartTile,
          "Place goal tile" -> PlaceGoalTile
        )
        val editActionList: List[String] = editActions.keySet.toList

        val editQueue: BlockingQueue[Option[Edit]] = new LinkedBlockingQueue

        // Editor thread
        val editThread = new Thread {
          override def run() {
            try {
              val editedLevel = Editor.editLevel(
                level,
                modifiedLevel =>
                  Platform.runLater(boardPane.children = board(modifiedLevel)),
                editQueue.take
              )

              val playScene = setupPlayScene(List(editedLevel))
              Platform.runLater(setScene(playScene))
            } catch {
              case e: IllegalMonitorStateException =>
                println("editor thread stopped")
            }
          }
        }
        editThread.setDaemon(true)
        editThread.start()

        lazy val squareSize = width() / level.vector.head.size

        val actionCBox = new ComboBox(editActionList) {
          layoutX = 80
          layoutY = 20
        }
        actionCBox.getSelectionModel().selectFirst()

        lazy val menuPane = new Pane {
          prefHeight = 70

          val finishButton = new Button("Finish") {
            layoutX = 20
            layoutY = 20
            onAction = (e: ActionEvent) => editQueue.put(None)
          }

          children = List(finishButton, actionCBox)
        }

        def board(level: GameDef) =
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

        lazy val boardPane: Pane = new Pane {
          children = board(level)
          onMouseClicked = (e: MouseEvent) => {
            val pos =
              Pos(
                Math.floor((e.y) / squareSize).toInt,
                Math.floor(e.x / squareSize).toInt
              )
            println(pos)
            editQueue.put(Some(Edit(pos, editActions(actionCBox.value()))))
          }
        }

        content = new VBox {
          children = List(menuPane, boardPane)
        }
      }

    def setScene(s: Scene) = {
      scene = s
    }
    setScene(menuScene)

    // width onChange (show())
    // height onChange (show())
  }
}
