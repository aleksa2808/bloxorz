package bloxorz

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
import scalafx.scene.layout.HBox
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Rectangle
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.KeyCode
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import scalafx.scene.control.ComboBox
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.VBox
import scalafx.stage.FileChooser
import scala.io.Source
import collection.immutable.ListMap

class GameThread(
    level: GameDef,
    reportBlockState: Block => Unit,
    getNextMove: () => Option[Move],
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

  val boardWidth = 400
  def board(level: GameDef, squareSize: Double) =
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

  stage = new JFXApp.PrimaryStage {
    title = "Bloxorz"
    resizable = false

    val menuScene: Scene = new Scene {
      val playButton = new Button("Play") {
        onAction = (e: ActionEvent) => {
          val levels = levelNames
            .dropWhile(_ != levelCBox.value())
            .map(lName => new FileLevel(levelNameFileMap(lName)))

          setPlayScene(levels)
        }
      }

      val fileButton: Button = new Button("Load solution") {
        onAction = (e: ActionEvent) => {
          val fileChooser = new FileChooser
          val solutionFile = fileChooser.showOpenDialog(stage)

          solutionFile match {
            case null => println("No file selected")
            case _ => {
              val charMoveMap = Map[Char, Move](
                'u' -> Up,
                'd' -> Down,
                'l' -> Left,
                'r' -> Right
              )

              val fileLines = Source.fromFile(solutionFile).getLines.toList
              fileLines.forall(
                line =>
                  line.length() == 1 && charMoveMap.keySet.contains(line.head)
              ) match {
                case false => println("Invalid solution file format")
                case true => {
                  val level = new FileLevel(levelNameFileMap(levelCBox.value()))
                  val moveList =
                    fileLines.map(line => charMoveMap(line.head))

                  val moveQueue: BlockingQueue[Option[Move]] =
                    new LinkedBlockingQueue

                  val fileScene: Scene = gameScene(
                    level,
                    moveQueue,
                    List(),
                    true
                  )

                  setScene(fileScene)

                  for (m <- moveList) {
                    moveQueue.put(Some(m))
                  }
                  moveQueue.put(None)
                }
              }
            }
          }
        }
      }

      val solveButton: Button = new Button("Solve") {
        onAction = (e: ActionEvent) => {
          val level = new FileLevel(levelNameFileMap(levelCBox.value()))
          new Solver(level).solution match {
            case Some(s) => {
              val moveQueue: BlockingQueue[Option[Move]] =
                new LinkedBlockingQueue

              val solveScene: Scene = gameScene(
                level,
                moveQueue,
                List(),
                true
              )

              setScene(solveScene)

              for (m <- s) {
                moveQueue.put(Some(m))
              }
            }
            case None => println("Level isn't solvable")
          }
        }
      }

      val editButton: Button = new Button("Edit") {
        onAction = (e: ActionEvent) => {
          val pickedLevel = new FileLevel(levelNameFileMap(levelCBox.value()))
          val editScene: Scene = makeEditScene(pickedLevel)
          setScene(editScene)
        }
      }

      val levelLabel = new Label("Level:")

      val levelCBox = new ComboBox(levelNames)
      levelCBox.getSelectionModel().selectFirst()

      content = new VBox(
        new HBox {
          padding = Insets(15, 12, 15, 12)
          spacing = 10
          children = List(
            playButton,
            fileButton,
            solveButton,
            editButton
          )
        },
        new HBox {
          padding = Insets(0, 12, 15, 12)
          spacing = 10
          children = List(levelLabel, levelCBox)
        }
      )
    }

    def setPlayScene(levels: List[GameDef]) = {
      val awsdMap = Map[KeyCode, Move](
        KeyCode.W -> Up,
        KeyCode.S -> Down,
        KeyCode.D -> Right,
        KeyCode.A -> Left
      )

      val moveQueue: BlockingQueue[Option[Move]] = new LinkedBlockingQueue
      val playScene: Scene = gameScene(
        levels.head,
        moveQueue,
        levels.tail,
        false
      )
      playScene.onKeyPressed = (e: KeyEvent) => {
        awsdMap.get(e.code) match {
          case None =>
          case move => moveQueue.put(move)
        }
      }
      setScene(playScene)
    }

    def gameScene(
        level: GameDef,
        moveQueue: BlockingQueue[Option[Move]],
        nextLevelList: List[GameDef],
        moveDelay: Boolean
    ): Scene =
      new Scene {
        val gameThread =
          new GameThread(
            level,
            block => Platform.runLater(moveBlock(block)),
            () => {
              val move = moveQueue.take
              moveDelay match {
                case true  => Thread.sleep(300)
                case false =>
              }
              move
            },
            gameResult => {
              Thread.sleep(500)
              gameResult match {
                case Win =>
                  nextLevelList match {
                    case List() => Platform.runLater(setScene(menuScene))
                    case _      => Platform.runLater(setPlayScene(nextLevelList))
                  }
                case Lose => Platform.runLater(setScene(menuScene))
              }
            }
          )
        gameThread.setDaemon(true)
        gameThread.start()

        lazy val squareSize = boardWidth / level.vector.head.size

        lazy val blockRect = new Rectangle {
          width = squareSize * (1 + (level.startBlock.b2.col - level.startBlock.b1.col))
          height = squareSize * (1 + (level.startBlock.b2.row - level.startBlock.b1.row))
          x = level.startBlock.b1.col * squareSize
          y = level.startBlock.b1.row * squareSize
          fill = blockColor
        }

        lazy val gamePane: Pane = new Pane {
          children = board(level, squareSize) :+ blockRect
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
      new Scene {
        val editActions = ListMap[String, LocalEditAction](
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
                  Platform.runLater {
                    boardPane.children = board(modifiedLevel, squareSize)
                    modifiedLevel.isValidFormat match {
                      case true  => finishButton.setDisable(false)
                      case false => finishButton.setDisable(true)
                    }
                  },
                editQueue.take
              )

              Platform.runLater(setPlayScene(List(editedLevel)))
            } catch {
              case e: IllegalMonitorStateException =>
                println("editor thread stopped")
            }
          }
        }
        editThread.setDaemon(true)
        editThread.start()

        lazy val squareSize = boardWidth / level.vector.head.size

        val actionCBox = new ComboBox(editActionList)
        actionCBox.getSelectionModel().selectFirst()

        val finishButton = new Button("Finish") {
          onAction = (e: ActionEvent) => editQueue.put(None)
        }

        lazy val menuPane = new Pane {

          val inverseButton = new Button("Inverse start and goal") {
            onAction = (e: ActionEvent) =>
              editQueue.put(Some(GlobalEdit(InverseLevel)))
          }

          val switchButton = new Button("Normalise weak tiles") {
            onAction = (e: ActionEvent) =>
              editQueue.put(Some(GlobalEdit(SwitchWeak)))
          }

          children = new VBox {
            padding = Insets(15, 12, 15, 12)
            spacing = 10
            children =
              List(finishButton, actionCBox, inverseButton, switchButton)
          }
        }

        lazy val boardPane: Pane = new Pane {
          children = board(level, squareSize)
          onMouseClicked = (e: MouseEvent) => {
            val pos =
              Pos(
                Math.floor((e.y) / squareSize).toInt,
                Math.floor(e.x / squareSize).toInt
              )
            editQueue.put(Some(LocalEdit(pos, editActions(actionCBox.value()))))
          }
        }

        content = new HBox {
          children = List(boardPane, menuPane)
        }
      }

    def setScene(s: Scene) = {
      scene = s
    }
    setScene(menuScene)
  }
}
