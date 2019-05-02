package bloxorz

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

object GraphicalUI extends JFXApp {
  class Level(val filePath: String)
      extends GameDef
      with FileParserTerrain
      with Playable
  val level = new Level("/home/murtaugh/master/fp/levels/3")

  val blockColor = Color.Orange
  val fieldToColorMap = Map[Field, Color](
    Start -> Color.Blue,
    Goal -> Color.Green,
    Normal -> Color.LightBlue,
    Weak -> Color.LightSalmon,
    Nil -> Color.LightGreen
  )

  stage = new JFXApp.PrimaryStage {
    title.value = "Bloxorz"
    width = 600
    height = 450
    scene = new Scene {
      fill = fieldToColorMap(Nil)
      content = for (i <- 0 until level.vector.size;
                     j <- 0 until level.vector.head.size)
        yield
          new Rectangle {
            width = 600 / level.vector.size
            height = width.get()
            x = j * width.get()
            y = i * height.get()
            fill = fieldToColorMap(level.vector(i)(j))
            // fill <== when(hover) choose Green otherwise Red
          }
    }
  }
}
