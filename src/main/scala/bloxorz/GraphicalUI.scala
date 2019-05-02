package bloxorz

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle

object GraphicalUI extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Bloxorz"
    width = 600
    height = 450
    scene = new Scene {
      fill = LightGreen
      content = for (i <- 0 to 5; j <- 0 to 5)
        yield
          new Rectangle {
            width = 100
            height = 100
            x = 25 + j * width.get()
            y = 40 + i * height.get()
            fill <== when(hover) choose Green otherwise Red
          }
    }
  }
}
