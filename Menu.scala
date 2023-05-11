import FxApp._
import Projeto.{BoardState, Random, RandomState}
import javafx.animation.{Animation, Interpolator, Timeline, TranslateTransition}
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.scene.{Node, Parent, Scene}
import javafx.scene.control.{CheckBox, ColorPicker, Label, RadioButton}
import javafx.scene.layout.{AnchorPane, Background, BackgroundFill}
import javafx.scene.paint.{Color, CycleMethod, LinearGradient, Stop}
import javafx.stage.Stage
import javafx.util.Duration

import java.net.URL
import java.util.ResourceBundle

class Menu  extends Initializable{

  @FXML private var colorpicker1 : ColorPicker = _
  @FXML private var colorpicker2 : ColorPicker = _
  @FXML private var radioButton : RadioButton = _
  @FXML private var title : Label = _
  @FXML private var anchorPane : AnchorPane = _

  var node : Node = _

  def playPVP():Unit={
    initGame(1)
  }

  def playPVC(): Unit = {
    initGame(0)
  }

  def playPVCA():Unit={
    initGame(2)
  }
  def clearFocus(): Unit = {
    anchorPane.requestFocus()
  }
  def initGame(gametype : Int):Unit={
    playing=radioButton.isSelected
    starting = playing
    p1Color = colorpicker1.getValue
    p2Color = colorpicker2.getValue
    gameType = gametype


    var secondaryStage = new Stage();
    secondaryStage.setTitle("HexGame")
    val fxmlLoader = new FXMLLoader(getClass.getResource("Game.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    mainViewRoot.requestFocus()

    val controller: Game = fxmlLoader.getController

    controller.setMainMenu(title.getScene.getWindow.asInstanceOf[Stage])

    val scene = new Scene(mainViewRoot)
    secondaryStage.setResizable(false)
    secondaryStage.setScene(scene)
    secondaryStage.show()
    controller.clearFocus()
    if (gameType != 1 && !playing) {
      playing = true
      controller.checkHist = true
      controller.playCPU()
    }
    controller.setColors()
    title.getScene.getWindow.hide()
  }

  def ColorChangeP1(): Unit = {

    p1Color = colorpicker1.getValue

  }

  def ColorChangeP2(): Unit = {
    p2Color = colorpicker2.getValue

  }

  override def initialize(url: URL, resourceBundle: ResourceBundle): Unit = {
    var translate = new TranslateTransition()
    translate.setNode(title)
    translate.setDuration(Duration.millis(1000))
    translate.setCycleCount(Animation.INDEFINITE)
    translate.setByY(-10)
    translate.setInterpolator(Interpolator.EASE_BOTH)
    translate.setAutoReverse(true)
    translate.play()
  }


}
