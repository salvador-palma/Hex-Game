import FxApp._
import Projeto.BoardState
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.Label
import javafx.scene.layout.AnchorPane
import javafx.stage.Stage

class Results {
  var mainMenu : Stage = _
  var result : String = _
  @FXML private var title: Label = _
  @FXML private var anchor: AnchorPane = _

  def setMainMenu(stage: Stage): Unit = {
    mainMenu = stage
  }

  def setResult(res: String): Unit = {
    result = res
    title.setText(res)
  }

  def menuClicked(): Unit = {
    menu.clearFocus()
    mainMenu.show()
    boardState = BoardState.defineBoard(5)
    history = List.empty[BoardState]
    moves = List.empty[(Int, Int)]
    anchor.getScene.getWindow.asInstanceOf[Stage].close()
  }

  def playClicked(): Unit = {
    boardState = BoardState.defineBoard(5)
    history = List.empty[BoardState]
    moves = List.empty[(Int, Int)]
    playing = starting

    var secondaryStage = new Stage();
    secondaryStage.setTitle("HexGame")
    val fxmlLoader = new FXMLLoader(getClass.getResource("Game.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val controller: Game = fxmlLoader.getController

    controller.setMainMenu(mainMenu)
    val scene = new Scene(mainViewRoot)
    secondaryStage.setScene(scene)
    secondaryStage.setResizable(false)
    secondaryStage.show()
    controller.clearFocus()
    if (gameType != 1 && !playing) {
      playing = true
      controller.checkHist = true
      controller.playCPU()
    }
    controller.setColors()
    title.getScene.getWindow.asInstanceOf[Stage].close()
  }

  def clearFocus(): Unit = {
    anchor.requestFocus()
  }
}
