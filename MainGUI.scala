import Projeto.{BoardState, Random, RandomState}
import Projeto.MainTUI.GetSize
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.paint.Color
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class MenuScreen extends Application {
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("HexGame - Menu")
    val fxmlLoader = new FXMLLoader(getClass.getResource("Menu.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val controller : Menu = fxmlLoader.getController
    FxApp.menu = controller
    primaryStage.setResizable(false)
    val scene = new Scene(mainViewRoot)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}
object FxApp {
  var gameType : Int = 1
  var playing : Boolean = true
  var starting : Boolean = true
  var boardState: BoardState = BoardState.defineBoard(5)
  var history : List[BoardState] =  List.empty[BoardState]
  var moves : List[(Int,Int)] = List.empty[(Int,Int)]
  var myRandom : RandomState = Random(30)

  var p1Color : Color = Color.BLUE
  var p2Color : Color = Color.RED
  var menu : Menu = _
  var lastCPUmove : (Int,Int) = (-1,-1)

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[MenuScreen], args: _*)
  }
}