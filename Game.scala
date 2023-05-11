import Projeto.{BoardState, Cells}
import javafx.event.ActionEvent
import javafx.scene.input.MouseEvent
import javafx.scene.shape.Polygon
import FxApp.{boardState, _}
import javafx.animation.{Animation, FadeTransition, Interpolator, TranslateTransition}
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.scene.control.Label
import javafx.scene.{Parent, Scene}
import javafx.scene.paint.Color
import javafx.scene.layout.AnchorPane
import javafx.stage.Stage
import javafx.util.Duration

import java.net.URL
import java.util.ResourceBundle

class Game  {


  @FXML private var anchor :AnchorPane = _
  @FXML private var undoLabel : Label = _
  @FXML private var borderp1 : Polygon=_
  @FXML private var borderp2 : Polygon=_
  var mainMenu : Stage = _
  var mainController : Menu = _
  var checkHist = false;
  private var firstCPUcoord : (Int,Int)=_
  def hexClicked(event: MouseEvent): Unit = {

    val source = event.getSource.asInstanceOf[Polygon]
    val id = source.getId
    val Y = id.charAt(3).asDigit - 1
    val X = id.charAt(4).asDigit - 1
    if(boardState.valid((Y,X))){
      history = boardState :: history
      boardState = boardState.play((X,Y),if (playing) Cells.Blue else Cells.Red)
      source.setFill(if (playing) p1Color else p2Color)
      source.getStyleClass.remove("hexbutton")
      boardState.hasContinuousLine match{
        case Some("P1") => win("PLAYER 1 WINS!")
        case Some("P2") => win("PLAYER 2 WINS!")
        case None=>

          if (gameType != 1) {
            playCPU()
          } else {
            playing = !playing
          }
          moves = (Y + 1, X + 1) :: moves
      }

    }
  }
  def undoMove(coords: (Int,Int)):Unit={
    val strID = "#hex" + coords._1 + "" + coords._2
    val poly = anchor.getScene.lookup(strID).asInstanceOf[Polygon]
    poly.getStyleClass.add("hexbutton")
    poly.setFill(Color.web("#d4aa70"))
  }

  def undoClicked():Unit={
    try{
      history match {
        case Nil => undoAnim("nothing to undo...")
        case h :: t =>
          if (gameType != 1) {
            t match {
              case Nil =>
                boardState = h
                history = t
                moves match {
                  case Nil => undoAnim("nothing to undo... 2")
                  case c :: tc =>
                    moves = tc
                    undoMove(c)
                }
              case h2 :: t2 =>
                boardState = h2
                history = t2
                moves match {
                  case c :: c2 :: tc =>
                    moves = tc
                    lastCPUmove = if (tc != Nil) {val d = tc.tail.head; (d._1 -1, d._2 -1)} else if(starting)(-1,-1) else firstCPUcoord
                    undoMove(c)
                    undoMove(c2)
                }
            }
          } else {
            boardState = h
            history = t
            moves match {
              case c :: tc =>
                moves = tc
                undoMove(c)
            }
          }

      }
      clearFocus()

    }catch {
      case _ => undoAnim("undo move not possible...")
    }
  }

  def playCPU():Unit = {
    if (!checkHist){history = boardState :: history}
    val ((cpuX, cpuY), nextRand) = if(gameType == 0) boardState.playCPU(myRandom) else boardState.playCPUAdj(myRandom, lastCPUmove)
    lastCPUmove = (cpuX,cpuY)
    myRandom = nextRand
    boardState = boardState.play((cpuY, cpuX), Cells.Red)
    val strID = "#hex" + (cpuX + 1) + "" + (cpuY + 1)
    val polyCPU = anchor.getScene.lookup(strID).asInstanceOf[Polygon]
    polyCPU.setFill(p2Color)
    polyCPU.getStyleClass.remove("hexbutton")
    //boardState.draw()
    if(!checkHist){moves = (cpuX+1,cpuY+1) :: moves}else{firstCPUcoord = lastCPUmove;checkHist=false}
    boardState.hasContinuousLine match{
      case None=>
      case _ => win("CPU WINS!")
    }
  }

  def setMainMenu(stage : Stage): Unit = {
    mainMenu = stage
  }
  def menuClicked(): Unit = {
    menu.clearFocus()
    mainMenu.show()
    boardState = BoardState.defineBoard(5)
    history = List.empty[BoardState]
    moves = List.empty[(Int, Int)]
    anchor.getScene.getWindow.asInstanceOf[Stage].close()
  }


  def win(message : String): Unit = {
    var resultsStage = new Stage();
    resultsStage.setTitle("HexGame - Results")
    val fxmlLoader = new FXMLLoader(getClass.getResource("Results.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val controller: Results = fxmlLoader.getController
    controller.setMainMenu(mainMenu)
    controller.setResult(message)

    val scene = new Scene(mainViewRoot)
    resultsStage.setScene(scene)
    resultsStage.setResizable(false)
    resultsStage.show()
    controller.clearFocus()
    anchor.getScene.getWindow.asInstanceOf[Stage].close()
  }

  def clearFocus(): Unit = {
    anchor.requestFocus()
  }


  def undoAnim(string: String): Unit = {
    undoLabel.setTranslateY(0)

    var translate = new TranslateTransition()
    undoLabel.setText(string)
    var fade = new FadeTransition()
    fade.setNode(undoLabel)
    translate.setNode(undoLabel)
    translate.setDuration(Duration.millis(1000))
    fade.setDuration(Duration.millis(1000))
    translate.setByY(-10)
    fade.setFromValue(1);
    fade.setToValue(0);
    translate.setInterpolator(Interpolator.EASE_BOTH)
    translate.setAutoReverse(true)
    fade.play()
    translate.play()
  }

  def setColors(): Unit = {
    borderp1.setFill(p1Color)
    borderp2.setFill(p2Color)
  }
}
