package Projeto
import Projeto.BoardState
import Projeto.Main.Board

import scala.annotation.tailrec

case class NeuralNetwork(IHW:List[List[Double]],HB:List[Double],HOW:List[List[Double]],OB:List[Double]){

  def Predict(board: Board,inputs : List[Double]):(Int,Int)= NeuralNetwork.Predict(IHW, HB, HOW, OB, board);
}

object NeuralNetwork{

  def Create(values: List[Double], Input: Int, Hidden: Int, Output: Int): NeuralNetwork = {
    val (inputHiddenWeights, y) = Compress(values,Hidden,Input)
    val (hiddenBias,y2) = Split(y,Hidden)
    val (hiddenOutputWeights,outputBias) = Compress(y2,Output,Hidden)
    NeuralNetwork(inputHiddenWeights, hiddenBias, hiddenOutputWeights, outputBias)

  }
  def Predict(IHW:List[List[Double]],HB:List[Double],HOW:List[List[Double]],OB:List[Double],board: Board): (Int,Int) = {
    def ActivateHiddenLayer(weights:List[List[Double]], inputs: List[Double], hiddenB:List[Double]):List[Double]={
      def getNodeValue(w: List[Double], i: List[Double], b: Double): Double = {
        w match {
          case Nil => b
          case x :: y => x * i.head + getNodeValue(y, i.tail, b)
        }
      }
      weights match{
        case Nil=> Nil
        case h::t=>Sigmoid(getNodeValue(h,inputs,hiddenB.head)) :: ActivateHiddenLayer(t,inputs,hiddenB.tail)
      }
    }
    def translateBoard(b: Board):List[Double]={
      b match{
        case Nil => Nil
        case h::t=> h.map(c=> if (c.equals(Cells.Blue)) 1 else if (c.equals(Cells.Red)) -1 else 0) ::: translateBoard(t)
      }
    }
    val Inputs = translateBoard(board)
    val hiddenZ = ActivateHiddenLayer(IHW,Inputs,HB)
    val outputZ = ActivateHiddenLayer(HOW,hiddenZ,OB)
    val balanced = outputZ.map(x=>(x*5).toInt)
    (balanced(0),balanced(1))
  }


  def Sigmoid(x: Double): Double = 1f / (1f +scala.math.exp(-x))

  def Compress(list: List[Double], height: Int, width: Int): (List[List[Double]], List[Double]) = {
    val (matrixElements, remainingElements) = list.splitAt(height * width)
    val matrix = matrixElements.grouped(width).toList
    (matrix, remainingElements)
  }
  def Split(list: List[Double], index: Int): (List[Double], List[Double]) = list.splitAt(index)
}
