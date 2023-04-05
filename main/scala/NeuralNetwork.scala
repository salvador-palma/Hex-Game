package Projeto
import Projeto.BoardState
import Projeto.Main.Board

import scala.annotation.tailrec

case class NeuralNetwork(IHW:List[List[Double]],HB:List[Double],HOW:List[List[Double]],OB:List[Double], Gene:List[Double]){
  def Predict(boardState: BoardState):(Int,Int)= NeuralNetwork.Predict(IHW, HB, HOW, OB, boardState);

  override def toString: String = Gene.toString()
}

object NeuralNetwork{

  def Create(values: List[Double], Input: Int, Hidden: Int, Output: Int): NeuralNetwork = {
    val (inputHiddenWeights, y) = Compress(values,Hidden,Input)
    val (hiddenBias,y2) = Split(y,Hidden)
    val (hiddenOutputWeights,outputBias) = Compress(y2,Output,Hidden)
    NeuralNetwork(inputHiddenWeights, hiddenBias, hiddenOutputWeights, outputBias, values)
  }

  def Crossover(parent: List[Double], mother : List[Double]): NeuralNetwork = {
    val r = Random(80)
    val index = r.nextInt(parent.size)
    def loop(switch:Int, cur : Int):List[Double]={
      cur match{
        case -1 => Nil
        case _ => {
          if(cur>switch){
            parent(cur) :: loop(switch, cur-1)
          }else{
            mother(cur) :: loop(switch, cur-1)
          }
        }
      }
    }
    Create(loop(index._1, parent.size-1),5*5, 15, 5*5)

  }
  def Predict(IHW:List[List[Double]],HB:List[Double],HOW:List[List[Double]],OB:List[Double],boardState: BoardState): (Int,Int) = {
    def ActivateHiddenLayer(weights:List[List[Double]], inputs: List[Double], hiddenB:List[Double]):List[Double]={
      def getNodeValue(w: List[Double], i: List[Double], b: Double): Double = {
        w match {
          case Nil => b
          case x :: y => x * i.head + getNodeValue(y, i.tail, b)
        }
      }
      weights match{
        case Nil=> Nil
        case h::t=>{
          hiddenB match{
            case Nil=>Nil
            case h2::t2=>getNodeValue(h,inputs,h2) :: ActivateHiddenLayer(t,inputs,t2)
          }

        }
      }
    }
    def translateBoard(b: Board):List[Double]={
      b match{
        case Nil => Nil
        case h::t=> h.map(c=> if (c.equals(Cells.Blue)) 1.0 else if (c.equals(Cells.Red)) -1.0 else 0) ::: translateBoard(t)
      }
    }
    val Inputs = translateBoard(boardState.board)
    val hiddenZ = Sigmoid(ActivateHiddenLayer(IHW,Inputs,HB))
    val outputZ = SoftMax(ActivateHiddenLayer(HOW,hiddenZ,OB))
    val indexes = outputZ.zipWithIndex.sortBy(-_._1).map(_._2)
    def TryPlay(list : List[Int]): (Int,Int) = {

      val h::t = list
      val (x,y) = (h%5,h/5)
      if (boardState.valid(x,y)){
        //println(x + "; " + y)
        (x,y)
      }else{
        TryPlay(t)
      }
    }
    TryPlay(indexes)
  }


  def Sigmoid(x: List[Double]): List[Double] = x.map(e=>1.0 / (1.0 +scala.math.exp(-e)))

  def SoftMax(output:List[Double]):List[Double]={
    val sum = (output foldRight 0.0)(_ + _)
    val x = output.map(x=> x/sum)
    x
  }

  def Compress(list: List[Double], height: Int, width: Int): (List[List[Double]], List[Double]) = {
    val (matrixElements, remainingElements) = list.splitAt(height * width)
    val matrix = matrixElements.grouped(width).toList
    (matrix, remainingElements)
  }
  def Split(list: List[Double], index: Int): (List[Double], List[Double]) = list.splitAt(index)
}
