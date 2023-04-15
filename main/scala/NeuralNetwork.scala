package Projeto
import Projeto.BoardState
import Projeto.Cells.Cell
import Projeto.Main.Board

import scala.annotation.tailrec

case class NeuralNetwork(IHW:List[List[Double]],HB:List[Double],HOW:List[List[Double]],OB:List[Double], Gene:List[Double]){
  def Predict(boardState: BoardState, player : Cell):(Int,Int)= NeuralNetwork.Predict(IHW, HB, HOW, OB, boardState, player);

  override def toString: String = Gene.toString()
}

object NeuralNetwork{

  def Create(values: List[Double], Input: Int, Hidden: Int, Output: Int): NeuralNetwork = {
    val (inputHiddenWeights, y) = Compress(values,Hidden,Input)
    val (hiddenBias,y2) = Split(y,Hidden)
    val (hiddenOutputWeights,outputBias) = Compress(y2,Output,Hidden)
    NeuralNetwork(inputHiddenWeights, hiddenBias, hiddenOutputWeights, outputBias, values)
  }

  def Crossover(parent: List[Double], mother : List[Double], randomState: RandomState): (NeuralNetwork,RandomState) = {
    val index = randomState.nextInt(parent.size)
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

    val d = loop(index._1, parent.size-1)
    val n = index._2.nextInt(10)

    if(n._1 <= 2){

      val index1 = n._2.nextInt(d.size)
      val index2 = index1._2.nextInt(d.size)
      val (p1,p2) = if(index1._1<index2._1)(index1._1,index2._1)else(index2._1,index1._1)
      val (start,rest)  = d.splitAt(p1)
      val (mid,end)  = rest.splitAt(p2)
      val shuffled  = shuffleList(mid, index2._2)
      val c = start:::shuffled._1:::end
      (Create(c,5*5,10,5*5),shuffled._2)
    }

    (Create(d,5*5, 10, 5*5),n._2)
  }
  def Predict(IHW:List[List[Double]],HB:List[Double],HOW:List[List[Double]],OB:List[Double],boardState: BoardState, player:Cell): (Int,Int) = {
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
    val adjustedBoard = if(player.equals(Cells.Blue)){ TurnBoard(boardState.board) }else{boardState.board}
    val Inputs = translateBoard(adjustedBoard)
    val hiddenZ = ReLu(ActivateHiddenLayer(IHW,Inputs,HB))
    val outputZ = SoftMax(ActivateHiddenLayer(HOW,hiddenZ,OB))
    val indexes = outputZ.zipWithIndex.sortBy(-_._1).map(_._2)
    def TryPlay(list : List[Int]): (Int,Int) = {

      val h::t = list
      val (x,y) = (h%5,h/5)
      if (boardState.valid(x,y)){
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

  def ReLu(output:List[Double]):List[Double]= output.map(x=> if(x>=0) x else 0)

  def TurnBoard(list:Board):Board = {
    val newList = list.map(r => r.map(e=> if(e.equals(Cells.Red)) Cells.Blue else if(e==Cells.Blue) Cells.Red else Cells.Empty))
    ReflectTranspose(newList)
  }
  def ReflectTranspose(lst:Board):Board = {
    lst.transpose.map(_.reverse).reverse
  }

  def Compress(list: List[Double], height: Int, width: Int): (List[List[Double]], List[Double]) = {
    val (matrixElements, remainingElements) = list.splitAt(height * width)
    val matrix = matrixElements.grouped(width).toList
    (matrix, remainingElements)
  }
  def Split(list: List[Double], index: Int): (List[Double], List[Double]) = list.splitAt(index)


  def shuffleList(lst: List[Double], randomState: RandomState): (List[Double],RandomState) = {
    def shuffle(lst: List[Double], acc: List[Double],r:RandomState): (List[Double],RandomState) = {
      lst match {
        case Nil => (acc,r)
        case _ =>
          val rand = r.nextInt(lst.length)
          val (s, e) = lst.splitAt(rand._1)
          shuffle(s ::: e.tail, acc :+ e.head,rand._2)
      }
    }
    shuffle(lst, Nil,randomState)
  }
}
