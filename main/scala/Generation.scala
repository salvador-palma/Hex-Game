package Projeto
case class Generation(population : List[NeuralNetwork]){
  def Select(): List[(NeuralNetwork, Int)] = Generation.Selection(this)
  def Breed(selection:List[NeuralNetwork]):Generation = Generation.Breed(selection, 64, Random(20))
  def Strongest()=Generation.BattleRoyale(population)
  override def toString: String = "Generation size: " + population.size
}

object Generation{
  def Start(n:Int, range:(Int,Int), myRandom: RandomState): Generation = {
    def loop(m:Int, randomState: RandomState):List[NeuralNetwork]={
      m match{
        case 0=> Nil
        case _=> {
          val b = randomState.doubleList(790,range)
          NeuralNetwork.Create(b._1, 5*5, 15, 5*5) :: loop(m-1, b._2)
        }
      }
    }
    Generation(loop(n,myRandom))
  }

  def Selection(generation: Generation):List[(NeuralNetwork,Int)]={
    def SelectWinner(contestants : List[List[NeuralNetwork]]):List[(NeuralNetwork,Int)]={
      contestants match{
        case Nil => Nil
        case h::t =>{
          Simulate(BoardState(BoardState.defineBoard(5), UnionFind(UnionFind.init(5))), Array(h(0),h(1))) match{
            case (Some(0),n)=>(h(0),n)::SelectWinner(t)
            case (Some(1),n)=>(h(1),n)::SelectWinner(t)
          }
        }
        case _ => Nil
      }
    }
    SelectWinner(generation.population.grouped(2).toList)
  }

  def Simulate(boardState: BoardState, Players: Array[NeuralNetwork],acc:Int=0) : (Option[Int],Int) = {
    val(x,y) = Players(0).Predict(boardState)
    val newBoardState = boardState.playGameState((y,x), Cells.Blue)
    newBoardState.hasContinuousLine match {
      case Some("P1")=>(Some(0),acc)
      case Some("P2")=>(Some(1),acc)
      case _=>{
        val (w, z) = Players(1).Predict(newBoardState)
        val newerBoardState = newBoardState.playGameState((z, w), Cells.Red)
        newerBoardState.hasContinuousLine match {
          case Some("P1") => (Some(0),acc)
          case Some("P2") => (Some(1),acc)
          case _ => Simulate(newerBoardState, Players, acc+1)
        }
      }
    }
  }

  def Breed(selection:List[NeuralNetwork], n : Int, randomState: RandomState):Generation={
    if(selection.size < n){
      val i = randomState.nextInt(selection.size)
      val j = i._2.nextInt(selection.size)
      Breed(NeuralNetwork.Crossover(selection(i._1).Gene, selection(j._1).Gene) :: selection,n,j._2)
    }else{
      Generation(selection)
    }
  }

  def BattleRoyale(list : List[NeuralNetwork]):NeuralNetwork={
    def SelectWinner(contestants: List[List[NeuralNetwork]]): List[NeuralNetwork] = {
      contestants match {
        case Nil => Nil
        case h :: t => {
          Simulate(BoardState(BoardState.defineBoard(5), UnionFind(UnionFind.init(5))), Array(h(0), h(1))) match {
            case Some(0) => h(0) :: SelectWinner(t)
            case Some(1) => h(1) :: SelectWinner(t)
          }
        }
        case _ => Nil
      }
    }

    val l = SelectWinner(list.grouped(2).toList)
    if(l.size==1){
      l(0)
    }else{
      BattleRoyale(l)
    }
  }

}