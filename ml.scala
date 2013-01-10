object ml {
  
  case class Judgement(userchosen: Int, usernotchosen: Int)
  case class TrueScore(id: Int, true_score: Double)
  
  def extractIds(ids: List[Judgement], uids: List[TrueScore]): List[TrueScore] = ids match {
     case Nil => uids.distinct
     case _ =>
       extractIds(ids.tail, new TrueScore(ids.head.userchosen,0.0) :: new TrueScore(ids.head.usernotchosen,0.0) :: uids)
  }                                               
                                                  
  def expon(va: Double, vi: Double):Double =
    math.exp(va - vi) / (1 + math.exp(va - vi))   
    
  def toys(id: Int, abi: Double, dat: List[Judgement]): List[(Double,Int)] =
    dat map(x => (abi,if(id==x.userchosen) 1 else 0))    
 
  def byId(ids: List[TrueScore], dat: List[Judgement], updated: List[TrueScore]): List[TrueScore] = ids match {
    case Nil => updated
    case _ =>
      var myGames = dat filter (x => x.userchosen == ids.head.id || x.usernotchosen == ids.head.id)
      var myScores = toys(ids.head.id, ids.head.true_score, myGames)
      var obs_score = myScores.filter(xy => xy._2 == 1).length
      var exp_score = myScores.map(xy => expon(ids.head.true_score ,xy._1)).sum
      var info = myScores.map(xy => (1 - expon(ids.head.true_score , xy._1))* (expon(ids.head.true_score , xy._1))).sum
      var update = ids.head.true_score + ((obs_score - exp_score)/info)
      byId(ids.tail, dat, new TrueScore(ids.head.id,update)::updated)
  }                                               
                                                  
  def iter(uids: List[TrueScore],dat: List[Judgement],iters: Int):List[TrueScore] = iters match {
    case 0 => uids
    case _ => iter(byId(uids, dat, Nil), dat, iters-1)
  }                                               
  

   
}