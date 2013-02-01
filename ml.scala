import org.apache.commons.math3.stat.correlation._
import org.apache.commons.math3.stat.ranking._
import org.apache.commons.math3.random._
import scala.util.Random.shuffle

object ml {
  
  case class Assessment(id: Int)
  case class Judgement(userchosen: Int, usernotchosen: Int, timetaken: Double, marker:Int)
  case class Marker(id:Int, infit:Double)
  case class TrueScore(id: Int, true_score: Double, se:Double, obs:Int, comps:Int, timetaken:Double)
  case class Contest(opponent_ability:Double, result:Int)
  
  def extractIds(ids: List[Judgement], uids: List[TrueScore]): List[TrueScore] = ids match {
     case Nil => uids.distinct
     case _ =>
       extractIds(ids.tail, new TrueScore(ids.head.userchosen,0.0,0.0,0,0,0.0) :: new TrueScore(ids.head.usernotchosen,0.0,0.0,0,0,0.0) :: uids)
  }
  
  def uniqueMarker(ids: List[Judgement], markers: List[Marker]): List[Marker] = ids match {
    case Nil => markers.distinct
    case _ =>
      uniqueMarker(ids.tail, new Marker(ids.head.marker,0.0) :: markers)  
  } 
  
  def markerInfit(ts: List[TrueScore],dat: List[Judgement], markers: List[Marker], updated: List[Marker]):List[Marker] = markers match {
    case Nil => updated
    case _ =>
    //filter judgements by marker 
    val myJs = dat filter (x => x.marker==markers.head.id)
    val probs: List[Double] = myJs.map(x => expon(ts.find{_.id == x.userchosen}.get.true_score,ts.find{_.id == x.usernotchosen}.get.true_score))
    val numer: Double = probs.foldLeft(0.0d)((total,p)=>{
      val res = 1 - p
      val sq = math.pow(res,2)
      val info = p * (1 - p)
      val zsq = sq / info
      val num = info * zsq
      total + num 
    })
    val denom: Double = probs.foldLeft(0.0d)((total,item)=>{
      val info = item * (1 - item)
      total + info 
    })
    val wms = numer / denom
    
    markerInfit(ts, dat, markers.tail, new Marker(markers.head.id,wms) :: updated)
    
  }
                                                  
  def expon(va: Double, vi: Double):Double =
    math.exp(va - vi) / (1 + math.exp(va - vi))   
    
  def toys(id: Int, abi: Double, dat: List[Judgement]): List[(Double,Int)] =
    dat map(x => (abi,if(id==x.userchosen) 1 else 0))    
 
  def byId(all_ids: List[TrueScore],ids: List[TrueScore], dat: List[Judgement], updated: List[TrueScore]): List[TrueScore] = ids match {
    case Nil => updated
    case _ =>
      //println("Estimating person id: "+ids.head.id)
      val myGames = dat filter (x => x.userchosen == ids.head.id || x.usernotchosen == ids.head.id)
      val timesTaken = myGames map (x => x.timetaken)
      val timetaken = timesTaken.sum
      val opps = myGames map (x => if (x.userchosen==ids.head.id) x.usernotchosen else x.userchosen)
      //println("Opponents: "+opps)
      //println("all_ids: "+all_ids)
      val opp_ts: List[Double] = opps map (x => all_ids.find{_.id == x}.get.true_score)
      //println("Opponents Ts: "+opp_ts)
      
      val myScores = toys(ids.head.id, ids.head.true_score, myGames)
      //println(myScores)
      val obs_score = myScores.filter(xy => xy._2 == 1).length
      //println("obs: " + obs_score)
      //println(ids.head.true_score + "vs ?")
      
      //Calculate expected score
      val exp_scores = opp_ts map(x => expon(ids.head.true_score ,x))
      val exp_score = exp_scores.sum
      
      //println("exp:" + exp_score)
      //println("current ts estimate:" + ids.head.true_score)
      val info = myScores.map(xy => (1 - expon(ids.head.true_score , xy._1))* (expon(ids.head.true_score , xy._1))).sum
      val update = ids.head.true_score + ((obs_score - exp_score)/info)
      //println("updated:" + update)
      byId(all_ids, ids.tail, dat, new TrueScore(ids.head.id,update,1/math.sqrt(info),obs_score,myGames.length,timetaken)::updated)
  }                                               
                                                  
  def iter(uids: List[TrueScore],dat: List[Judgement],iters: Int):List[TrueScore] = iters match {
    case 0 => uids
    case _ => iter(byId(uids, uids, dat, Nil), dat, iters-1)
  }
  
  def interrater(uids: List[TrueScore],js: List[Judgement]):Double ={
        val jn = js.length
        //More than one marker, calculate inter-rater reliabilty
	    if(jn>1){		
	      
	      val jsr = shuffle(js)
	      val js1 = jsr.slice(0, (jn/2))
	      val js2 = jsr.slice((jn/2)+1, jn-1)
	    
	      val r1 = iter(uids, js1, 4)
	      val true1 = r1.map(x => x.true_score).toArray
	
	      val r2 = iter(uids, js2, 4)
	      val true2 = r2.map(x => x.true_score).toArray
	      
	      val ranking = new NaturalRanking(NaNStrategy.MAXIMAL, TiesStrategy.MAXIMUM);
	      
	     new PearsonsCorrelation().correlation(ranking.rank(true1), ranking.rank(true2))
	      
	    } else {
	      -1.0
	    }
    }
  
def repeatWhile(uids: List[TrueScore],js: List[Judgement],nTries: Int, rel: List[Double]): List[Double] = nTries match {
    case 0 => rel
    case _ => repeatWhile(uids,js,nTries-1,interrater(uids,js)::rel)
}
  
def mean[T](item:Traversable[T])(implicit n:Numeric[T]) = {
  n.toDouble(item.sum) / item.size.toDouble
}
  
def variance[T](items:Traversable[T])(implicit n:Numeric[T]) : Double = {
  val itemMean = mean(items)
  val count = items.size
  val sumOfSquares = items.foldLeft(0.0d)((total,item)=>{
  val itemDbl = n.toDouble(item)
  val square = math.pow(itemDbl - itemMean,2)
    total + square
  })
  sumOfSquares / count.toDouble
}

def raschRel(ts: List[TrueScore]): Double = {
  val tse = ts.map(x => x.se)
  val rms = tse.foldLeft(0.00d)((total, item)=> {
    val square = math.pow(item, 2)
    total + square
  })
  val rmse = math.pow((rms / ts.length),0.5)
  val tss =ts.map(x => x.true_score)
  val G = stddev(tss) / rmse
  val alpha = (math.pow(G,2) - 1) / math.pow(G,2) 
  alpha
}
  
def stddev[T](items:Traversable[T])(implicit n:Numeric[T]) : Double = {
  math.sqrt(variance(items))
}
  
def irrel(uids: List[TrueScore],js: List[Judgement],nTries: Int): List[Double] = {
     val rels = repeatWhile(uids,js,nTries, Nil)
     //(0.0 /: rels){_ + _} / rels.length
     List(mean(rels),stddev(rels))
  }   
}