import scala.slick.driver.{ExtendedProfile, MySQLDriver}
import scala.slick.session.{Database}
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import Q.interpolation
import Database.threadLocalSession
import org.apache.commons.math3.stat.correlation._
import org.apache.commons.math3.stat.ranking._
import ml._

object conn {
  //scala -cp . conn 1
  implicit val getJudgementResult = GetResult(r => Judgement(r.<<, r.<<, r.<<, r.<<))
  
  def main(args: Array[String])  = {
    
    //assessment id
    val sup = args(0).toInt
    
    Database.forURL("jdbc:mysql://nmm.cjibvszqadnr.eu-west-1.rds.amazonaws.com:3306/nmm", driver = "com.mysql.jdbc.Driver", user = "django", password="z5A!a4!c") withSession {
    
    val q = sql"call get_judgements_sp ($sup)".as[Judgement]
       
    val js = q.list()
    var uids = extractIds(js, Nil)
    var i4 = iter(uids,js,4)
    
    // Update
    i4.foreach(println)
    println("all")
    
    def update(ts: TrueScore) = (Q.u + "update nmm.assess_userassessment " +
    		"set truescore = " +? ts.true_score + ", " +
    		"rawscore = " +? ts.obs + ", " +
    		"scoreaccuracy = " +? ts.se + ", " +
    		"comparisons = " +? ts.comps + ", " +
    		"timetaken = " +? ts.timetaken +
    		" where id = "+? ts.id +";").execute
  
    //For reliability filter markers
    val js1 = q.list().filter(x=>evenOrOdd(x.marker)==0)
    val js2 = q.list().filter(x=>evenOrOdd(x.marker)==1)
    
    if(js1.length>0 && js2.length>0){
      val r1 = iter(uids, js1, 4)
      println("evens")
      r1.foreach(println)  
      val true1 = r1.map(x => x.true_score).toArray

      //For reliability filter markers

      val r2 = iter(uids, js2, 4)
      println("odds")
      r2.foreach(println)
      val true2 = r2.map(x => x.true_score).toArray
      println(true1.deep.mkString("\n"))
      println(true2.deep.mkString("\n"))
    
      val ranking = new NaturalRanking(NaNStrategy.MAXIMAL, TiesStrategy.MAXIMUM);
      println("correlation")
      println(new PearsonsCorrelation().correlation(ranking.rank(true1), ranking.rank(true2)))
      println("end of correlation")
      
    } else {
      println("No reliability available")
    }
    		 
    /*
    def update(ts: TrueScore) = (Q.u + "call nmm.update_true_scores " +
    		"(" +? ts.true_score + 
    		"," +? ts.obs + 
    		"," +? ts.se + 
    		"," +? ts.comps + 
    		"," +? ts.timetaken +
    		","+? ts.id +");").execute
    		 
    */
    		
    //call update_true_scores (5.5555, 101, 0.001, 102, 33.3, 107)
    
    i4.foreach(update)
    println(i4.length.toString + " records updated")
    
    //Needs this for questaurus
    //val assignment:Assessment = new Assessment(id=sup)
    //def update_ass(ass: Assessment) = (Q.u + "update questaurus.assess_assessment " +
    //    "set needsupdating = 0 where id = " +? ass.id).execute
    //update_ass(assignment)    
    
    }
    
    //val js2 = List(Judgement(1,2),Judgement(1,2),Judgement(1,2),Judgement(1,2),Judgement(2,1),Judgement(2,1))
    //var uids2 = extractIds(js2, Nil)
    //var i42 = iter(uids2, js2, 4)
    //i42.foreach(println)
    
  
  }
  
}
  
 


