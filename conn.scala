import scala.slick.driver.{ExtendedProfile, MySQLDriver}
import scala.slick.session.{Database}
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import Q.interpolation
import Database.threadLocalSession
import ml._


object conn {
  //scala -cp . conn 1
  implicit val getJudgementResult = GetResult(r => Judgement(r.<<, r.<<, r.<<, r.<<))
  
  def main(args: Array[String])  = {
    
    //assessment id = sup
    val sup = args(0).toInt
    
    //Connect to database
    Database.forURL("jdbc:mysql://nmm.cjibvszqadnr.eu-west-1.rds.amazonaws.com:3306/nmm", driver = "com.mysql.jdbc.Driver", user = "django", password="z5A!a4!c") withSession {
    
    //Run get judgements stored procedure  
    val q = sql"call get_judgements_sp ($sup)".as[Judgement]
    
    //Extract stored proc results into a list of Judgements
    val js = q.list()
    //Extract unique candidate ids
    val uids = extractIds(js, Nil)
    //Calculate true scores
    val i4 = iter(uids,js,4)
    //Calculate interrater reliability
    val ir = irrel(uids,js,100)
    //Standard deviation of current parameter values
    val alpha = raschRel(i4)
    
    
    //Extract unique marker ids
    val markers = uniqueMarker(js, Nil)
    //Calculate marker infit
    val sumRes = markerInfit(i4, js, markers,Nil)
    
    //Update candidate true scores
    def update(ts: TrueScore) = (Q.u + "update nmm.assess_userassessment " +
    		"set truescore = " +? ts.true_score + ", " +
    		"rawscore = " +? ts.obs + ", " +
    		"scoreaccuracy = " +? ts.se + ", " +
    		"comparisons = " +? ts.comps + ", " +
    		"timetaken = " +? ts.timetaken +
    		" where id = "+? ts.id +";").execute
      
    i4.foreach(update)
    
    //Update marker infit
    def updateMarkers(marker: Marker) = (Q.u + "update nmm.assess_marker " +
    		"set truescore = " +? marker.infit + 
    		" where user_id = "+? marker.id +";").execute
    
    sumRes.foreach(updateMarkers)		
    
    //Print out results
    i4.foreach(println)
    sumRes.foreach(println)
    println("Reliability:" + ir)
    println(i4.length.toString + " candidate records updated")
    println(sumRes.length.toString + " marker records updated")
    println("Rasch Reliability: " + alpha)
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
  
 


