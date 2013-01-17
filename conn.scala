import scala.slick.driver.{ExtendedProfile, MySQLDriver}
import scala.slick.session.{Database}
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import Q.interpolation
import Database.threadLocalSession
import ml._

object conn {
  //scala -cp . conn 1
  implicit val getJudgementResult = GetResult(r => Judgement(r.<<, r.<<, r.<<))
  
  def main(args: Array[String])  = {
    
    //assessment id
    val sup = args(0).toInt
    
    Database.forURL("jdbc:mysql://nmm.cjibvszqadnr.eu-west-1.rds.amazonaws.com:3306/questaurus", driver = "com.mysql.jdbc.Driver", user = "django", password="z5A!a4!c") withSession {
    
    val q = sql"call get_judgements_sp ($sup)".as[Judgement]
       
    val js = q.list()
    var uids = extractIds(js, Nil)
    var i4 = iter(uids,js,4)
    
    def update(ts: TrueScore) = (Q.u + "update questaurus.assess_userassessment " +
    		"set truescore = " +? ts.true_score + ", " +
    		"rawscore = " +? ts.obs + ", " +
    		"scoreaccuracy = " +? ts.se + ", " +
    		"comparisons = " +? ts.comps + ", " +
    		"timetaken = " +? ts.timetaken +
    		" where assessment_id = "+? sup + " and user_id = "+? ts.id +";").execute    		
    i4.foreach(update)
   
    // Update
    i4.foreach(println)
    println(i4.length.toString + " records updated")
    
    val assignment:Assessment = new Assessment(id=sup)
    def update_ass(ass: Assessment) = (Q.u + "update questaurus.assess_assessment " +
        "set needsupdating = 0 where id = " +? ass.id).execute
        
    update_ass(assignment)
    //return(true)
    
    
    }
    
    //val js2 = List(Judgement(1,2),Judgement(1,2),Judgement(1,2),Judgement(1,2),Judgement(2,1),Judgement(2,1))
    //var uids2 = extractIds(js2, Nil)
    //var i42 = iter(uids2, js2, 4)
    //i42.foreach(println)
    
  
  }
  
}
  
 


