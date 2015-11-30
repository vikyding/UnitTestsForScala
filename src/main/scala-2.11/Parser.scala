/**
  * Created by mengchending on 11/24/15.
  */

import scala.util._

object Parser {



  def SplitAndFilter(tweet:String)=  {

    tweet.split(" ").filterNot(_.startsWith("RT")).filterNot(_.startsWith("@"))
  }


}
