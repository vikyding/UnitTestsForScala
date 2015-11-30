


import org.scalatest.{FlatSpec, Matchers, Inside}

/**
  * Created by mengchending on 11/24/15.
  */
class UnitTest extends FlatSpec with Inside with Matchers{

  "OR" should "return ((?iu)[:=]|[;]) for "+"(?iu)[:=]"+" , "+"[;]" in {
    val x = "(?iu)[:=]"
    val y = "[;]"
    Tokenize.Tokenizer.OR(x,y)
  }

  "allowEntities" should  "return ((<|&lt;)+/?3+) for "+"(<+/?3+)" in {
    val x="(<+/?3+)"
    Tokenize.Tokenizer.allowEntities(x)
  }

  it should "return ((<|&lt;)*[-=]*(>|&gt;)+|(<|&lt;)+[-=]*(>|&gt;)*) for "+"(<*[-=]*>+|<+[-=]*>*)" in {
    val x="(<*[-=]*>+|<+[-=]*>*)"
    Tokenize.Tokenizer.allowEntities(x)
  }

  "splitEdgePunct" should "return ;([\" hello \"]); for"+" ;([\"+'\"'+\"hello\"+'\"'+\"]);" in{
    val x= ";(["+'"'+"hello"+'"'+"]);"
    Tokenize.Tokenizer.splitEdgePunct(x)
  }



  //"Name" should "parse Tom Brady" in {
  //  Name.parse("Tom Brady") should matchPattern { case Success(h) => }
  //}


}
