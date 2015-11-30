import scala.util.matching.Regex

/**
  * Created by mengchending on 11/29/15.
  */
object Tokenize {

  object PorterStemmer{
    // word to be stemmed.
    var b = ""
    // Character sets to test membership for
    val vowels = Set('a', 'e', 'i', 'o', 'u')
    val wxy = Set('w', 'x', 'y')
    // Just recode the existing stuff, then go through and refactor with some intelligence.
    def cons(i: Int): Boolean = {
      val ch = b(i)

      if (vowels(ch))
        false
      else {
        if (ch == 'y')
          if (i == 0) true else !cons(i - 1)
        else
          true
      }
    }

    // Add via letter or entire word
    def add(ch: Char) = b += ch
    def add(word: String) = b = word

    /**
      *  m() measures the number of consonant sequences between 0 and j. if c is
      * a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
      * presence,
      *
      * <c><v>       gives 0
      * <c>vc<v>     gives 1
      * <c>vcvc<v>   gives 2
      * <c>vcvcvc<v> gives 3
      * ....
      *
      * I think this can be recoded far more neatly.
      */
    def calcM(s: String): Int = {
      val l = s.length
      var count = 0
      var currentConst = false

      for (c <- 0 to l - 1) {
        if (cons(c)) {
          if (!currentConst && c != 0) {
            count += 1
          }
          currentConst = true
        } else {
          currentConst = false
        }
      }

      count
    }

    // removing the suffix 's', does a vowel exist?'
    def vowelInStem(s: String): Boolean = {
      for (i <- 0 to b.length - 1 - s.length) {
        if (!cons(i)) {
          return true
        }
      }
      return false
    }

    /* doublec(j) is true <=> j,(j-1) contain a double consonant. */
    def doublec(): Boolean = {
      val l = b.length - 1

      if (l < 1)
        false
      else {
        if (b(l) != b(l - 1)) false else cons(l)
      }
    }

    /**
      * cvc(i) is true <=> i-2,i-1,i has the form consonant - vowel - consonant
      * and also if the second c is not w,x or y. this is used when trying to
      * restore an e at the end of a short word. e.g.
      *
      * cav(e), lov(e), hop(e), crim(e), but
      * snow, box, tray.
      *
      */
    def cvc(s: String): Boolean = {
      val i = b.length - 1 - s.length
      if (i < 2 || !cons(i) || cons(i - 1) || !cons(i - 2))
        false
      else
        !wxy(b(i))
    }

    // returns true if it did the change.
    def replacer(orig: String, replace: String, checker: Int => Boolean): Boolean = {
      val l = b.length
      val origLength = orig.length

      if (b.endsWith(orig)) {
        val n = b.substring(0, l - origLength)
        val m = calcM(n)

        if (checker(m))
          b = n + replace

        true
      } else {
        false
      }
    }

    // process the list of tuples to find which prefix matches the case.
    // checker is the conditional checker for m.
    def processSubList(l: List[(String, String)], checker: Int => Boolean): Boolean = {
      val iter = l.iterator
      var done = false

      while (!done && iter.hasNext) {
        val v = iter.next
        done = replacer(v._1, v._2, checker)
      }

      done
    }

    def step1() {

      val l = b.length

      var m = calcM(b)

      // step 1a
      val esses = List(("sses", "ss"), ("ies", "i"), ("ss", "ss"), ("s", ""))
      processSubList(esses, _ >= 0)

      // step 1b
      if (!(replacer("eed", "ee", _ > 0))) {

        if ((vowelInStem("ed") && replacer("ed", "", _ >= 0)) || (vowelInStem("ing") && replacer("ing", "", _ >= 0))) {

          val atebleize = List(("at", "ate"), ("bl", "ble"), ("iz", "ize"))

          if (!processSubList(atebleize, _ >= 0)) {
            // if this isn't done, then it gets more confusing.

            m = calcM(b)
            val last = b(b.length - 1)
            if (doublec() && !"lsz".contains(last)) {
              b = b.substring(0, b.length - 1)
            } else if (m == 1 && cvc("")) {
              b = b + "e"
            }
          }
        }
      }

      // step 1c

      (vowelInStem("y") && replacer("y", "i", _ >= 0))

    }

    def step2 = {

      val suffixes = List(("ational", "ate"), ("tional", "tion"), ("enci", "ence"), ("anci", "ance"), ("izer", "ize"), ("bli", "ble"), ("alli", "al"),
        ("entli", "ent"), ("eli", "e"), ("ousli", "ous"), ("ization", "ize"), ("ation", "ate"), ("ator", "ate"), ("alism", "al"),
        ("iveness", "ive"), ("fulness", "ful"), ("ousness", "ous"), ("aliti", "al"), ("iviti", "ive"), ("biliti", "ble"), ("logi", "log"))

      processSubList(suffixes, _ > 0)
    }

    def step3 = {
      val suffixes = List(("icate", "ic"), ("ative", ""), ("alize", "al"), ("iciti", "ic"), ("ical", "ic"), ("ful", ""), ("ness", ""))
      processSubList(suffixes, _ > 0)
    }

    def step4 = {

      // first part.
      val suffixes = List(("al", ""), ("ance", ""), ("ence", ""), ("er", ""), ("ic", ""), ("able", ""), ("ible", ""), ("ant", ""), ("ement", ""),
        ("ment", ""), ("ent", ""))

      var res = processSubList(suffixes, _ > 1)

      // special part.
      if (!res) {
        if (b.length > 4) {
          if (b(b.length - 4) == 's' || b(b.length - 4) == 't') {
            res = replacer("ion", "", _ > 1)
          }
        }
      }

      // third part.
      if (!res) {
        val suffixes = List(("ou", ""), ("ism", ""), ("ate", ""), ("iti", ""), ("ous", ""), ("ive", ""), ("ize", ""))
        res = processSubList(suffixes, _ > 1)
      }

    }

    def step5a = {
      replacer("e", "", _ > 1)
      if (!cvc("e"))
        replacer("e", "", _ == 1)
    }

    def step5b = {
      val m = calcM(b)
      if (m > 1 && doublec() && b.endsWith("l"))
        b = b.substring(0, b.length - 1)
    }

    def apply(token: String) = {
      add(token)
      if (b.length > 2) {
        step1
        step2
        step3
        step4
        step5a
        step5b
      }
      b
    }

  }



  object Tokenizer{
    val Contractions = """(?i)(\w+)(n't|'ve|'ll|'d|'re|'s|'m)$""".r
    val Whitespace = """\s+""".r

    val punctChars = """['“\".?!,:;]"""
    val punctSeq = punctChars + """+"""
    val entity = """&(amp|lt|gt|quot);"""

    // URLs

    // David: I give the Larry David eye to this whole URL regex
    // (http://www.youtube.com/watch?v=2SmoBvg-etU) There are
    // TODO potentially better options, see:
    // http://daringfireball.net/2010/07/improved_regex_for_matching_urls
    // http://mathiasbynens.be/demo/url-regex

    val urlStart1 = """(https?://|www\.)"""
    val commonTLDs = """(com|co\.uk|org|net|info|ca|ly|mp|edu|gov)"""
    val urlStart2 = """[A-Za-z0-9\.-]+?\.""" + commonTLDs + """(?=[/ \W])"""
    val urlBody = """[^ \t\r\n<>]*?"""
    val urlExtraCrapBeforeEnd = "(" + punctChars + "|" + entity + ")+?"
    val urlEnd = """(\.\.+|[<>]|\s|$)"""
    val url = """\b(""" + urlStart1 + "|" + urlStart2 + ")" + urlBody + "(?=(" + urlExtraCrapBeforeEnd + ")?" + urlEnd + ")"

    // Numeric
    val timeLike = """\d+:\d+"""
    val numNum = """\d+\.\d+"""
    val numberWithCommas = """(\d+,)+?\d{3}""" + """(?=([^,]|$))"""

    // Abbreviations
    val boundaryNotDot = """($|\s|[“\"?!,:;]|""" + entity + ")"
    val aa1 = """([A-Za-z]\.){2,}(?=""" + boundaryNotDot + ")"
    val aa2 = """[^A-Za-z]([A-Za-z]\.){1,}[A-Za-z](?=""" + boundaryNotDot + ")"
    val standardAbbreviations = """\b([Mm]r|[Mm]rs|[Mm]s|[Dd]r|[Ss]r|[Jj]r|[Rr]ep|[Ss]en|[Ss]t)\."""
    val arbitraryAbbrev = "(" + aa1 + "|" + aa2 + "|" + standardAbbreviations + ")"

    val separators = "(--+|―)"
    val decorations = """[♫]+"""
    val thingsThatSplitWords = """[^\s\.,]"""
    val embeddedApostrophe = thingsThatSplitWords + """+'""" + thingsThatSplitWords + """+"""

    // Emoticons
    val normalEyes = "(?iu)[:=]"
    val wink = "[;]"
    val noseArea = "(|o|O|-|[^a-zA-Z0-9 ])"
    val happyMouths = """[D\)\]]+"""
    val sadMouths = """[\(\[]+"""
    val tongue = "[pP]"
    val otherMouths = """[doO/\\]+""" // remove forward slash if http://'s aren't cleaned

    // mouth repetition examples:
    // @aliciakeys Put it in a love song :-))
    // @hellocalyclops =))=))=)) Oh well

    def OR(parts: String*) = {
      "(" + parts.toList.mkString("|") + ")"
    }

    val emoticon = OR(
      // Standard version :) :( :] :D :P
      OR(normalEyes, wink) + noseArea + OR(tongue, otherMouths, sadMouths, happyMouths),

      // reversed version (: D: use positive lookbehind to remove "(word):"
      // because eyes on the right side is more ambiguous with the standard usage of : ;
      """(?<=( |^))""" + OR(sadMouths, happyMouths, otherMouths) + noseArea + OR(normalEyes, wink) // TODO japanese-style emoticons
      // TODO should try a big precompiled lexicon from Wikipedia, Dan Ramage told me (BTO) he does this
    )

    def allowEntities(pat: String) = {
      // so we can write patterns with < and > and let them match escaped html too
      pat.replace("<", "(<|&lt;)").replace(">", "(>|&gt;)")

    }

    val Hearts = allowEntities("""(<+/?3+)""")


    val Arrows = allowEntities("""(<*[-=]*>+|<+[-=]*>*)""")

    // BTO 2011-06: restored Hashtag, AtMention protection (dropped in original scala port) because it fixes
    // "hello (#hashtag)" ==> "hello (#hashtag )" WRONG
    // "hello (#hashtag)" ==> "hello ( #hashtag )" RIGHT
    // "hello (@person)" ==> "hello (@person )" WRONG
    // "hello (@person)" ==> "hello ( @person )" RIGHT
    // ... Some sort of weird interaction with edgepunct I guess, because edgepunct
    // has poor content-symbol detection.

    val Hashtag = """#[a-zA-Z0-9_]+""" // also gets #1 #40 which probably aren't hashtags .. but good as tokens

    val AtMention = """@[a-zA-Z0-9_]+\s*:?"""
    val Singnature ="""RT"""

    // I was worried this would conflict with at-mentions
    // but seems ok in sample of 5800: 7 changes all email fixes
    // http://www.regular-expressions.info/email.html
    val Bound = """(\W|^|$)"""
    val Email = "(?<=" + Bound + """)[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}(?=""" + Bound + ")"

    // We will be tokenizing using these regexps as delimiters
    // Additionally, these things are "protected", meaning they shouldn't be further split themselves.
    val Protected = new Regex(
      OR(

        entity,
        timeLike,
        numNum,
        numberWithCommas,
        punctSeq,
        arbitraryAbbrev,
        separators,
        decorations,
        embeddedApostrophe
      ))

    val Removed = new Regex(
      OR(
        Hearts,
        Arrows,
        emoticon,
        url,
        Email,
        Hashtag,
        AtMention,
        Singnature
      )

    )

    // Edge punctuation
    // Want: 'foo' => ' foo '
    // While also: don't => don't
    // the first is considered "edge punctuation".
    // the second is word-internal punctuation -- don't want to mess with it.
    // BTO (2011-06): the edgepunct system seems to be the #1 source of problems these days.
    // I remember it causing lots of trouble in the past as well. Would be good to revisit or eliminate.

    // Note the 'smart quotes' (http://en.wikipedia.org/wiki/Smart_quotes)

    val edgePunctChars = """'"“”‘’«»{}\(\)\[\]\*"""
    val edgePunct = "[" + edgePunctChars + "]"
    val notEdgePunct = "[a-zA-Z0-9]" // content characters
    val offEdge = """(^|$|:|;|\s)""" // colon here gets "(hello):" ==> "( hello ):"
    val EdgePunctLeft = new Regex(offEdge + "(" + edgePunct + "+)(" + notEdgePunct + ")")
    val EdgePunctRight = new Regex("(" + notEdgePunct + ")(" + edgePunct + "+)" + offEdge)

    def splitEdgePunct(input: String) = {
      var s = input

      s = EdgePunctLeft.replaceAllIn(s, "$1$2 $3")

      s = EdgePunctRight.replaceAllIn(s, "$1 $2$3")
      s
    }


    // The main work of tokenizing a tweet.
    def simpleTokenize(text: String) = {

      // Do the no-brainers first
      val splitPunctText = splitEdgePunct(text)

      val textLength = splitPunctText.length

      // Find the matches for subsequences that should be protected,
      // e.g. URLs, 1.0, U.N.K.L.E., 12:53
      val textafterremoved=Removed.replaceAllIn(splitPunctText," ")

      val matches = Protected.findAllIn(textafterremoved).matchData.toList


      // The spans of the "bads" should not be split.
      val badSpans = matches map (mat => Tuple2(mat.start, mat.end))

      // Create a list of indices to create the "goods", which can be
      // split. We are taking "bad" spans like
      // List((2,5), (8,10))
      // to create
      /// List(0, 2, 5, 8, 10, 12)
      // where, e.g., "12" here would be the textLength
      val indices = (0 :: badSpans.foldRight(List[Int]())((x, y) => x._1 :: x._2 :: y)) ::: List(textLength)
      // Group the indices and map them to their respective portion of the string
      val goods = indices.grouped(2).map { x => textafterremoved.slice(x(0), x(1)) }.toList


      //The 'good' strings are safe to be further tokenized by whitespace
      val splitGoods = goods map { str => str.trim.split(" ").toList }

      //Storing as List[List[String]] to make zip easier later on
      val bads = badSpans map { case (start, end) => List(textafterremoved.slice(start, end)) }

      // Reinterpolate the 'good' and 'bad' Lists, ensuring that
      // additonal tokens from last good item get included
      val zippedStr =
        (if (splitGoods.length == bads.length)
          splitGoods.zip(bads) map { pair => pair._1 ++ pair._2 }
        else
          (splitGoods.zip(bads) map { pair => pair._1 ++ pair._2 }) ::: List(splitGoods.last)).flatten

      // Split based on special patterns (like contractions) and check all tokens are non empty
      zippedStr.map(splitToken(_)).flatten.filter(_.length > 0)
    }

    // "foo bar" => "foo bar"
    def squeezeWhitespace(input: String) = Whitespace.replaceAllIn(input, " ").trim

    // Final pass tokenization based on special patterns
    def splitToken(token: String) = {
      token match {
        // BTO: our POS tagger wants "ur" and "you're" to both be one token.
        // Uncomment to get "you 're"
        // case Contractions(stem, contr) => List(stem.trim, contr.trim)
        case token => List(token.trim)
      }
    }
    // Apply method allows it to be used as Twokenize(line) in Scala.
    def apply(text: String): Seq[String] = simpleTokenize(squeezeWhitespace(text))

    // More normal name for @apply@
    def tokenize(text: String): Seq[String] = apply(text)

    // Very slight normalization for AFTER tokenization.
    // The tokenization regexes are written to work on non-normalized text.
    // (to make byte offsets easier to compute)
    // Hm: 2+ repeated character normalization here?
    // No, that's more linguistic, should be further down the pipeline
    def normalizeText(text: String) = {
      text.replaceAll("&lt;", "<").replaceAll("&gt;", ">").replaceAll("&amp;", "&")
    }

    def tokenizeForTagger(text: String): Seq[String] = {
      tokenize(text).map(normalizeText)
    }

    // def tokenizeForTagger_J(text: String): Seq[String] = {
    //   tokenizeForTagger(text).toSeq
    // }

    // Convenience method to produce a string representation of the
    // tokenized tweet in a standard-ish format.
    def tokenizeToString(text: String): String = {
      tokenizeForTagger(text).mkString(" ");
    }



  }

  def main(args:Array[String]):Unit={
    val s=" ;(["+'"'+"hello"+'"'+"]);"
    val f=Tokenizer.splitEdgePunct(s)
    println(f)
  }


}
