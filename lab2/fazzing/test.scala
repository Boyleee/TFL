import java.util.regex.Pattern

object FuzzEquivalence extends App {

  private val patAcad: Pattern =
    Pattern.compile("^(?:aaaaa|aaa)*(?:aa|b)(?:bb|aab)*$")

  private val patExt: Pattern =
    Pattern.compile("^(?:a{3}|a{5}|a{6}|a{8,})?(?:aa|b)(?:(?:bb|aab)+)?$")

  private def acceptsRegexAcad(s: String): Boolean =
    patAcad.matcher(s).matches()

  private def acceptsRegexExt(s: String): Boolean =
    patExt.matcher(s).matches()

  private val nfaStart = 0
  private val nfaAccept: Set[Int] = Set(6)

  private val nfaTrans: Map[(Int, Char), Set[Int]] = Map(
    (0, 'a') -> Set(1, 5),
    (0, 'b') -> Set(6),

    (1, 'a') -> Set(2),
    (2, 'a') -> Set(0, 3),
    (3, 'a') -> Set(4),
    (4, 'a') -> Set(0),

    (5, 'a') -> Set(6),

    (6, 'b') -> Set(7),
    (7, 'b') -> Set(6),

    (6, 'a') -> Set(8),
    (8, 'a') -> Set(9),
    (9, 'b') -> Set(6)
  )

  private val nfaEps: Map[Int, Set[Int]] = Map.empty


  private def epsClosure(states: Set[Int]): Set[Int] = {
    val stack = collection.mutable.Stack[Int]()
    val seen  = collection.mutable.Set[Int]()
    states.foreach { s => stack.push(s); seen += s }
    while (stack.nonEmpty) {
      val v = stack.pop()
      for (u <- nfaEps.getOrElse(v, Set.empty))
        if (!seen(u)) { seen += u; stack.push(u) }
    }
    seen.toSet
  }

  private def acceptsNFA(w: String): Boolean = {
    var cur = epsClosure(Set(nfaStart))
    for (c <- w) {
      val next = cur.flatMap(s => nfaTrans.getOrElse((s, c), Set.empty[Int]))
      cur = epsClosure(next)
      if (cur.isEmpty) return false
    }
    cur.exists(nfaAccept)
  }

  private val dfaStart = 0
  private val dfaAccept: Set[Int] = Set(2, 3, 8, 10, 11, 12, 14)

  private val dfaTrans: Map[(Int, Char), Int] = Map(
    (0, 'a') -> 1,  (0, 'b') -> 2,
    (1, 'a') -> 3,  (1, 'b') -> 15,
    (2, 'a') -> 4,  (2, 'b') -> 5,
    (3, 'a') -> 6,  (3, 'b') -> 5,
    (4, 'a') -> 5,  (4, 'b') -> 15,
    (5, 'a') -> 15, (5, 'b') -> 2,
    (6, 'a') -> 7,  (6, 'b') -> 2,
    (7, 'a') -> 8,  (7, 'b') -> 2,
    (8, 'a') -> 9,  (8, 'b') -> 10,
    (9, 'a') -> 11, (9, 'b') -> 2,
    (10,'a') -> 4,  (10,'b') -> 10,
    (11,'a') -> 12, (11,'b') -> 10,
    (12,'a') -> 13, (12,'b') -> 10,
    (13,'a') -> 14, (13,'b') -> 2,
    (14,'a') -> 14, (14,'b') -> 10,
    (15,'a') -> 15, (15,'b') -> 15
  )

  private def acceptsDFA(w: String): Boolean = {
    var state = dfaStart
    for (c <- w) state = dfaTrans((state, c))
    dfaAccept(state)
  }

  private val i1Start  = 0
  private val i1Accept = Set(0, 1, 2)

  private val i1Trans: Map[(Int, Char), Int] = Map(
    (0,'a') -> 0, (0,'b') -> 1,
    (1,'a') -> 2, (1,'b') -> 1,
    (2,'a') -> 0, (2,'b') -> 3,
    (3,'a') -> 3, (3,'b') -> 3
  )

  private def acceptsI1(w: String): Boolean = {
    var s = i1Start
    for (c <- w) s = i1Trans((s, c))
    i1Accept(s)
  }

  private val i2Start  = 0
  private val i2Accept = Set(0, 1)

  private val i2Trans: Map[(Int, Char), Int] = Map(
    (0,'a') -> 0, (0,'b') -> 1,
    (1,'a') -> 2, (1,'b') -> 1,
    (2,'a') -> 0, (2,'b') -> 1
  )

  private def acceptsI2(w: String): Boolean = {
    var s = i2Start
    for (c <- w) s = i2Trans((s, c))
    i2Accept(s)
  }

  private def acceptsCFA(w: String): Boolean =
    acceptsDFA(w) && acceptsI1(w) && acceptsI2(w)

  private val rnd = new scala.util.Random()

  private def randomWord(maxLen: Int): String = {
    val len = rnd.nextInt(maxLen + 1)
    val sb  = new StringBuilder(len)
    val alphabet = Array('a', 'b')
    var i = 0
    while (i < len) {
      sb.append(alphabet(rnd.nextInt(2)))
      i += 1
    }
    sb.toString()
  }

  private def checkWord(w: String): Option[String] = {
    val v1 = acceptsRegexAcad(w)
    val v2 = acceptsRegexExt(w)
    val v3 = acceptsNFA(w)
    val v4 = acceptsDFA(w)
    val v5 = acceptsCFA(w)
    val vals = List(("acad", v1), ("ext", v2), ("nfa", v3), ("dfa", v4), ("cfa", v5))
    if (vals.map(_._2).distinct.size == 1) None
    else Some(s"Mismatch for '$w': " + vals.map { case (n,v) => s"$n=$v" }.mkString(", "))
  }

  val maxLenEnum = 10
  for (len <- 0 to maxLenEnum) {
    val total = 1 << len
    var mask = 0
    while (mask < total) {
      val w = (0 until len).map { i =>
        if (((mask >> i) & 1) == 0) 'a' else 'b'
      }.mkString
      checkWord(w) match {
        case Some(msg) =>
          println(msg)
          sys.exit(1)
        case None =>
      }
      mask += 1
    }
  }

  val randomTests = 50000
  var i = 0
  while (i < randomTests) {
    val w = randomWord(25)
    checkWord(w) match {
      case Some(msg) =>
        println(msg)
        sys.exit(1)
      case None =>
    }
    i += 1
  }

  println("ok")
}
