import scala.collection.mutable

object Sample extends App {

  lazy val random = new java.util.Random

  def printUsageAndExit(): Unit = {
    println("""|Usage:
      |sample <n> [file]: output n > 0 random lines from file or stdin
      |sample <p> [file]: output each line with probability p < 1"""
      .stripMargin)
    sys.exit()
  }

  def parseDouble(s: String): Option[Double] =
    try { Some(s.toDouble) } catch { case _: Throwable => None }

  def parseInt(s: String): Option[Int] =
    try { Some(s.toInt) } catch { case _: Throwable => None }

  def writeRandomFraction(lines: Iterator[String], p: Double): Unit = {
    lines
      .filter(line => random.nextDouble() < p)
      .foreach(println)
  }

  def writeRandomLines(lines: Iterator[String], n: Int): Unit = {
    require(n > 0)
    val sample = new mutable.ArrayBuffer[(Int, String)]
    var seen = 0
    for (line <- lines) {
      seen += 1
      if (seen <= n)
        sample += ((seen, line))
      else if (random.nextInt(seen) < n) {
        val i = random.nextInt(n)
        sample(i) = (seen, line)
      }
    }
    sample.sortBy(_._1).map(_._2).foreach(println)
  }

  if (!List(1, 2).contains(args.size))
    printUsageAndExit()

  val input = {
    if (args.size == 1 || args(1) == "-")
      io.Source.stdin
    else
      io.Source.fromFile(args(1))
  }

  val lines = input.getLines

  val prob = parseDouble(args(0)).filter { p => 0 <= p && p < 1 }
  val count = parseInt(args(0)).filter { n => 1 <= n }

  if (prob.isDefined)
    writeRandomFraction(lines, prob.get)
  else if (count.isDefined)
    writeRandomLines(lines, count.get)
  else
    printUsageAndExit()

}
