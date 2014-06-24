

sealed abstract class AppState

class Point(xs: Float, ys: Float) {
  var x = xs
  var y = ys
}

class StraightLine(as: Float, bs: Float, cs: Float) {
  var a = as
  var b = bs
  var c = cs

  def this(p1: Point, p2: Point) = this(p2.y - p1.y, p1.x - p2.x, p2.x * p1.y - p1.x * p2.y)

  def in(x: Float, y: Float) = {
    a * x + b * y + c <= 0
  }

  def yforx(x: Float) = {
    -(a * x + c) / b
  }

  def xfory(y: Float) = {
    -(b * y + c) / a
  }

  def cross(l: StraightLine) = {
    val det = a * l.b - l.a * b
    new Point((-l.b * c + b * l.c) / det, (l.a * c - a * l.c) / det)
  }

  override def toString = "\"" ++ a.toString ++ "x + " ++ b.toString ++ "y + " ++ c.toString ++ " = 0\""
}

class Polygon(val pls: List[StraightLine], val prs: List[StraightLine]) {
  def this(l: StraightLine) = this(if (l.a < 0) List(l) else List(), if (l.a < 0) List() else List(l))

  override def toString = "< " ++ pls.toString() ++ " | " ++ prs.toString() ++ ">"

  def topPoints() = (pls, prs) match {
    case (l1 :: _, l2 :: _) =>
      if ((l1 xfory -Params.infty) < (l2 xfory -Params.infty))
        List(new Point(l1 xfory -Params.infty, -Params.infty), new Point(l2 xfory -Params.infty, -Params.infty))
      else
        List(l1 cross l2)
    case (l1 :: _, Nil) => List(new Point(l1 xfory -Params.infty, -Params.infty), new Point(Params.infty, -Params.infty))
    case (Nil, l2 :: prsp) =>
      l2.a match {
        case 0 =>
          if (l2.b > 0)
            List(new Point(-Params.infty, -Params.infty), new Point(Params.infty, -Params.infty))
          else if (prsp.nonEmpty)
            List(new Point(-Params.infty, l2 yforx -Params.infty))
          else
            List(new Point(-Params.infty, l2 yforx -Params.infty), new Point(Params.infty, l2 yforx Params.infty))
        case _ =>
          List(new Point(-Params.infty, -Params.infty), new Point(l2 xfory -Params.infty, -Params.infty))

      }
    case (Nil, Nil) => null
  }

  def botPoints() = (pls.reverse, prs.reverse) match {
    case (l1 :: _, l2 :: _) =>
      if ((l1 xfory Params.infty) < (l2 xfory Params.infty))
        List(new Point(l2 xfory Params.infty, Params.infty), new Point(l1 xfory Params.infty, Params.infty))
      else
        List(l1 cross l2)
    case (l1 :: _, Nil) => List(new Point(Params.infty, Params.infty), new Point(l1 xfory Params.infty, Params.infty))
    case (Nil, l2 :: prsp) =>
      l2.a match {
        case 0 =>
          if (l2.b < 0)
            List(new Point(Params.infty, Params.infty), new Point(-Params.infty, Params.infty))
          else if (prsp.nonEmpty)
            List(new Point(-Params.infty, l2 yforx -Params.infty))
          else
            List(new Point(Params.infty, l2 yforx Params.infty), new Point(-Params.infty, l2 yforx -Params.infty))
        case _ =>
          List(new Point(l2 xfory Params.infty, Params.infty), new Point(-Params.infty, Params.infty))

      }
    case (Nil, Nil) => null
  }

  def isEmpty = pls.isEmpty && prs.isEmpty
}

case class Selecting() extends AppState
case class Running(ap: AlgorithmPart) extends AppState

case class Finished(pg: Polygon) extends AppState

class AlgorithmPart(val hps: Array[StraightLine]) {
  var beg = 0
  var end = hps.length
  var sweepLine: SweepLine = null
  var iPol1: Polygon = null
  var iPol2: Polygon = null

  def halfPlanesIntersection(b: Int = 0, e: Int = hps.length): Polygon = {
    beg = b
    end = e
    Thread.sleep(Params.delay)
    e - b match {
      case 1 => new Polygon(hps(b))
      case _ =>
        val m = (b + e) / 2
        val lef = halfPlanesIntersection(b, m)
        val rig = halfPlanesIntersection(m, e)
        beg = b
        end = e
        printf("intersecting (%d - %d) and (%d - %d)\n", b, m, m, e)
        intersectPolygons(lef, rig)
    }
  }

  def intersectPolygons(p1: Polygon, p2: Polygon): Polygon = {
    if (p1.isEmpty || p2.isEmpty)
      return new Polygon(List(), List())
    iPol1 = p1
    iPol2 = p2
    sweepLine = new SweepLine(p1, p2)
    sweepLine.init()
    Thread.sleep(Params.delay)
    do {
      sweepLine.nextPI()
      Thread.sleep(Params.delay)
    } while (!sweepLine.finito)
    val ret = sweepLine.currentPolygon()
    println("\tret => " ++ ret.toString)
    Thread.sleep(3 * Params.delay)
    sweepLine = null
    iPol1 = null
    iPol2 = null
    ret
  }

  class SweepLine(var bl: Array[List[StraightLine]], var br: Array[List[StraightLine]]) {
    var level: Float = 0.0f
    var hasBeenOpen: Boolean = false
    var finito: Boolean = false
    var leftCurEdge: StraightLine = null
    var rightCurEdge: StraightLine = null
    var leftIntBor: List[StraightLine] = Nil
    var rightIntBor: List[StraightLine] = Nil

    def this(p1: Polygon, p2: Polygon) = this(List(p1.pls, p2.pls).filter(xs => xs.nonEmpty).toArray, List(p1.prs, p2.prs).filter(xs => xs.nonEmpty).toArray)

    def init() {
      level = -Params.infty
      bl = bl.sortBy(xs => xs.head xfory level)
      br = br.sortBy(xs => xs.head xfory level)
      leftCurEdge = if (bl.isEmpty) null else bl.last.head
      rightCurEdge = if (br.isEmpty) null else br.head.head
      hasBeenOpen = isOpen
      if (isOpen) {
        leftIntBor = if (bl.isEmpty) Nil else List(bl.last.head)
        rightIntBor = if (br.isEmpty) Nil else List(br.head.head)
      }
    }

    def nextPI() {
      val nextCrossL = bl match {
        case Array(ls1, ls2) => if ((ls1.head cross ls2.head).y > level) (ls1.head cross ls2.head).y else Params.infty
        case _ => Params.infty
      }
      val nextCrossR = br match {
        case Array(ls1, ls2) => if ((ls1.head cross ls2.head).y > level) (ls1.head cross ls2.head).y else Params.infty
        case _ => Params.infty
      }

      val nextCrossLR =
        if (leftCurEdge == null || rightCurEdge == null)
          Params.infty
        else if ((leftCurEdge cross rightCurEdge).y > level)
          (leftCurEdge cross rightCurEdge).y
        else
          Params.infty

      def nextEC(ls: List[StraightLine]): Float = ls match {
        case l1 :: l2 :: _ => (l1 cross l2).y
        case _ => Params.infty
      }

      val nextEdgeChangeL = if (bl.nonEmpty) bl.map(nextEC).zip(List(0, 1)).minBy(x => x._1) else (Params.infty, -1)
      val nextEdgeChangeR = if (br.nonEmpty) br.map(nextEC).zip(List(0, 1)).minBy(x => x._1) else (Params.infty, -1)


      val nextEvent = List(nextCrossL, nextCrossR, nextCrossLR, nextEdgeChangeL._1, nextEdgeChangeR._1).min

      printf("\tnextEvent (%f): ", nextEvent)
      nextEvent match {
        case Params.infty =>
          println("none")
          finito = true
        case `nextCrossL` =>
          println("CrossL")
          bl = bl.reverse
          if (isOpen)
            leftIntBor = bl.last.head :: leftIntBor
          leftCurEdge = bl.last.head
        case `nextCrossR` =>
          println("CrossR")
          br = br.reverse
          if (isOpen)
            rightIntBor = br.head.head :: rightIntBor
          rightCurEdge = br.head.head
        case `nextCrossLR` =>
          println("CrossLR")
          if (isOpen)
            finito = true
          else {
            hasBeenOpen = true
            leftIntBor = List(leftCurEdge)
            rightIntBor = List(rightCurEdge)
          }
        case nextEdgeChangeL._1 =>
          println("EdgeChangeL")
          bl(nextEdgeChangeL._2) = bl(nextEdgeChangeL._2).tail
          if (nextEdgeChangeL._2 == bl.length - 1) {
            leftCurEdge = bl(nextEdgeChangeL._2).head
            if (isOpen)
              leftIntBor = leftCurEdge :: leftIntBor
          }
        case nextEdgeChangeR._1 =>
          println("EdgeChangeR")
          br(nextEdgeChangeR._2) = br(nextEdgeChangeR._2).tail
          if (nextEdgeChangeR._2 == 0) {
            rightCurEdge = br(nextEdgeChangeR._2).head
            if (isOpen)
              rightIntBor = rightCurEdge :: rightIntBor
          }
      }
      level = nextEvent
    }

    def isOpen: Boolean = (leftCurEdge, rightCurEdge) match {
      case (l: StraightLine, r: StraightLine) =>
        (l xfory (level + Params.eps)) <= (r xfory (level + Params.eps))
      case _ => true
    }

    def currentPolygon() = new Polygon(leftIntBor.reverse, rightIntBor.reverse)
  }
}