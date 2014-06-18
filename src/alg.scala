
class Point(xs: Float, ys: Float) {
  var x = xs
  var y = ys
}

class StraightLine(as: Float, bs: Float, cs: Float) {
  def this(p1: Point, p2: Point) = this(p2.y - p1.y, p1.x - p2.x, p2.x * p1.y - p1.x * p2.y)

  var a = as
  var b = bs
  var c = cs

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
  def this(l: StraightLine) = this(if (l.a <= 0) List(l) else List(), if (l.a <= 0) List() else List(l))

  override def toString = "< " ++ pls.toString() ++ " | " ++ prs.toString() ++ ">"

  def topPoint() = (pls, prs) match {
    case (l1 :: _, l2 :: _) => l1 cross l2
    case (l1 :: _, Nil) => new Point(l1.xfory(-Params.infty), -Params.infty)
    case (Nil, l2 :: _) => new Point(l2.xfory(-Params.infty), -Params.infty)
    case (Nil, Nil) => null
  }

  def botPoint() = (pls.reverse, prs.reverse) match {
    case (l1 :: _, l2 :: _) => l1 cross l2
    case (l1 :: _, Nil) => new Point(l1.xfory(Params.infty), Params.infty)
    case (Nil, l2 :: _) => new Point(l2.xfory(Params.infty), Params.infty)
    case (Nil, Nil) => null
  }
}

sealed abstract class AppState
case class Selecting() extends AppState
case class Running(ap: AlgorithmPart) extends AppState

class AlgorithmPart(hps: Array[StraightLine]) {
  var sweepLine: SweepLine = null

  class SweepLine(var bal: List[StraightLine], var bar: List[StraightLine], var bbl: List[StraightLine], var bbr: List[StraightLine]) {
    var level: Float = 0.0f
    var brushA: Array[StraightLine] = Array(null, null)
    var brushB: Array[StraightLine] = Array(null, null)
    var edgeCodes: Set[Int] = Set()
    // 1 = bal, 2 = bar, 3 = bbl, 4 = bbr
    var leftSoFar: List[StraightLine] = Nil
    var rightSoFar: List[StraightLine] = Nil
    var finito: Boolean = false

    // see, if sweepline is crossing the resulting polygon
    def isIntersectionNonEmpty: Boolean = {
      brushA(0) == null || brushB(1) == null || (brushA(0) xfory level) <= (brushB(1) xfory level) &&
        brushA(1) == null || brushB(0) == null || (brushA(1) xfory level) >= (brushB(0) xfory level)
    }

    def leftAB: Boolean = {
      brushA(1) != null && brushB(0) != null && (brushA(1) xfory level) < (brushB(0) xfory level)
    }

    def rightAB: Boolean = {
      brushA(0) != null && brushB(1) != null && (brushA(0) xfory level) > (brushB(1) xfory level)
    }

    def nextIP() {
      def ipls(ls: List[StraightLine]): Float = ls match {
        case l1 :: l2 :: _ => (l1 cross l2).y
        case _ => Params.infty
      }
      val (nextEdgesChangePoint, nextEdgesChanging) = List(bal, bar, bbl, bbr).map(ipls).zip(List(1, 2, 3, 4)).minBy(_._1)
      var totBrush = brushA.zip(List(1, 2)).filter(x => x._1 != null) ++ brushB.zip(List(3, 4)).filter(x => x._1 != null)
      totBrush = totBrush.sortWith((l1, l2) => (l1._1 xfory level) < (l2._1 xfory level))
      var nextIntersectionPoint = Params.infty
      var nextIntersecting = (0, 0)
      for (i <- 1 to totBrush.length - 1) {
        val c = (totBrush(i)._1 cross totBrush(i - 1)._1).y
        if (c > level && c < nextIntersectionPoint) {
          nextIntersectionPoint = c
          nextIntersecting = (totBrush(i - 1)._2, totBrush(i)._2)
        }
      }

      // Now we know, that the next point of interest may be of two types.
      //  - It might be the beginning of another edge on one of four boundaries (nextEdgesChangePoint)
      //  - It might be an intersection of some of the boundaries (nextIntersectionPoint)


      if (nextEdgesChangePoint < nextIntersectionPoint) {
        level = nextEdgesChangePoint
        if (edgeCodes contains nextEdgesChanging)
          nextEdgesChanging match {
            case 1 =>
              bal = bal.tail
              leftSoFar = bal.head :: leftSoFar
            case 2 =>
              bar = bar.tail
              rightSoFar = bar.head :: rightSoFar
            case 3 =>
              bbl = bbl.tail
              leftSoFar = bbl.head :: leftSoFar
            case 4 =>
              bbr = bbr.tail
              rightSoFar = bbr.head :: rightSoFar
          }
      } else {
        level = nextIntersectionPoint
        if (edgeCodes.isEmpty && isIntersectionNonEmpty) {
          edgeCodes += nextIntersecting._1
          edgeCodes += nextIntersecting._2
          nextIntersecting match {
            case (2, 3) =>
              leftSoFar = List(bbl.head)
              rightSoFar = List(bar.head)
            case (4, 1) =>
              leftSoFar = List(bal.head)
              rightSoFar = List(bbr.head)
          }
        }

        if (Set(nextIntersecting._1, nextIntersecting._2) subsetOf edgeCodes) {
          finito = true
          return
        } else {
          val inting = edgeCodes & Set(nextIntersecting._1, nextIntersecting._2)
          if (inting.nonEmpty)
            inting.head match {
              case 1 =>
                edgeCodes -= 1
                edgeCodes += 3
                leftSoFar = bbl.head :: leftSoFar
              case 2 =>
                edgeCodes -= 2
                edgeCodes += 4
                rightSoFar = bbr.head :: rightSoFar
              case 3 =>
                edgeCodes -= 3
                edgeCodes += 1
                leftSoFar = bal.head :: leftSoFar
              case 4 =>
                edgeCodes -= 4
                edgeCodes += 2
                rightSoFar = bar.head :: rightSoFar
            }
        }
      }
    }

    def currentPolygon(): Polygon = {
      if (leftSoFar.nonEmpty || rightSoFar.nonEmpty)
        new Polygon(leftSoFar.reverse, (new StraightLine(0, -1, level) :: rightSoFar).reverse)
      new Polygon(Nil, Nil)
    }
  }

  def intersectPolygons(p1: Polygon, p2: Polygon) {
    val hora = Math.max(p1.topPoint().y, p2.topPoint().y)
    def clearTop(ls: List[StraightLine], f: (Float, Float) => Boolean) = ls match {
      case l1 :: l2 :: lsp => if (f(l2 xfory hora, l1 xfory hora)) ls.tail else ls
      case _ => ls
    }
    def lt(x: Float, y: Float) = x <= y
    def gt(x: Float, y: Float) = x >= y
    sweepLine = new SweepLine(clearTop(p1.pls, gt), clearTop(p1.prs, lt), clearTop(p2.pls, gt), clearTop(p2.prs, lt))

  }
}