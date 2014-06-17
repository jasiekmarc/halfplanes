
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

  override def toString() = "\"" ++ a.toString ++ "x + " ++ b.toString() ++ "y + " ++ c.toString() ++ " = 0\""
}

class Polygon(val pls: List[StraightLine], val prs: List[StraightLine]) {
  def this(l: StraightLine) = this(if (l.a <= 0) List(l) else List(), if (l.a <= 0) List() else List(l))

  override def toString() = "< " ++ pls.toString() ++ " | " ++ prs.toString() ++ ">"

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
  var sweepLine: Float

  // Compute an intersection for two polygons
  def intersectPolygons(p1: Polygon, p2: Polygon): Polygon = {
    sweepLine = Math.max(p1.topPoint.y, p2.topPoint.y)
    // at every point we want to maintain four edges C1l, C2l, C1r, C2r

  }
}