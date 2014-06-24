import processing.core.PConstants._
import processing.core._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._


class DrawingPart extends PApplet {

  var lines: List[StraightLine] = List()
  var stx = 0.0f
  var sty = 0.0f
  var candLine: StraightLine = null

  var state: AppState = null

  def internalCrossPoints(ls: List[StraightLine]): List[Point] = ls match {
    case l1 :: l2 :: lss => (l1 cross l2) :: internalCrossPoints(l2 :: lss)
    case _ => Nil
  }

  def drawPolygon(pg: Polygon, rgb: Int, alp: Int) {
    if (pg.pls.isEmpty && pg.prs.isEmpty)
      return
    stroke(rgb)
    fill(rgb, alp)
    beginShape()
    // we start with the uppermost point
    val tp = pg.topPoints
    for (p <- tp)
      vertex(p.x, p.y)
    // now we build right border
    for (p <- internalCrossPoints(pg.prs))
      vertex(p.x, p.y)
    val bp = pg.botPoints
    for (p <- bp)
      vertex(p.x, p.y)
    for (p <- internalCrossPoints(pg.pls.reverse))
      vertex(p.x, p.y)
    endShape(CLOSE)
  }

  override def setup {
    background(0)
    size(Params.width, Params.height)
    if (frame != null)
      frame.setResizable(true)
    state = Selecting()
  }

  override def draw {
    background(0)
    state match {
      case Selecting() => {
        stroke(255)
        for (l <- lines)
          drawPolygon(new Polygon(l), 255, 20)
        stroke(color(255, 0, 0))
        if (candLine != null)
          drawPolygon(new Polygon(candLine), color(255, 0, 0), 40)
        stroke(color(227, 66, 52))
        fill(color(227, 66, 52))
        rect(10, height - 60, 100, 50)
        stroke(color(255, 127, 80))
        if (12 <= mouseX && mouseX <= 12 + 96 && height - 58 <= mouseY && mouseY <= height - 58 + 46)
          fill(color(0, 102, 153), 20)
        else
          fill(0, 102, 153)
        rect(12, height - 58, 96, 46)
        textSize(25)
        fill(255)
        text("start", 30, height - 27)
      }
      case Running(ap) => {
        for (i <- 0 to ap.beg - 1)
          drawPolygon(new Polygon(ap.hps(i)), color(47, 79, 79), 5)
        for (i <- ap.beg to ap.end - 1)
          drawPolygon(new Polygon(ap.hps(i)), 255, 8)
        for (i <- ap.end to ap.hps.length - 1)
          drawPolygon(new Polygon(ap.hps(i)), color(47, 79, 79), 5)
        if (ap.sweepLine != null) {
          val sline = ap.sweepLine
          val p1 = ap.iPol1
          val p2 = ap.iPol2
          drawPolygon(p1, color(0, 0, 255), 20)
          drawPolygon(p2, color(0, 255, 0), 20)
          val upbeach = new Polygon(new StraightLine(0, -1, -sline.level))
          //          drawPolygon(upbeach, color(255, 0, 0), 100)
          stroke(color(255, 0, 0))
          line(-Params.infty, sline.level, Params.infty, sline.level)
          drawPolygon(sline.currentPolygon(), color(255, 127, 0), 50)
          //          drawPolygon(new AlgorithmPart(Array()).intersectPolygons(sline.currentPolygon(), upbeach), color(255, 127, 0), 100)
          fill(255)
          textSize(15)
          text("beg: " ++ ap.beg.toString ++ ", end: " ++ ap.end.toString, 30, height - 32)
          text("level: " ++ sline.level.toString, 30, height - 16)
        } else {
          fill(255)
          textSize(20)
          text("beg: " ++ ap.beg.toString ++ ", end: " ++ ap.end.toString, 30, height - 27)
        }
      }
      case Finished(pg) =>
        for (l <- lines)
          drawPolygon(new Polygon(l), 255, 20)
        drawPolygon(pg, color(255, 127, 0), 100)
    }
  }

  override def mousePressed {
    stx = mouseX
    sty = mouseY
  }

  override def mouseDragged {
    candLine = new StraightLine(new Point(stx, sty), new Point(mouseX, mouseY))
  }

  override def mouseReleased {
    if (12 <= mouseX && mouseX <= 12 + 96 && height - 58 <= mouseY && mouseY <= height - 58 + 46)
      return
    lines = candLine :: lines
    candLine = null
  }

  override def mouseClicked {
    state match {
      case Selecting() => {
        if (12 <= mouseX && mouseX <= 12 + 96 && height - 58 <= mouseY && mouseY <= height - 58 + 46) {
          val ap = new AlgorithmPart(lines.toArray)
          state = Running(ap)
          val f = future {
            ap.halfPlanesIntersection()
          }
          f onSuccess {
            case pg => state = Finished(pg)
          }

        }
      }
      case _ => {}
    }
  }
}


object Params {
  val infty = 100000.0f
  val eps = 1 / infty
  val delay = 1000
  val width = 1200
  val height = 750
}