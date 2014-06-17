import processing.core._
import PConstants._


object StraightScalaRunner {
  def main(args: Array[String]) {
    PApplet.main(Array("DrawingPart"))
  }
}

object Params {
  val infty = 1000000.0f
}

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

  def drawPolygon(pg: Polygon, rgb: Int) {
    if (pg.pls.isEmpty && pg.prs.isEmpty)
      return
    stroke(rgb)
    fill(rgb, 20)
    beginShape()
    // we start with the uppermost point
    val tp = pg.topPoint
    vertex(tp.x, tp.y)
    // now we build right border
    for (p <- internalCrossPoints(pg.prs))
      vertex(p.x, p.y)
    if (pg.prs.isEmpty) {
      vertex(Params.infty, -Params.infty)
      vertex(Params.infty, Params.infty)
    }
    val bp = pg.botPoint
    vertex(bp.x, bp.y)
    for (p <- internalCrossPoints(pg.pls.reverse))
      vertex(p.x, p.y)
    if (pg.pls.isEmpty) {
      vertex(-Params.infty, Params.infty)
      vertex(-Params.infty, -Params.infty)
    }
    endShape(CLOSE)
  }

  override def setup {
    background(0)
    size(1000, 700)
    state = Selecting()
  }

  override def draw {
    background(0)
    state match {
      case Selecting() => {
        stroke(255)
        for (l <- lines) {
          drawPolygon(new Polygon(l), 255)
        }
        stroke(color(255, 0, 0))
        if (candLine != null) {
          drawPolygon(new Polygon(candLine), color(255, 0, 0))
        }
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
        fill(0, 102, 153)
        textSize(50)
        text("algPart", width / 2 - 50, height / 2 - 20)
      }
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
    lines = candLine :: lines
    candLine = null
  }

  override def mouseClicked {
    state match {
      case Selecting() => {
        if (12 <= mouseX && mouseX <= 12 + 96 && height - 58 <= mouseY && mouseY <= height - 58 + 46)
          state = Running(new AlgorithmPart(lines.toArray))
      }
      case _ => {}
    }
  }
}