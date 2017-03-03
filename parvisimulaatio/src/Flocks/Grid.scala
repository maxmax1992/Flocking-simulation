package Flocks

import scala.collection.mutable.Buffer
import java.awt.Graphics2D
import java.awt.Color
import scala.collection.mutable.ArrayBuffer

/*
 * This class is used for optimization of calculations, when looking for near boids
 * each boids only filters the cells around it making calculation with huge amount of boids faster
 */
class Grid {
  val width = Simulation.width
  val height = Simulation.height
  val boxWidth = 50
  val boxHeight = 50

  //main array that contains cells
  val array = Array.ofDim[Cell](width / boxWidth, height / boxHeight)

  //array initialization
  for (i <- 0 until array.length) {
    for (j <- 0 until array(i).length) {
      array(i)(j) = new Cell(this, i, j)
    }
  }

  //this is needed for debugging reasons
  override def toString() = {
    var string = ""
    for (i <- 0 until array.length) {
      for (j <- 0 until array(i).length) {
        string += array(i)(j).toString()
      }
      string += "\n"
    }
    string
  }

  //updates all cells, this is needed when boids are removed or added
  def updateAll(): Unit = {
    for (i <- 0 until array.size) {
      for (j <- 0 until array(i).size) {
        array(i)(j).boids = array(i)(j).boids.intersect(World.boids)
      }
    }
  }

  //gets cells by overlapping over World bounds
  def bound(xBound: Int, yBound: Int, x: Int, y: Int): (Int, Int) = {
    val newX = {
      if (x >= xBound)
        x - xBound
      else if (x < 0)
        x + xBound
      else x
    }

    val newY = {
      if (y >= yBound)
        y - yBound
      else if (y < 0)
        y + yBound
      else y
    }

    if (newX != x || newY != y)
      (newX, newY)
    else
      (x, y)
  }
  
  //gets all the boids around given boid with radius r
  def getBoids(boid: Boid, radius: Int): Array[Boid] = {

    val index = math.ceil(radius.toDouble / boxHeight).toInt

    var seq = Array[Boid]()

    for (i <- (boid.cell.indexX - index + 1) to (boid.cell.indexX + index)) {
      for (j <- (boid.cell.indexY - index + 1) to (boid.cell.indexY + index)) {
        val cell = bound(width / boxWidth, height / boxHeight, i, j)
        seq ++= array(cell._1)(cell._2).getBoids()
      }
    }
    seq = seq.filter { x => x != boid && boid.getRange(x) <= radius }

    seq
  }

  //draws grid if needed USED FOR DEBUGGING REASONS
  def draw(g: Graphics2D) = {
    g.setColor(Color.WHITE)
    for (i <- 0 until width by boxWidth; j <- 0 until height by boxHeight) {
      g.drawRect(i, j, (i + 50), (j + 50))
    }
  }
}