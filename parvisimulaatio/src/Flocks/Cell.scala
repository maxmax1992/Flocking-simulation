package Flocks
import scala.collection.mutable.ArrayBuffer

/*
 * this Class main purpose is to contain boids into the cell, every time boid moves
 * it updates itself position in the grid cell
 */
class Cell(grid: Grid, val indexX: Int, val indexY: Int) {

  var boids = ArrayBuffer[Boid]()
  
  //gets the boids ArrayBuffer
  def getBoids(): ArrayBuffer[Boid] = {
    this.boids
  }
  
  //adds boid to boids ArrayBuffer
  def addBoid(boid: Boid) = {
    boids += boid
  }
  
  //deletes boid from ArrayBuffer
  def deleteBoid(boid: Boid) = {
    boids -= boid
  }
  
  //method for debugging reasons
  override def toString() = {
    if(boids.isEmpty) {
      " " + indexX + indexY + " "
    } else {
      boids.size.toString()
    }
  }
  
}
