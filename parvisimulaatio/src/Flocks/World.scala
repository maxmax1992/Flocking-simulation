package Flocks

import java.awt.Graphics2D

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Avaruus joka sisältää asteroideja
 */
object World {
  
  //random that we use to generate various random coordinates, speed directions...
  val random = new Random()

  //starting speed of generated boids
  val startingSpeed = 2
  
  //size of each boid
  val boidSize = 20
  
  //inital amount of boids
  var boidAmount = 70
  
  //buffer that contains boids
  var boids = ArrayBuffer[Boid]()

  //adds boids to world
  for(i <- 0 until boidAmount) {
      addBoid()
  }

  //delete random boid from world method
  def deleteBoid() = {
      boids = boids - boids(random.nextInt(boids.size))
      boids.foreach { x => x.updateCell() }
      Simulation.grid.updateAll
   }
  
  //add boid to random place in the world method
  def addBoid():Unit = {
    
    val speedVectors = (if(random.nextBoolean()) +this.startingSpeed 
    else -startingSpeed, if(random.nextBoolean()) +this.startingSpeed else -startingSpeed)
    val randomX = random.nextInt(Simulation.width - 1)
    val randomY = random.nextInt(Simulation.height - 1)
    
    boids += new Boid(new Vector2D(speedVectors._1, speedVectors._2),
        new Vector2D(randomX, randomY), boidSize, Simulation.grid )
    boids.foreach { x => x.updateCell() }
    Simulation.grid.updateAll
  }
  
  //sets boid amount to specified number
  def setBoidAmount(n: Int) = {
    if(n < 0 ) {
      throw new NumberFormatException
    } else if(n < this.boids.size) {
      while(this.boids.size != n) {
        this.deleteBoid()
      }
    } else if(n > this.boids.size) {
      while(this.boids.size != n) {
        this.addBoid()
      }
    }
  }

  //Moving is called all the time, moving is executed in different threads, allowing faster calculations of movement
  def step() = {
    try {
    boids.foreach { x => x.updateCell() }
    boids.par.foreach { x => x.move() }
    }
    finally {
    }
  }

  //draws boids to panel
  def draw(g: Graphics2D) = boids foreach (_.draw(g))
  
}