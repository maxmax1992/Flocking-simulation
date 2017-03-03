package Flocks

import java.lang.Object
import math.sqrt
import math.abs
import math.pow
import java.awt.Graphics2D
import java.awt.geom._
import java.awt.geom.{GeneralPath, Path2D}
import scala.util.Random
import java.awt.Color

/**
 *Each boid has speed ector, place vector which indicates its place from origin
 * parameter 'r' which indicates the radius of the circle body of the boid and the grid for efficency purposes
 */
case class Boid(var speed: Vector2D, var place: Vector2D, var r: Int, grid: Grid) {
  
  val random = new Random()
  
  /*
   * lastTick is used to mantain the same speed of the boid, independently 
  from framerate or timer tick rate in object Simulation
  */
  var lastTick = System.currentTimeMillis()
  
  val red = random.nextInt(256)
  val green = random.nextInt(256)
  val blue = random.nextInt(256)
  
  /* 4 main factors by which the movement of the boids is calculated
   * COHENSION  -> boid is going to average position of flock neighbors the higher the smoother
   * SEPARATION -> boid is keeping the lowDistance to local flock neighbors the higher the more aware it is of the neighbours
   * ALIGNMENT  -> boid is calculation the average speedVector of flock neighbors
   * LOWERFACTORS -> makes ALL THREE factors to affect less the boids the higher the smoother
   */
  
  //FACTORS
  var cohension:Double = 245.0
  var separation:Double = 2.50
  var alignment:Double = 0.1
  var lowerFactors = 20.0
  
  //PROPERTIES OF EACH BOID
  var minDistance = 50
  var viewRadius = 300
  val maxSpeed = 2
 
  //initial placement to cell inside grid
  val x = math.max( math.min(((place.y-1)/grid.boxWidth).toInt, 11), 0)
  val y = math.max( math.min(((place.x-1)/grid.boxHeight).toInt, 11), 0)
  var cell = grid.array(x)(y)
  val rand = new Random()
  
  def updateCell() = {
    cell.deleteBoid(this)
//    println("PLACE X: " + place.x + " " + grid.boxHeight)
    val x = math.max( math.min(((place.y-1)/grid.boxWidth).toInt, 11), 0)
    val y = math.max( math.min(((place.x-1)/grid.boxHeight).toInt, 11), 0)
    this.cell = grid.array(x)(y)
    this.cell.addBoid(this)
  }
    
  
  /*
   * Returns range of the boids taking into account that each boid viewRadius can 
   * overlap inside the world over the bounds
   *  to the other sides
   */
  def getRange(another: Boid):Double = {
    import math.abs
    val xRange = {
      if(abs(another.place.x - this.place.x) <= grid.width/2) {
        another.place.x - this.place.x
      } else {
        if(another.place.x >= this.place.x) {
          this.place.x + (grid.width - another.place.x)
        } else {
          another.place.x + (grid.width - this.place.x)
        }
      }
    }
    val yRange = {
      if(abs(another.place.y - this.place.y) <= grid.height/2) {
        another.place.y - this.place.y
      } else {
        if(another.place.y >= this.place.y) {
          this.place.y + (grid.height - another.place.y)
        } else {
          another.place.y + (grid.height - this.place.y)
        }
      }
    }
//    println(    math.sqrt(math.pow(xRange, 2) + math.pow(yRange, 2) ))
    math.sqrt(math.pow(xRange, 2) + math.pow(yRange, 2) )
  }
  
  
  /*Normal range calculation method that uses pythagoras 
   * equation to calculate the distance from given boid
   */
  def getRangenoBounds(another: Boid): Double = {
    sqrt(pow(this.place.x - another.place.x, 2) + pow((this.place.y - another.place.y), 2) ) 
  }
  
  //returns the x range to given boid comparing place vectors x component
  def getXrange(another: Boid): Double = {
    abs(this.place.x - another.place.x)
  }
  
  //returns the y range to given boid comparing place vectors y component
  def getYrange(another: Boid): Double = {
    abs(this.place.y - another.place.y)
  }

  /*calculates the average position of the boids 
   * that are visible by this boid depending on viewRange,
   * then calculates needed speed vector to go into that position
   */
  def rule1(a: Array[Boid]):Vector2D = {
    val debug = false
    
    //returns zero Vector that we should 
    if(a.length == 0) {
      return Vector2D(0,0)
    }
    
    var middleMass: Vector2D = {
      var b = Vector2D(0,0)
      for(i <- 0 until a.length) {
        b += this.calculateClosestBoid(a(i))
      }
      b = b/a.length 
      b

    } 
    //we lower this vector amount by dividing it by given factor
    val newA = (middleMass - this.place)/cohension
    newA
  }

  
  //boid tries to avoid other boids inside viewRange
  def rule2(a: Array[Boid]):Vector2D = {
    
    if(a.length == 0) {
      return Vector2D(0, 0)
    }
    
    //sum vector that indicates where this boid should go
    var vectorC = Vector2D(0, 0)
    
    /*if boid sees other boids closer than minimum 
     * distance it will add to sum vector 
     */
    for(i <- 0 until a.length) {
     if(this.getRange(a(i)) < this.minDistance) {
       vectorC = vectorC + (this.place - this.calculateClosestBoid(a(i)) ).changeLength(1)
     }
    }
    
    (vectorC*separation)
    
  }
  
  //gets the average speed vector of the boids inside viewRange
  def rule3(a: Array[Boid]):Vector2D = {
    
    if(a.length == 0) {
      return Vector2D(0, 0)
    }
    
    //sum vector
    var vector = Vector2D(0,0)
    
    for(i <- 0 until a.length) {
      vector = vector + a(i).speed
    }
    
    vector = vector/a.length
    
    (vector - this.speed)*this.alignment
    
  }
  /*this method is responsible for moving the 
   * boids in desired direction
   * 
   */
  def move() = {
    
    //boids that are visible to this boid
    val boids = grid.getBoids(this, this.viewRadius)
    
    //cohension vector that we add to initial speed
    val cohensionVector = this.rule1(boids)
    
    //separation vector that we add to initial speed
    val separationVector = this.rule2(boids)
    
    //alignment vector that we add to initial speed
    val alignmentVector = this.rule3(boids)

    // all vectors above summed to indicate vector that changes or keeps speed
    var sumVector = cohensionVector + separationVector +  alignmentVector
    
    //speed is summed with sumVector and divided by weight coefficient
    this.speed = this.speed + sumVector/this.lowerFactors
   
    //then we limit the speed by this boid variable 'maxSpeed'
    this.speed = this.limitVelocity(this.speed)
    
    //here we take care of speed decreasing when we get lower frame rates
    val a = System.currentTimeMillis()
    val substraction = a - lastTick
    this.lastTick = a
    val toAdd = speed*(substraction/Simulation.speedinMS)
    
    //finaly the speed is changed by summing this place vector with omptized speed vector
    this.place = this.place  + toAdd
    
    //we are taking care of boid overlapping over World bounds
    this.place = place.bound(Simulation.width, Simulation.height)
      
  }
  
  //needed method for speed capping at maxSpeed
  def asUnitVector(v: Vector2D): Vector2D = {
    val length = sqrt(pow(v.x, 2) + pow(v.y, 2))
    v/length
  }
  
  //needed method for speed capping at maxSpeed
  def vectorLength(v: Vector2D):Double = {
    sqrt(pow(v.x, 2) + pow(v.y, 2))
  }

  //needed method for speed capping at maxSpeed
  def limitVelocity(v: Vector2D):Vector2D = {
    if(this.vectorLength(v) > this.maxSpeed ) {
      return (v)/(this.vectorLength(v))*this.maxSpeed
    } else return v
  }
  
  //needed method for debugging reasons
  override def toString() = {
    s" ${this.place.x} ${this.place.y} "
  }
  
  /*
   * this boids can go through the edges of the world so this 
   * must be able to calculate the closest coordinates to given boid 
   * from origin taking into account that boids can overlap over World bounds
   */
  def calculateClosestBoid(another:Boid):Vector2D = {
    
    if(this.getRange(another) != this.getRangenoBounds(another)) {
      if(this.getXrange(another) > Simulation.width/2 
          && this.getYrange(another) > Simulation.height/2) {
        if(this.place.x > another.place.x) {
          if(this.place.y > another.place.y) {
            another.place + Vector2D(Simulation.width, Simulation.height)
          } else {
            another.place + Vector2D(Simulation.width, -Simulation.height)
          }
        } else {
          if(this.place.y < another.place.y) {
            another.place + Vector2D(-Simulation.width , -Simulation.height)
          } else {
            another.place + Vector2D(-Simulation.width, Simulation.height)
          }
        }
      } else if(this.getXrange(another) > this.getYrange(another)) {
        if(this.place.x > another.place.x) {
          another.place + Vector2D(Simulation.width, 0)
        } else {
          another.place + Vector2D(-Simulation.width, 0)
        }
      } else {
        if(this.place.y > another.place.y) {
          another.place + Vector2D(0, Simulation.height)
        } else {
          another.place + Vector2D(0, -Simulation.height)
        }
      }
    } else {
      another.place
    }
  }
  
  //drawing function that draws boids and line indicating this boid speed vector
  def draw(g: Graphics2D) = {
    
    val newX = place.x-(r/2)
    val newY = place.y-(r/2)
    val a = new Color(red, green, blue).brighter()
    
    //Draws moving boid
    g.setColor(a)
    g.fill(new Ellipse2D.Double(newX, newY,  r, r))
    
    //Draws velocity Vector
    g.setColor(Color.WHITE)
    g.drawLine(place.x.toInt, place.y.toInt, (speed.x*15 + place.x).toInt, (speed.y*15 + place.y).toInt )
  }
}