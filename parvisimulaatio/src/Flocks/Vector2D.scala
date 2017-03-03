package Flocks

import math.pow
import math.sqrt

/**
 * Describes mathematical 2D vector, that are used in various calculations in
 * Boids class
 */
case class Vector2D(var x: Double, var y: Double) {

  /**
   * Calculates sumvector and returns it
   */
  def + (other: Vector2D):Vector2D = {
    Vector2D(x + other.x, y + other.y)    
  }
  
  //Unit method that changes this vector summing parameter vector
  def += (other: Vector2D):Unit = {
    this.x += other.x
    this.y += other.y
  }

  //returns this vector and parameter vector substraction vector
  def - (other: Vector2D):Vector2D = {
    Vector2D(x - other.x, y - other.y)
  }
  
  //changes this vector length to given amount
  def changeLength(amount: Double) = {
    ((this)/getLength)* amount
  }
  
  //returns length of this vector
  def getLength(): Double = {
    sqrt(pow(this.x, 2) + pow(this.y, 2))
  }

  //returns this vector divided by given Double
  def / (other: Double):Vector2D = {
    return Vector2D(this.x/other, this.y/other)
  }
  
  //returns this vector multiplied by given amount
  def * (other: Double) = {
    Vector2D(this.x*other, this.y*other)
  }
  
  def negate(v: Vector2D): Vector2D = {
    Vector2D(-v.x, -v.y)
  }

    /**
   * Coordinates overlap so the boid appears on other side of the World
   * boud gets as parameter bound of xBound as World width, and yBound as World height
   * @Param xBound			: boundary of x coordinate
   * @Param yBond 			: boundary of y coordinate
   */
  def bound(xBound: Int, yBound: Int) = {
    val newX = 
      if (x >= xBound) 
        x - xBound
      else if (x < 0)
        x + xBound 
      else x
      
    val newY = 
      if (y >= yBound) 
        y - yBound
      else if (y < 0)
        y + yBound
      else y
        
    if (newX != x || newY != y)
      Vector2D(newX, newY)
    else
      this
  }
  
}