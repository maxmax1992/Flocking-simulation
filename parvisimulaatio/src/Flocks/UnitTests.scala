package Flocks
import org.junit.Test
import Flocks._
import org.junit.Assert._

/*
 * Tests various possible bugs, when calculating range of each boid, overlapping technique
 * other possible bugs with grid, etc.
 */
class UnitTests {
  
  @Test def vectorTest1() {
      var a = new Vector2D(10, 10)
      assertEquals("Vector distribution not working", Vector2D(1.0, 1.0), a/10)
      a = Vector2D(1.0, 1.0)
      a += Vector2D(3, 3)
      assertEquals("Vector's += operation fail", Vector2D(4.0, 4.0), a )
      
      a = a - Vector2D(4, 4)
      assertEquals("Vector subtraction not working", Vector2D(0, 0), a)
      a = new Vector2D(10, 10)
      assertEquals("Can't calculate the unit vector of the vector => can't change magnitude of vector",Vector2D(20, 20), a * 2)
    }
  
  
  @Test def testCohension() {
    val grid = new Grid()
    val boid = new Boid(Vector2D(0,0), Vector2D(50,50), 20, grid)
    boid.viewRadius = 200
    val boid2 = new Boid(Vector2D(0, 0), Vector2D(550, 550), 20, grid)
    boid2.viewRadius = 200
    boid.updateCell()
    boid2.updateCell()
    val DELTA = 0.0000001

    val answer: Double =  141.4213562373095
    assertEquals(141.4213562373095 , boid.getRange(boid2), DELTA)
    assertEquals(Vector2D(-50,-50), boid.calculateClosestBoid(boid2))
    
    assertEquals(Vector2D(650, 650), boid2.calculateClosestBoid(boid))
    
    boid.place = Vector2D(550, 50)
    boid2.place = Vector2D(550,550)
    assertEquals(Vector2D(550, -50), boid.calculateClosestBoid(boid2))
    
    
    boid.place = Vector2D(50, 300)
    boid2.place = Vector2D(550, 300)
    assertEquals(Vector2D(-50, 300), boid.calculateClosestBoid(boid2))
   
    
    boid.place = Vector2D(0,0) 
    boid2.place = Vector2D(300,0)
    val boid3 = new Boid(Vector2D(0,0), Vector2D(0,300), 20, grid)
    val boid1 = new Boid(Vector2D(0,0), Vector2D(0,600), 20, grid)
    val boid4 = new Boid(Vector2D(0,0), Vector2D(300, 599), 20, grid)
    val boid5 = new Boid(Vector2D(0,0), Vector2D(599, 0), 20, grid)
    val boid6 = new Boid(Vector2D(0,0), Vector2D(599, 300), 20, grid)
    val boid7 = new Boid(Vector2D(0,0), Vector2D(599, 599), 20, grid)
    val boid8 = new Boid(Vector2D(0,0), Vector2D(300, 300), 20, grid)
    
    boid8.viewRadius = 600
    boid8.updateCell()
    val array : Array[Boid] = Array(boid, boid1, boid2, boid3, boid4, boid5, boid6, boid7)
    for(i <- 0 until array.size) {
      assertEquals("calculation of boid: " + array(i) + "failed", array(i).place, boid8.calculateClosestBoid(array(i)))
    }
    
  }
  
  @Test def testCohension2() = {
    val grid = new Grid()
    val boid = new Boid(Vector2D(0,0), Vector2D(50, 50), 20, grid)
    val boid1 = new Boid(Vector2D(0,0), Vector2D(550, 550), 20, grid)
    boid.updateCell()
    boid1.updateCell()
    boid.viewRadius = 142
    boid1.viewRadius = 142 
    assertEquals(true, grid.getBoids(boid, boid.viewRadius).head == boid1)
    assertEquals(true, grid.getBoids(boid1, boid1.viewRadius).head == boid)
    assertEquals(Vector2D(-50,-50), boid.calculateClosestBoid(boid1))
    assertEquals(Vector2D(650, 650), boid1.calculateClosestBoid(boid))
  }

  @Test def testRange() = {
    val grid =  new Grid()
    val boid1 = new Boid(Vector2D(1,  0), Vector2D(100, 100), 20, grid)
    val boid2 = new Boid(Vector2D(1,  0), Vector2D(500, 500), 20, grid)
    boid1.viewRadius = 300
    assertEquals(false, boid2.getRange(boid1) == boid2.getRangenoBounds(boid1))
    
    
  }
}