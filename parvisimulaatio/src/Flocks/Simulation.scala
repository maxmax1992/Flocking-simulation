package Flocks

import java.awt.Color
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.event.ActionListener

import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.GridBagPanel._
import scala.swing.Label
import scala.swing.event._

/*The application that is responsible for drawing panel and GUI
 */
object Simulation extends SimpleSwingApplication {

  //the time is waited to execute each frame
  val speedinMS = 6
  val drawGrid = false
  
  //World, constants. changing not recommded
  val width      = 600
  val height     = 600
  val fullHeight = 622
  
  //grid that we need
  val grid = new Grid()
  
  /**
   * MainFrame is scala-swing's main window representing method
   */  
  def top = new MainFrame {
    
    //title and some constants
    title     = "FlockingSimulation"
    resizable = false
  
    val arena = new Panel {
    
    //preferred size of World
    preferredSize = new Dimension(600, 600)
    
      //main paint function
      override def paintComponent(g: Graphics2D) = {

        //the world drawing is done here
        g.setColor(Color.BLACK)
        g.fillRect(0, 0, 600, 600)
        g.setColor(Color.BLACK)
        

        //for smoother boids movement we turn antialiasing on
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)          

        g.setColor(Color.white)
        World.draw(g)
        g.setColor(Color.black)
        
        //we can debug grid that indicates cells by turning this variable 'drawgrid' to true
        if(drawGrid) {
        grid.draw(g)
        } 
        
      }
    }  
    val vertical = new BoxPanel(Orientation.Horizontal)    

    
    //the UI is done mostly by BoxPanels
    val subPanel = new BoxPanel(Orientation.Vertical) {
      
      contents += new Label("\n")
      contents += new Label("\n")
      contents += new Label("\n")
      contents += new Label("\n")
      contents += new Label("\n")
      contents += new Label("\n")
      
      contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Cohesion")
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Button(" - 50% ")(World.boids.foreach { x => x.cohension =   (x.cohension*1.50)})
        contents += Button(" - 10% ")(World.boids.foreach { x => x.cohension =   (x.cohension*1.10)})
        contents += Button(" - 1% ")(World.boids.foreach { x => x.cohension =   (x.cohension*1.01)})
        contents += Button(" + 1% ")(World.boids.foreach { x => x.cohension =   (x.cohension*0.99)})
        contents += Button(" + 10% ")(World.boids.foreach { x => x.cohension =   (x.cohension*0.9)})
        contents += Button(" + 50% ")(World.boids.foreach { x => x.cohension =   (x.cohension*0.5)})
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Separation")

      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Button(" - 50% ")(World.boids.foreach { x => x.separation =   (x.separation*0.50)})
        contents += Button(" - 10% ")(World.boids.foreach { x => x.separation =   (x.separation*0.90)})
        contents += Button(" - 1% ")(World.boids.foreach { x => x.separation =   (x.separation*0.99)})
        contents += Button(" + 1% ")(World.boids.foreach { x => x.separation =   (x.separation*1.01) })
        contents += Button(" + 10% ")(World.boids.foreach { x => x.separation =   (x.separation*1.10) })
        contents += Button(" + 50% ")(World.boids.foreach { x => x.separation =   (x.separation*1.50) })
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Alignment")
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Button(" - 50% ")(World.boids.foreach { x => x.alignment =  (x.alignment*0.5)})
        contents += Button(" - 10% ")(World.boids.foreach { x => x.alignment =   (x.alignment*0.90)})
        contents += Button(" - 1% ")(World.boids.foreach { x => x.alignment =   (x.alignment*0.99)})
        contents += Button(" + 1% ")(World.boids.foreach { x => x.alignment =   (x.alignment*1.01)})
        contents += Button(" + 10% ")(World.boids.foreach { x => x.alignment =   (x.alignment*1.10)})
        contents += Button(" + 50% ")(World.boids.foreach { x => x.alignment =   (x.alignment*1.50)})
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Total factor adjustment")
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Button(" - 50% ")(World.boids.foreach { x => x.lowerFactors = (x.lowerFactors*1.5)})
        contents += Button(" - 10% ")(World.boids.foreach { x => x.lowerFactors = (x.lowerFactors*1.10)})
        contents += Button(" - 1% ")(World.boids.foreach { x => x.lowerFactors = (x.lowerFactors*1.01)})
        contents += Button(" + 1% ")(World.boids.foreach { x => x.lowerFactors = (x.lowerFactors*0.99)})
        contents += Button(" + 10% ")(World.boids.foreach { x => x.lowerFactors = (x.lowerFactors*0.90)})
        contents += Button(" + 50% ")(World.boids.foreach { x => x.lowerFactors = (x.lowerFactors*0.50)})
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Button("Reset Factors")({ 
          World.boids.foreach(x => {  
            x.cohension = 245;
            x.separation = 2.45;
            x.alignment = 0.1;
            x.lowerFactors = 20;
          })
        })
      }
      contents += new Label("\n")
      contents += new Label("\n")
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("View range (Not recommended over 300)")
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Button("-50")(World.boids.foreach { x => x.viewRadius = math.max(1, x.viewRadius -50)})
        contents += Button("-30")(World.boids.foreach { x => x.viewRadius = math.max(1, x.viewRadius -30)})
        contents += Button("-10")(World.boids.foreach { x => x.viewRadius = math.max(1, x.viewRadius -10)})
        contents += Button("+10")(World.boids.foreach { x => x.viewRadius = math.min(600, x.viewRadius +10)})
        contents += Button("+30")(World.boids.foreach { x => x.viewRadius = math.min(600, x.viewRadius +30)})
        contents += Button("+50")(World.boids.foreach { x => x.viewRadius = math.min(600, x.viewRadius +50)})
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Button("Reset viewRadius")(World.boids.foreach(x => x.viewRadius = 200))
      }
      contents += new Label("\n")
      contents += new Label("\n")
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Boids manipulation") 
      }

      object boids extends TextField { columns = 5} 

      contents += new FlowPanel() {
        contents += new Label("Amount of boids")
        contents += boids 
        listenTo(boids.keys)
        preferredSize = new Dimension(0, 0)
        reactions += {
          case KeyPressed(_, Key.Enter, _, _) => {
            try {
              val amount = boids.text.toInt
              World.setBoidAmount(amount)
              boids.text = ""
            } catch {
              case e: NumberFormatException => {
                Dialog.showMessage(vertical, "Wrong input!", title = "Notification", Dialog.Message.Info)
                boids.text = ""
              }
            }
          }
        }
      }
    }
    
    //main BoxPanel that contains Panel and another boxPanel that is Vertical
      vertical.contents += arena
      vertical.contents += subPanel
    
    
    contents = vertical
    centerOnScreen()

    //After each timer tick we update the World and we repaint arena
    val listener = new ActionListener() {
      def actionPerformed(e : java.awt.event.ActionEvent) = {
        World.step()
        arena.repaint() 
      }  
    }

    //Timer tick by default is executed every 6 millisecounds, though the code executing time is not taken into account 
    val timer = new javax.swing.Timer(Simulation.speedinMS, listener)
    timer.start()    
  }
}
