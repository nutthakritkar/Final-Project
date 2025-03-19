import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import java.io.File
import javax.imageio.ImageIO

object ImagePixelator {

  def pixelate(image: BufferedImage, blockSize: Int): BufferedImage = {
    val width = image.getWidth
    val height = image.getHeight
    val pixelated = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = pixelated.createGraphics() // c

    for (x <- 0 until width by blockSize; y <- 0 until height by blockSize) {
      val avgColor = getAverageColor(image, x, y, blockSize)
      graphics.setColor(avgColor)
      graphics.fillRect(x, y, blockSize, blockSize)
    }

    graphics.dispose()
    pixelated
  }

  def getAverageColor(image: BufferedImage, x: Int, y: Int, blockSize: Int): Color = {
    var sumRed, sumGreen, sumBlue, count = 0

    for (i <- 0 until blockSize; j <- 0 until blockSize) {
      if (x + i < image.getWidth && y + j < image.getHeight) {
        val color = new Color(image.getRGB(x + i, y + j))
        sumRed += color.getRed
        sumGreen += color.getGreen
        sumBlue += color.getBlue
        count += 1
      }
    }

    new Color(sumRed / count, sumGreen / count, sumBlue / count)
  }

  def main(args: Array[String]): Unit = {
    val inputFile = new File("/Users/Pon/Desktop/image.jpg")  // Change to your image file
    val outputFile = new File("pixelated.jpg")
    
    

    val image = ImageIO.read(inputFile)
    val pixelatedImage = pixelate(image, blockSize = 10)  // Adjust block size as needed
    ImageIO.write(pixelatedImage, "jpg", outputFile)

    println("Pixelation complete. Saved as pixelated.jpg")
  }
}
