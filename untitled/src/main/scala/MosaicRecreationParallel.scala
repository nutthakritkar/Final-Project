import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.concurrent._
import ExecutionContext.Implicits.global

object MosaicRecreationParallel {

  // Function to get the average color of an image
  def getAverageColor(image: BufferedImage): Color = {
    var r, g, b, count = 0
    val width = image.getWidth
    val height = image.getHeight
    
    // Go through all pixels to calculate average color
    for (x <- 0 until width; y <- 0 until height) {
      val color = new Color(image.getRGB(x, y)) // Get rgb value of oixel
      r += color.getRed //add total red value
      g += color.getGreen //add total green value
      b += color.getBlue //add total blue value
      count += 1 // Move to the next pixel
    }
    new Color(r / count, g / count, b / count)
  }

  // Function to load sample images and their average colors
  def loadSampleImages(sampleFolder: String): Seq[(BufferedImage, Color)] = {
    val folder = new File(sampleFolder)
    // Get all JPG from folder
    val imageFiles = folder.listFiles().filter(_.getName.endsWith(".jpg"))

    // Map each image to a tuple of the image and the average color
    imageFiles.map { file =>
      val img = ImageIO.read(file) // Read image from file
      (img, getAverageColor(img)) // Return image and average color
    }
  }

  // Function to find the closest matching image based on average color
  def findBestMatch(targetColor: Color, samples: Seq[(BufferedImage, Color)]): BufferedImage = {
    //use squared Euclidean distance to find smallest color difference
    samples.minBy { case (_, sampleColor) =>
      val rDiff = targetColor.getRed - sampleColor.getRed
      val gDiff = targetColor.getGreen - sampleColor.getGreen
      val bDiff = targetColor.getBlue - sampleColor.getBlue
      rDiff * rDiff + gDiff * gDiff + bDiff * bDiff // Squared distance
    }._1
  }

  // Function to create the mosaic image in parallel using Future
  def createMosaicParallel(inputImage: BufferedImage, sampleImages: Seq[(BufferedImage, Color)], blockSize: Int): BufferedImage = {
    val width = inputImage.getWidth
    val height = inputImage.getHeight
    val mosaic = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = mosaic.createGraphics() // Graphics object for drawing the mosaic image

    // Create future for each block to run in parallel
    val futures = for (x <- 0 until width by blockSize; y <- 0 until height by blockSize) yield {
      Future {
        // Get average color of current block of pixels
        val avgColor = getAverageColor(inputImage.getSubimage(x, y, Math.min(blockSize, width - x), Math.min(blockSize, height - y)))
        // Find best matching image for average color
        val bestMatch = findBestMatch(avgColor, sampleImages)
        graphics.drawImage(bestMatch, x, y, blockSize, blockSize, null)
      }
    }

    // Wait for all futures to complete
    Await.result(Future.sequence(futures), scala.concurrent.duration.Duration.Inf)

    graphics.dispose() // Clean up the resources
    mosaic
  }

  def main(args: Array[String]): Unit = {
    val inputPath = "/Users/Pon/Desktop/highdef.jpg"
    val sampleFolder = "/Users/Pon/Desktop/images/"
    val outputPath = "/Users/Pon/Desktop/mosaic_sequential.jpg"
    val blockSize = 5

    val inputImage = ImageIO.read(new File(inputPath))
    val sampleImages = loadSampleImages(sampleFolder)

    val startTime = System.nanoTime()
    val mosaicImage = createMosaicParallel(inputImage, sampleImages, blockSize)
    val endTime = System.nanoTime()

    val duration = (endTime - startTime) / 1e9d // in seconds
    println(s"Sequential Mosaic completed in $duration seconds")

    ImageIO.write(mosaicImage, "jpg", new File(outputPath))
    println(s"Mosaic saved to $outputPath")
  }
}
