import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object MosaicRecreationParallel {

  // Function to get the average color of an image
  def getAverageColor(image: BufferedImage): Color = {
    var r, g, b = 0
    val width = image.getWidth
    val height = image.getHeight

    // Loop through every pixel in the image
    for (x <- 0 until width; y <- 0 until height) {
      val rgb = image.getRGB(x, y)
      r += (rgb >> 16) & 0xFF // Red component
      g += (rgb >> 8) & 0xFF  // Green component
      b += rgb & 0xFF         // Blue component
    }
    val count = width * height
    new Color(r / count, g / count, b / count) // Return average color
  }

  // Function to load sample images and their average colors in parallel
  def loadSampleImages(sampleFolder: String, batchSize: Int): Future[Seq[(BufferedImage, Color)]] = Future {
    val folder = new File(sampleFolder)
    val imageFiles = folder.listFiles().filter(_.getName.endsWith(".jpg"))

    // Process each image concurrently
    val futures = imageFiles.map { file =>
      Future {
        val img = ImageIO.read(file)
        (img, getAverageColor(img)) // Compute amd store average color
      }
    }

    // Wait for all futures to complete
    val allResults = Future.sequence(futures)
    Await.result(allResults, 10.minutes).toSeq
  }

  // Function to find the closest matching image based on average color
  def findBestMatch(targetColor: Color, samples: Seq[(BufferedImage, Color)]): BufferedImage = {
    samples.minBy { case (_, sampleColor) =>
      val rDiff = targetColor.getRed - sampleColor.getRed
      val gDiff = targetColor.getGreen - sampleColor.getGreen
      val bDiff = targetColor.getBlue - sampleColor.getBlue
      rDiff * rDiff + gDiff * gDiff + bDiff * bDiff // Compute squared differences
    }._1
  }

  // Function to create the mosaic image with parallel block processing
  def createMosaic(inputImage: BufferedImage, sampleImages: Seq[(BufferedImage, Color)], blockSize: Int): BufferedImage = {
    val width = inputImage.getWidth
    val height = inputImage.getHeight
    val mosaic = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = mosaic.createGraphics()

    // Compute each block of input image in parallel
    val blockFutures = for {
      x <- 0 until width by blockSize
      y <- 0 until height by blockSize
    } yield Future {
      // Get sub-image block
      val subImage = inputImage.getSubimage(x, y, Math.min(blockSize, width - x), Math.min(blockSize, height - y))
      val avgColor = getAverageColor(subImage)
      val bestMatch = findBestMatch(avgColor, sampleImages)
      graphics.drawImage(bestMatch, x, y, blockSize, blockSize, null)
    }

    // Wait for all futures to complete
    Await.result(Future.sequence(blockFutures), 10.minutes)
    graphics.dispose()
    mosaic
  }

  def main(args: Array[String]): Unit = {
    val inputPath = "/Users/Pon/Desktop/highdef.jpg"
    val sampleFolder = "/Users/Pon/Desktop/images/"
    val outputPath = "/Users/Pon/Desktop/mosaic_parallel.jpg"
    val blockSize = 2
    val batchSize = 10 // Adjust batch size to process more images concurrently

    val inputImage = ImageIO.read(new File(inputPath))

    // Load sample images concurrently
    val startTime = System.nanoTime()
    val sampleImagesFuture = loadSampleImages(sampleFolder, batchSize)
    val sampleImages = Await.result(sampleImagesFuture, 10.minutes)

    // Create the mosaic image
    val mosaicImage = createMosaic(inputImage, sampleImages, blockSize)

    val endTime = System.nanoTime()
    val duration = (endTime - startTime) / 1e9d // in seconds
    println(s"Mosaic completed in $duration seconds")

    // Save the mosaic image to a file
    ImageIO.write(mosaicImage, "jpg", new File(outputPath))
    println(s"Mosaic saved to $outputPath")
  }
}
