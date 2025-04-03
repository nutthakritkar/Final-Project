import java.awt.image.BufferedImage
import java.awt.Color
import java.io.{File, FileOutputStream, FileInputStream, ObjectOutputStream, ObjectInputStream}
import javax.imageio.ImageIO
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object MosaicRecreationParallel {

  // Cache to store loaded sample images and their average colors
  @volatile var cachedSamples: Option[Seq[(BufferedImage, Color)]] = None
  val cacheFile = new File("image_cache.dat")

  // Get the average color of the image
  def getAverageColor(image: BufferedImage): Color = {
    var r, g, b = 0
    val width = image.getWidth
    val height = image.getHeight

    //Iterate over the pixels get the sum of RGB value
    for (x <- 0 until width; y <- 0 until height) {
      val rgb = image.getRGB(x, y)
      r += (rgb >> 16) & 0xFF
      g += (rgb >> 8) & 0xFF
      b += rgb & 0xFF
    }
    val count = width * height
    new Color(r / count, g / count, b / count) // Compute average color
  }

  // Save the cached color data of sample images into a file
  def saveCache(samples: Seq[(BufferedImage, Color)]): Unit = {
    val output = new ObjectOutputStream(new FileOutputStream(cacheFile))
    output.writeObject(samples.map { case (_, color) => color })
    output.close()
  }

  // Load cached sample image color, if the sample images are loaded
  def loadCache(sampleFolder: String): Option[Seq[(BufferedImage, Color)]] = {
    if (!cacheFile.exists()) return None
    try {
      val input = new ObjectInputStream(new FileInputStream(cacheFile))
      val colors = input.readObject().asInstanceOf[Seq[Color]]
      input.close()
      val imageFiles = new File(sampleFolder).listFiles().filter(_.getName.endsWith(".jpg"))
      if (imageFiles.length != colors.length) return None
      Some(imageFiles.zip(colors).map { case (file, color) =>
        (ImageIO.read(file), color)
      })
    } catch {
      case _: Exception => None
    }
  }

  // Load sample images in batches for better memory management
  def loadSampleImages(sampleFolder: String, batchSize: Int): Future[Seq[(BufferedImage, Color)]] = Future {
    cachedSamples.getOrElse { // If cached is available, don't need to load
      val folder = new File(sampleFolder)
      val imageFiles = folder.listFiles().filter(_.getName.endsWith(".jpg"))

      val allResults = imageFiles.grouped(batchSize).flatMap { batch =>
        val batchFutures = batch.map { file =>
          Future {
            val img = ImageIO.read(file)
            (img, getAverageColor(img)) // Compute and store average color
          }
        }

        // Wait for all future to finish before continuing
        Await.result(Future.sequence(batchFutures), 10.minutes)
      }.toSeq

      saveCache(allResults) // Save cache so images don't have to be loaded again
      cachedSamples = Some(allResults)
      allResults
    }
  }

  // Finds the sample image that is closest to the target color
  def findBestMatch(targetColor: Color, samples: Seq[(BufferedImage, Color)]): BufferedImage = {
    samples.minBy { case (_, sampleColor) =>
      val rDiff = targetColor.getRed - sampleColor.getRed
      val gDiff = targetColor.getGreen - sampleColor.getGreen
      val bDiff = targetColor.getBlue - sampleColor.getBlue
      // Compute using Euclidean distance
      rDiff * rDiff + gDiff * gDiff + bDiff * bDiff
    }._1
  }

  // Create mosaic image by replacing each block with best matching image
  def createMosaic(inputImage: BufferedImage, sampleImages: Seq[(BufferedImage, Color)], blockSize: Int): BufferedImage = {
    val width = inputImage.getWidth
    val height = inputImage.getHeight
    val mosaic = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = mosaic.createGraphics()

    // Procss blocks of image in parallel
    val blockFutures = for {
      x <- 0 until width by blockSize
      y <- 0 until height by blockSize
    } yield Future {
      val subImage = inputImage.getSubimage(x, y, Math.min(blockSize, width - x), Math.min(blockSize, height - y))
      val avgColor = getAverageColor(subImage) // Compute average color
      val bestMatch = findBestMatch(avgColor, sampleImages) // Find best match
      graphics.drawImage(bestMatch, x, y, blockSize, blockSize, null) // Draw best match
    }

    // Wait for all future to finish before continuing
    Await.result(Future.sequence(blockFutures), 10.minutes)
    graphics.dispose() // Clear resources
    mosaic
  }

  def main(args: Array[String]): Unit = {
    val inputPath = "/Users/Pon/Desktop/highdef.jpg" // Change to your image file
    val sampleFolder = "/Users/Pon/Desktop/final project/untitled/images/" // Change to your folder with sample images
    val outputPath = "/Users/Pon/Desktop/mosaic_parallel.jpg"
    val blockSize = 2 // Adjust blocksize as needed
    val batchSize = 10 // Adjust batch size to load in sample images

    val inputImage = ImageIO.read(new File(inputPath))

    // Load sample images from cache or in batches if not loaded
    cachedSamples = loadCache(sampleFolder)
    val sampleImagesFuture = loadSampleImages(sampleFolder, batchSize)
    val sampleImages = Await.result(sampleImagesFuture, 10.minutes)

    val startTime = System.nanoTime()
    val mosaicImage = createMosaic(inputImage, sampleImages, blockSize)
    val endTime = System.nanoTime()

    println(s"Mosaic completed in ${(endTime - startTime) / 1e9d} seconds")
    ImageIO.write(mosaicImage, "jpg", new File(outputPath))
    println(s"Mosaic saved to $outputPath")
  }
}
