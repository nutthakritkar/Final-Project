import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

object MosaicRecreation {

  // Function to get the average color of an image
  // Goes through every pixel and compute the average RGB
  def getAverageColor(image: BufferedImage): Color = {
    var r, g, b, count = 0
    val width = image.getWidth
    val height = image.getHeight
    
    // Loop through every pixel in the image
    for (x <- 0 until width; y <- 0 until height) {
      val color = new Color(image.getRGB(x, y))
      r += color.getRed // Add red component of pixel
      g += color.getGreen // Add green component of pixel
      b += color.getBlue // Add blue compoonent of pixel
      count += 1 // Go to next pixel
    }
    //Return colour with the average of red blue and green
    new Color(r / count, g / count, b / count)
  }

  // Function to load sample images and their average colors
  def loadSampleImages(sampleFolder: String): Seq[(BufferedImage, Color)] = {
    val folder = new File(sampleFolder)
    val imageFiles = folder.listFiles().filter(_.getName.endsWith(".jpg"))

    //Read the image and compute average color for each image
    imageFiles.map { file =>
      val img = ImageIO.read(file)
      (img, getAverageColor(img))
    }
  }

  // Function to find the closest matching image based on average color
  // Compute squared Euclidean distance between target color and sample image
  // average color
  def findBestMatch(targetColor: Color, samples: Seq[(BufferedImage, Color)]): BufferedImage = {
    samples.minBy { case (_, sampleColor) =>
      val rDiff = targetColor.getRed - sampleColor.getRed
      val gDiff = targetColor.getGreen - sampleColor.getGreen
      val bDiff = targetColor.getBlue - sampleColor.getBlue
      rDiff * rDiff + gDiff * gDiff + bDiff * bDiff // Compute squared differences
    }._1
  }

  // Function to create the mosaic image
  def createMosaic(inputImage: BufferedImage, sampleImages: Seq[(BufferedImage, Color)], blockSize: Int): BufferedImage = {
    val width = inputImage.getWidth
    val height = inputImage.getHeight
    val mosaic = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = mosaic.createGraphics()

    // Loop over input image in step of blockSize
    for (x <- 0 until width by blockSize; y <- 0 until height by blockSize) {
      // Find best match sample image for current block
      val avgColor = getAverageColor(inputImage.getSubimage(x, y, Math.min(blockSize, width - x), Math.min(blockSize, height - y)))
      val bestMatch = findBestMatch(avgColor, sampleImages)
      // Draw the best match image in current block's position
      graphics.drawImage(bestMatch, x, y, blockSize, blockSize, null)
    }

    graphics.dispose()
    mosaic
  }

  def main(args: Array[String]): Unit = {
    val inputPath = "/Users/Pon/Desktop/highdef.jpg"
    val sampleFolder = "/Users/Pon/Desktop/images/"
    val outputPath = "/Users/Pon/Desktop/mosaic_sequential.jpg"
    val blockSize = 2

    val inputImage = ImageIO.read(new File(inputPath))
    val sampleImages = loadSampleImages(sampleFolder)
    //val mosaicImage = createMosaic(inputImage, sampleImages, blockSize)

    val startTime = System.nanoTime()
    val mosaicImage = createMosaic(inputImage, sampleImages, blockSize)
    val endTime = System.nanoTime()

    val duration = (endTime - startTime) / 1e9d // in seconds
    println(s"Sequential Mosaic completed in $duration seconds")

    ImageIO.write(mosaicImage, "jpg", new File(outputPath))
    println(s"Mosaic saved to $outputPath")
  }
}
