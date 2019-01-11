
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    if (clamp(x, 0, src.width) != x) {
      throw new RuntimeException("Input row is out bounds!")
    }

    if (clamp(y, 0, src.height) != y) {
      throw new RuntimeException("Input col is out bounds!")
    }

    val currentRGBA: RGBA = src.apply(x, y)
    var totalR: Int = 0
    var totalG: Int = 0
    var totalB: Int = 0
    var totalA: Int = 0
    var totalNeighbors: Int = 0

    if (radius < 1) {
      return currentRGBA
    }

    // get all neighbors
    var xDist: Int = -radius
    while (xDist <= radius) {
      if (clamp(x + xDist, 0, src.width - 1) == x + xDist) {
        var yDist: Int = -radius
        while (yDist <= radius) {
          if (clamp(y + yDist, 0, src.height - 1) == y + yDist) {
            val RGBANeighbor: RGBA = src.apply(x + xDist, y + yDist)
            totalR += red(RGBANeighbor)
            totalG += green(RGBANeighbor)
            totalB += blue(RGBANeighbor)
            totalA += alpha(RGBANeighbor)
            totalNeighbors += 1
          }
          yDist += 1
        }
      }
      xDist += 1
    }

    val avgR: Int = totalR / totalNeighbors
    val avgG: Int = totalG / totalNeighbors
    val avgB: Int = totalB / totalNeighbors
    val avgA: Int = totalA / totalNeighbors

    rgba(avgR, avgG, avgB, avgA)
  }

}
