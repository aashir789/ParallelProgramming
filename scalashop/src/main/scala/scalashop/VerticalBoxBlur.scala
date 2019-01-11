package scalashop

import org.scalameter._
import common._

import scala.collection.immutable

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 1
    val width = 32
    val height = 32
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks =32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var currentX = from
    while (currentX < end) {
      var currentY = 0
      while (currentY < src.height) {
        val blurredPixel: RGBA = boxBlurKernel(src, currentX, currentY, radius)
        dst.update(currentX, currentY, blurredPixel)
        currentY += 1
      }
      currentX += 1
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    def createTuple(startVal: Int): (Int, Int) = {
      val endVal = startVal + (src.width / numTasks)
      if (endVal > src.width) (startVal, src.width) else (startVal, endVal)
    }

    if (numTasks <= src.width) {
      val startIndexList = 0 until src.width by (src.width / numTasks)
      val startEndTuples: immutable.IndexedSeq[(Int, Int)] = startIndexList.map(n => createTuple(n))

      val blurTasks = startEndTuples.map(tuple => task {
        blur(src, dst, tuple._1, tuple._2, radius)
      })

      // run blur on final strip if any left
      val finalStrip = startEndTuples.apply(startEndTuples.length - 1)
      if (finalStrip._2 < src.width) {
        blur(src, dst, finalStrip._2, src.width, radius)
      }

      blurTasks.foreach(_.join())
    }
    else {
      blur(src, dst, 0, src.width, radius)
    }
  }

}
