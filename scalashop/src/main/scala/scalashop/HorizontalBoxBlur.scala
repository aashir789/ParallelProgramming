package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._
import scalashop.VerticalBoxBlur.blur

import scala.collection.immutable

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 32
    val height = 32
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
    * starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each row, `blur` traverses the pixels by going from left to right.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var currentY = from
    while (currentY < end) {
      var currentX = 0
      while (currentX < src.width) {
        val blurredPixel: RGBA = boxBlurKernel(src, currentX, currentY, radius)
        dst.update(currentX, currentY, blurredPixel)
        currentX += 1
      }
      currentY += 1
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * rows.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    def createTuple(startVal: Int): (Int, Int) = {
      val endVal = startVal + (src.height / numTasks)
      if (endVal > src.height) (startVal, src.height) else (startVal, endVal)
    }

    if (numTasks <= src.height) {
      val startIndexList = 0 until src.height by (src.height / numTasks)
      val startEndTuples = startIndexList.map(n => createTuple(n))
      val blurTasks = startEndTuples.map(tuple => task {
        blur(src, dst, tuple._1, tuple._2, radius)
      })

      // run blur on final strip if any left
      val finalStrip = startEndTuples.apply(startEndTuples.length - 1)
      if (finalStrip._2 < src.height) {
        blur(src, dst, finalStrip._2, src.height, radius)
      }

      blurTasks.foreach(_.join())
    }
    else {
      blur(src, dst, 0, src.height, radius)
    }
  }

}
