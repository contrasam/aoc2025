import scala.io.Source
import scala.annotation.tailrec

@main def day01(): Unit =
  val input = Source.fromFile("inputs/day01.txt").getLines().toSeq
  println(s"Part 1: ${solvePart1(input)}")
  println(s"Part 2: ${solvePart2(input)}")

@tailrec
def solvePart1(lines: Seq[String], currentPos: Int = 50, seenZero: Int = 0): Int =
    lines match
        case head +: tail =>
            val nextPos = rotate(head, currentPos)
            val newSeen = if(nextPos == 0) seenZero + 1 else seenZero
            solvePart1(tail, nextPos, newSeen)
        case _ => seenZero

@tailrec    
def solvePart2(lines: Seq[String], previousPos: Int = 50, seenZero: Int = 0): Int =
    lines match
        case head +: tail =>
            val (nextPos, crossings) = rotateV2(head, previousPos)
            solvePart2(tail, nextPos, seenZero + crossings)
        case _ => seenZero


def ceilRotation(rotated: Int): Int = ((rotated % 100) + 100) % 100

def rotate(rotation: String, currentPos: Int): Int =
    val rotationAmount = rotation.tail.toInt 
    val isLeft = rotation.startsWith("L")
    if isLeft then
        ceilRotation(currentPos - rotationAmount)
    else
        ceilRotation(currentPos + rotationAmount)


def rotateV2(rotation: String, currentPos: Int): (Int, Int) =
    val clicks = rotation.tail.toInt
    val isLeft = rotation.head == 'L'
    val fullSpins = clicks / 100
    val remainingClicks = clicks % 100
    
    val nextPos = if isLeft then 
        ceilRotation(currentPos - clicks)
    else 
        ceilRotation(currentPos + clicks)
    
    val crossesZero = if isLeft then
        currentPos != 0 && currentPos <= remainingClicks
    else
        currentPos + remainingClicks > 99
    
    val totalCrossings = fullSpins + (if crossesZero then 1 else 0)
    (nextPos, totalCrossings)