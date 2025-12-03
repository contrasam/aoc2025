import scala.io.Source
import scala.annotation.tailrec

@main def day03(): Unit =
  val input = Source.fromFile("inputs/day03.txt").getLines().toList
  val banks = input.map(line =>
    line.split("").flatMap(joltage => joltage.toLongOption).toList
  )
  println(
    s"Part 1: ${banks.map(bank => largestJoltagePossible(bank, 0, 0)).sum}"
  )
  println(
    s"Part 2: ${banks.map(bank => largestJoltagePossibleV2(bank, bank.length - 12)).sum}"
  )

def largestJoltagePossible(
    joltages: List[Long],
    largeSeen: Long,
    maxJoltage: Long
): Long =
  joltages match
    case joltage :: tail =>
      val newMaxJoltage = largeSeen * 10 + joltage
      val newSeen = if joltage > largeSeen then joltage else largeSeen
      largestJoltagePossible(
        tail,
        newSeen,
        if newMaxJoltage > maxJoltage then newMaxJoltage else maxJoltage
      )
    case _ => maxJoltage

def largestJoltagePossibleV2(
    joltages: List[Long],
    removals: Int,
    sequence: List[Long] = Nil
): Long =
  joltages match
    case joltage :: tail =>
      if (removals > 0 && sequence.nonEmpty && joltage > sequence.last) then
        largestJoltagePossibleV2(joltages, removals - 1, sequence.init)
      else largestJoltagePossibleV2(tail, removals, sequence :+ joltage)

    case Nil =>
      if (removals > 0) then
        largestJoltagePossibleV2(Nil, removals - 1, sequence.init)
      else sequence.mkString.toLong
