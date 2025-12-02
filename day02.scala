
import scala.io.Source
import scala.compiletime.ops.double
import scala.util.boundary, boundary.break

@main def day02(): Unit =
  val input = Source.fromFile("inputs/day02.txt").getLines().toList
  val ranges = input.head.split(",")
  val invalidProductIds = ranges.map(parseProductIdRange).flatMap(getInvalidProducIdsInRange)
  val invalidProductIdsV2 = ranges.map(parseProductIdRange).flatMap(getInvalidProducIdsInRangeV2)
  println(s"Part 1: ${invalidProductIds.sum}")
  println(s"Part 1: ${invalidProductIdsV2.sum}")

case class ProductIdRange(low: Long, high: Long)

def parseProductIdRange(range: String): ProductIdRange =
    val Array(low, high) = range.split("-")
    ProductIdRange(low.toLong, high.toLong)

def isWithinProductIdRange(productId: Int, productIdRange: ProductIdRange): Boolean =
    (productIdRange.low to productIdRange.high).contains(productId)

def getInvalidProducIdsInRange(productIdRange: ProductIdRange): Set[Long] =
    (productIdRange.low to productIdRange.high).toSet.filter(isInvalidId)

def getInvalidProducIdsInRangeV2(productIdRange: ProductIdRange): Set[Long] =
    (productIdRange.low to productIdRange.high).toSet.filter(isInvalidIdV2)

def isInvalidId(productId: Long): Boolean =
    val asString = productId.toString()
    if asString.length() % 2 == 0 then
        val (left, right) = asString.splitAt(asString.length() / 2)
        left.equals(right)
    else
        false

def isInvalidIdV2(productId: Long): Boolean =
  val productIdAsStr = productId.toString()
  val n = productIdAsStr.length
  (1 to n / 2).exists: i =>
    if n % i == 0 then
      val unit = productIdAsStr.take(i)
      unit * (n / i) == productIdAsStr
    else 
      false
