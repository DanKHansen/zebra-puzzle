import ZebraPuzzle.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** @version created manually * */
class ZebraPuzzleTest extends AnyFunSuite with Matchers:
   test("solve the Zebra Puzzle") {
      ZebraPuzzle.solve should be(Solution(waterDrinker = Norwegian, zebraOwner = Japanese))
   }
