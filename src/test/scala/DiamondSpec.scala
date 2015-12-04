import org.scalacheck.Gen

class DiamondSpec extends UnitSpec {

  val inputChar = Gen.alphaUpperChar

  "A diamond generator" - {
    "should produce some lines" in {
      forAll (inputChar) { c => assert(DiamondGenerator.diamondLines(c).nonEmpty) }
    }

    "produces an odd number lines" in {
      forAll (inputChar) { c => assert(DiamondGenerator.diamondLines(c).size %2 != 0) }
    }

    "number of lines" in {
      forAll (inputChar) { c => assert(DiamondGenerator.diamondLines(c).size == Diamond.numberOfLines(c)) }
    }

    "squareness" in {
      forAll (inputChar) { c => assert(DiamondGenerator.diamondLines(c) forall {_.length == Diamond.numberOfLines(c)}) }
    }
  }
}

object DiamondGenerator {

  def diamondLines(character: Char) : Vector[String] = {
    Diamond.diamond(character).lines.toVector
  }
}

object Diamond {
  def diamond(c: Char) : String = {
    val diamondSize = numberOfLines(c)
    ("A" * diamondSize + "\n") * diamondSize
  }

  def numberOfLines(c: Char) = ((2 * ord(c)) + 1)

  def ord(c: Char) : Int = c - 'A'
}