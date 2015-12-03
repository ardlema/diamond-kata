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
      forAll (inputChar) { c => assert(DiamondGenerator.diamondLines(c).size == 2 * Diamond.ord(c) + 1 ) }
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
    "A\n" * ((2 * ord(c)) + 1)
  }

  def ord(c: Char) : Int = c - 'A'
}