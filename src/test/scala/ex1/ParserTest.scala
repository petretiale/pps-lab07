package ex1


import org.scalatest.matchers.should.Matchers.*
import ex1.Parsers.*

class ParserTest extends org.scalatest.funsuite.AnyFunSuite {

  test("test basic parser"):
    def parser = new BasicParser(Set('a', 'b', 'c'))
    parser.parseAll("aabc".toList) shouldBe true
    parser.parseAll("aabcdc".toList) shouldBe false

  test("test not empty parser"):
    def parserNE = new NonEmptyParser(Set('0', '1'))
    assert(parserNE.parseAll("0101".toList))
    parserNE.parseAll("0101".toList) shouldBe true
    parserNE.parseAll("0123".toList) should not be true
    parserNE.parseAll(List()) shouldBe false

  test("test not two consecutive parser"):
    def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
    parserNTC.parseAll("XYZ".toList) shouldBe true
    parserNTC.parseAll("XYYZ".toList) should not be true
    parserNTC.parseAll("".toList) shouldBe true

  test("testNotTwoConsecutiveParser"):
    def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
    assert(parserNTCNE.parseAll("XYZ".toList))
    parserNTCNE.parseAll("XYZ".toList) shouldBe true
    parserNTCNE.parseAll("XYYZ".toList) should not be true
    parserNTCNE.parseAll("".toList) should not be true

  test("testStringParser"):
    def sparser: Parser[Char] = "abc".charParser
    sparser.parseAll("aabc".toList) shouldBe true
    sparser.parseAll("aabcdc".toList) should not be true
    sparser.parseAll("".toList) shouldBe true
}

//class ParserTests:
//  def parser = new BasicParser(Set('a', 'b', 'c'))
//  // Note NonEmpty being "stacked" on to a concrete class
//  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
//  def parserNE = new NonEmptyParser(Set('0', '1'))
//  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
//  // note we do not need a class name here, we use the structural type
//  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
//  def sparser: Parser[Char] = "abc".charParser // "abc".charParser()

//  @Test
//  def testBasicParser =
//    assertTrue(parser.parseAll("aabc".toList))
//    assertFalse(parser.parseAll("aabcdc".toList))
//    assertTrue(parser.parseAll("".toList))
//
//  @Test
//  def testNotEmptyParser =
//    assertTrue(parserNE.parseAll("0101".toList))
//    assertFalse(parserNE.parseAll("0123".toList))
//    assertFalse(parserNE.parseAll(List()))
//
//  @Test
//  def testNotTwoConsecutiveParser =
//    assertTrue(parserNTC.parseAll("XYZ".toList))
//    assertFalse(parserNTC.parseAll("XYYZ".toList))
//    assertTrue(parserNTC.parseAll("".toList))
//
//  @Test
//  def testNotEmptyAndNotTwoConsecutiveParser =
//    assertTrue(parserNTCNE.parseAll("XYZ".toList))
//    assertFalse(parserNTCNE.parseAll("XYYZ".toList))
//    assertFalse(parserNTCNE.parseAll("".toList))

//  @Test
//  def testStringParser =
//    assertTrue(sparser.parseAll("aabc".toList))
//    assertFalse(sparser.parseAll("aabcdc".toList))
//    assertTrue(sparser.parseAll("".toList))
