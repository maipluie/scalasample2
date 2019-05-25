package test.scala.info.pandanote.test

import org.scalatest.Assertions
import org.scalatest._
import main.scala.info.pandanote.test.ComplexNumber

class ComplexNumberTest extends FlatSpec with Matchers {
  "a" should "123" in {
    val a = "123"
    a should not be("123う")
  }

  "a*a" should "-3+4i" in {
    val a = ComplexNumber(1,2)
    a*a should be(ComplexNumber(-3,4))
  }
}