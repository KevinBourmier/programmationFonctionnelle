package example

import org.scalatest.funsuite.AnyFunSuite

class HelloSpec extends AnyFunSuite {

  test("The Hello object should say hello") {
    assert(Hello.greeting === "hello")
  }

  test("Hello size is equals to 5") {
    assert(Hello.greeting.size === 5)
  }

  test(
    """ "Hello"(6) should throw a "java.lang.StringIndexOutOfBoundsException" """
  ) {
    assertThrows[java.lang.StringIndexOutOfBoundsException]("Hello" (6))
  }

}
