package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("secret should return the right number of stars") {
    forAll { (text: String) =>
      val starsOnly = secret(text)
      assert(starsOnly.length == text.length)
      assert(starsOnly.forall(_ == '*'))
    }
  }

  // Test `isValidUsernameCharacter` which checks if a character is suitable for a username.
  // We accept:
  // - lower and upper case letters
  // - digits
  // - special characters: '-' and '_'
  // For example, isValidUsernameCharacter('3') == true
  //              isValidUsernameCharacter('a') == true
  // but          isValidUsernameCharacter('^') == false
  test("isValidUsernameCharacter should accept all valid characters") {
    val valid = ('a' to 'z') ++
      ('A' to 'Z') ++
      ('0' to '9') ++
      List('-', '_')
    valid.map { (char: Char) =>
      assert(isValidUsernameCharacter(char))
    }
  }

  test("isValidUsernameCharacter should not accept some example invalid characters") {
    val invalid = List('%', '+')
    invalid.map { (char: Char) =>
      assert(isValidUsernameCharacter(char) == false)
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

}
