package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }
  
  test("pascal: col=0, row=6") {
    assert(pascal(0,6) === 1)
  }
  
  test("pascal: col=6, row=6") {
    assert(pascal(6,6) === 1)
  }
  
  test("pascal: col=1, row=6") {
    assert(pascal(1,6) === 6)
  }
  
  test("pascal: col=3, row=6") {
    assert(pascal(3,6) === 20)
  }
  
  test("pascal: col=4, row=6") {
    assert(pascal(4,6) === 15)
  }
  
  test("pascal: negitive col") {
     intercept[IllegalArgumentException] {
       pascal(-1, 0)
     }
  }
  
  test("pascal: negitive row") {
     intercept[IllegalArgumentException] {
       pascal(0, -1)
     }
  }
  
  test("pascal: invalid first row col combination") {
     intercept[IllegalArgumentException] {
       pascal(1, 0)
     }
  }
  
  test("pascal: invalid row col combination") {
     intercept[IllegalArgumentException] {
       pascal(2, 1)
     }
  }
}
