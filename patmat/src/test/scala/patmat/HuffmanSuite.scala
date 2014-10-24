package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  test("times") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }
  
  test("times - empty list") {
    assert(times(Nil) === Nil)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("combine list of leafs into right place") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 3), Leaf('x', 4), Leaf('y', 7))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',3),Leaf('t',3),List('e', 't'),6), Leaf('y', 7)))
  }
  
  test("combine large list of leafs into right place") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 3), Leaf('x', 4), Leaf('y', 7), Leaf('r', 8))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',3),Leaf('t',3),List('e', 't'),6), Leaf('y', 7), Leaf('r', 8)))
  }
  
  test("combine 2 leafs") {
    val leafList = List(Leaf('e', 1), Leaf('t', 2)) 
    assert(combine(leafList) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
  }
  
  test("combine a leaf") {
    val leafList = List(Leaf('e', 1)) 
    assert(combine(leafList) === leafList)
  }
  
  test("combine nil") {
    assert(combine(Nil) === Nil)
  }
  
  test("convert: code table is created correctly") {
    val codeTree = Fork(Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2), Leaf('d', 3), List('a', 'b', 'd'), 5)
    assert(convert(codeTree) === List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
