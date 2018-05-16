package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {


  test("chars count test") {
    assert(List(('a', 2), ('b', 1), ('c', 1)) === Huffman.times(List('a', 'b', 'a', 'c')))
  }

  test("make ordered list from chars count"){
    assert(List(Leaf('b',1),Leaf('c',1),Leaf('a',2)) === Huffman.makeOrderedLeafList(List(('a', 2), ('b', 1), ('c', 1))))
  }


  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("test that encode and decode secret phrase if identity "){
    assert(Huffman.decode(Huffman.frenchCode, Huffman.encode(Huffman.frenchCode)("huffmanestcool".toList))
      === Huffman.decode(Huffman.frenchCode,Huffman.secret) )
  }

  test("test that quick encode is  identity secrete phrase decode"){
    assert(Huffman.decode(Huffman.frenchCode, Huffman.quickEncode(Huffman.frenchCode)("huffmanestcool".toList))
      === Huffman.decode(Huffman.frenchCode,Huffman.secret) )

  }

}
