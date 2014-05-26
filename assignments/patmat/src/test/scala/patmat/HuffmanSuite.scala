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

    val single = Leaf('a',2) :: Nil
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

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val combined = combine(leaflist)
    println(combined)
    assert(combined === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val encoded = encode(t1)("ab".toList)
      val decoded = decode(t1, encoded)
      println("encoded: "+encoded)
      println("decoded: "+decoded)
      assert(decoded === "ab".toList)
    }
  }

  ////////////////

  test("times") {
    new TestTrees {
      val d = times('a' :: 'b' :: 'a' :: Nil)
      println(d.mkString("[",",","]"))
      assert(d == ('a',2)::('b',1)::Nil)
    }
  }

  test("singleton") {
    new TestTrees {
      assert(singleton(single))
      assert(singleton(t1::Nil))
      assert(!singleton(Nil))
    }
  }

  test("make tree") {
    new TestTrees {
      val chars: List[Char] = string2Chars("dabaaacc")
      val codeTree: CodeTree = createCodeTree(chars)
      println(codeTree)

    }
  }

  test("decode secret") {
    new TestTrees {
      println(decodedSecret)
    }
  }

  test("encode a") {
    new TestTrees {
      val encoded = encode(t1)("abba".toList)
      val decoded = decode(t1, encoded)
      println("encoded: "+encoded)
      println("decoded: "+decoded)
      assert(decoded === "abba".toList)
    }
  }

  test("convert") {
    new TestTrees {
      val converted = convert(t1)
      println("converted: "+converted)
      //assert(converted === "abba".toList)
    }
  }

  test("quickEncode") {
    new TestTrees {
      quickEncode(frenchCode)(string2Chars("HelloWorld!"))

    }
  }

}
