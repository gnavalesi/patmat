package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

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
			assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
		}
	}

	test("convert codetree to codetable") {
		new TestTrees {
			var table = convert(t2)
			var bits = codeBits(table) _
			var encoder = encode(t2) _
			println(bits('a') + " === " + encoder("a".toList))
			println(bits('b') + " === " + encoder("b".toList))
			println(bits('d') + " === " + encoder("d".toList))

			assert(bits('a') === encoder("a".toList))
			assert(bits('b') === encoder("b".toList))
			assert(bits('d') === encoder("d".toList))
		}
	}

	test("encode with codetable equals encode with tree") {
		new TestTrees {
			assert(encode(t2)("abd".toList) === quickEncode(t2)("abd".toList))
		}
	}

}
