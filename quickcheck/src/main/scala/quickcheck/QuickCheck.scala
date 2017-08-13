package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    findMin(insert(a, empty)) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val heap = insert(b, insert(a, insert(a, empty)))
    findMin(heap) == Math.min(a, b)
  }

  property("delete") = forAll { (a: Int) =>
    val heap = deleteMin(insert(a, empty))
    isEmpty(heap)
  }

  property("delete2") = forAll { (a: Int, b:Int) =>
    val heap = deleteMin(deleteMin(insert(b, insert(a, empty))))
    isEmpty(heap)
  }



  property("deleteFromMeld") = forAll { (a: Int, b:Int, c:Int) =>
    val h1 = (insert(a, insert(b, empty)))
    val h2 = insert(c, empty)
    findMin(deleteMin(meld(h1,h2))) == List(a,b,c).sorted.apply(1)
  }

  property("sorted") = forAll { h:H =>
    def sorted(h:H):List[Int] = {
      if (isEmpty(h)) Nil
      else findMin(h)::sorted(deleteMin(h))
    }
    val lst = sorted(h)
    lst.sorted == lst
  }

  property("meld") = forAll { (h1:H, h2:H) =>
    val minH1 = findMin(h1)
    val minH2 = findMin(h2)
    val minMeld = findMin(meld(h1, h2))
    List(minH1, minH2).contains(minMeld)
  }



  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
