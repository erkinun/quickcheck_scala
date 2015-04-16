package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    isEmpty <- arbitrary[Boolean]
    emp <- empty
    hp <- if (isEmpty) emp else insert(i, emp)
    gH <- oneOf(const(hp), genHeap)
  } yield gH

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


}
