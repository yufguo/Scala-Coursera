package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for(ele <- arbitrary[Int]; m <- oneOf(const(empty), genHeap))yield insert(ele, m)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (x: Int, y: Int) =>
    val h = insert(y, insert(x, empty))
    findMin(h) == (x min y)
  }  

  property("gen3") = forAll { (x: Int) => {
    isEmpty(deleteMin(insert(x, empty))) 
   }
  }  
  
  property("gen4") = forAll { (h: H) => 
    def helper(h : H): List[Int] = {
      if(isEmpty(h)) Nil
      else findMin(h) :: helper(deleteMin(h))
    }
    
    val temp = helper(h)
    temp == temp.sorted
  }  
  

  
  property("gen6") = forAll { (h1: H, h2: H) => 
    def helper(h1: H, h2: H, h3: H): Boolean = {
      val min1 = if(isEmpty(h1)) Int.MaxValue else findMin(h1)
      val min2 = if(isEmpty(h2)) Int.MaxValue else findMin(h2)
      val min3 = if(isEmpty(h3)) Int.MaxValue else findMin(h3)
      val min = min3 min min2
      if( ( isEmpty(h1) && (!(isEmpty(h2) && isEmpty(h3))) ) || ((! isEmpty(h1)) && (isEmpty(h2) && isEmpty(h3))) ) false
      else if (min1 != min) return false
      else {
        if(min == min2) helper(deleteMin(h1), deleteMin(h2), h3)
        else helper(deleteMin(h1), h2, deleteMin(h3))
      }
    }
    
    helper(meld(h1, h2), h1, h2)

    
  }  
  
}
