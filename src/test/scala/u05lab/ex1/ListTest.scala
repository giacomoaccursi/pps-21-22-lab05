package u05lab.ex1

import org.junit.Test
import org.junit.Assert.*

class ListTest {

  val reference = List(1, 2, 3, 4)

  @Test
  def testZipRightWithRecursion(): Unit = {
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), reference.zipRightWithRecursion)
  }

  @Test
  def testPartition(): Unit = {
    assertEquals((List(2, 4), List(1, 3)), reference.partition(_ % 2 == 0))
  }
  
  @Test
  def testSpan(): Unit = {
    assertEquals((List(1), List(2, 3, 4)), reference.span(_ % 2 == 0))
  } 
  
  @Test
  def testReduce(): Unit = {
    assertEquals(10, reference.reduce(_ + _))
  }

  @Test
  def testCollect(): Unit = {
    val incIfEven: PartialFunction[Int, Int] = {
      case n if n % 2 == 0 => n + 1
    }
    assertEquals(List(3, 5), reference.collect(incIfEven))
  }

}
