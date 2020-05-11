package week_12.funsets

import org.junit._
import week_12.funsets.FunSets._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s0 = singletonSet(0)
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = union(s0, s1)

  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(! contains(s2, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersec 1`: Unit = {
    new TestSets {
      val s = intersect(s1, s4)
      assert(contains(s, 1), "intersect 1")
      assert(!contains(s, 4), "intersect 4")
      assert(!contains(s, 0), "intersect 0")
    }
  }

  @Test def `diff 1`: Unit = {
    new TestSets {
      val s = FunSets.diff(s1, s4)
      assert(!contains(s, 1), "diff 1")
      assert(!contains(s, 2), "diff 2")
      assert(!contains(s, 0), "diff 0")
    }
  }

  @Test def `diff 2`: Unit = {
    new TestSets {
      val s = FunSets.diff(s4, s1)
      assert(!contains(s, 1), "diff 1")
      assert(!contains(s, 2), "diff 2")
      assert(contains(s, 0), "diff 0")
    }
  }

  @Test def `filter 1`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val w = filter(s, (n: Int) => n % 2 == 0)
      assert(!contains(w, 1), "filter 1")
      assert(contains(w, 2), "filter 2")
      assert(!contains(w, 0), "filter 0")
    }
  }

  @Test def test_forall: Unit = {
    new TestSets {
      val s = (x: Int) => List(1,3,4,5,7,1000).contains(x)
      assert(forall(s, (x: Int) => x > 0))
      assert(! forall(s, (x: Int) => x < 0))
    }
  }

  @Test def test_map: Unit = {
    new TestSets {
      val s = (x: Int) => List(1,3,4,5,7,1000).contains(x)
      val smap = map(s, (x: Int) => x - 1)
      val rng = (-10 to 1010).filter(smap).toList
      assert(List(0,2,3,4,6,999) == rng, rng)
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
