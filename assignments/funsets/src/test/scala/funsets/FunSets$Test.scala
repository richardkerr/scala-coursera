package funsets

import org.scalatest._
import org.scalatest.matchers.ClassicMatchers
import FunSets._

/**
 * Created by rocky on 12/05/2014.
 */
class FunSets$Test extends FlatSpec with ClassicMatchers {
  val s3 = singletonSet(3)

  "A singleton" should "contain the element" in {
    assert(contains(s3, 3))
  }

  it should "not contain anything else" in {
    assert(!contains(s3, 5))
    assert(!contains(s3, 2))
  }

  it should "print with print" in {
    printSet(s3)
  }

  val threeToSix = (x: Int) => {
    if(x>=3 && x<=6) true
    else false
  }

  val fiveToEight = (x: Int) => {
    if(x>=5 && x<=8) true
    else false
  }

  "Map" should "map it's elements according to the parameter" in {
    val mapped = map(threeToSix, _ * 2)
    val expected = (x: Int) => {
      if(x == 6 || x == 8 || x == 10 || x == 12) true
      else false
    }

    assert(forall(mapped, expected))
    printSet(mapped)
  }

  it should "perform correctly around its upper bound of 1000" in {
    val upper: Set = (x: Int) => {
      if(x == 1 || x == 3 || x == 6 || x == 1000) true
      else false
    }

    val expected: Set = (x: Int) => {
      if(x == 0 || x == 2 || x == 5 || x == 999) true
      else false
    }

    val mapped = map(upper, _ -1)
    assert(forall(mapped, expected))
    printSet(mapped)
  }

  "Exists" should "return true when any of the parameters is present" in {
    val existss = exists(threeToSix, x => {x==4 || x == 7})
    assert(existss)
  }

  it should "return false when none of the parameters are present" in {
    val existss = exists(threeToSix, x => {x==2 || x == 7})
    assert(!existss)
  }

  "forall" should "return true when the set is fully statisfied" in {
    assert(forall(threeToSix, x => {x == 3 || x == 4 || x == 5 || x == 6}))
  }

  it should "return false when the set is not fully satisfied" in {
    assert(!forall(threeToSix, x => {x == 4}))
    assert(!forall(threeToSix, x => {x == 4 || x == 5}))
  }

  "Two sets" should "contain both sets of elements when unioned" in {
    val unioned = union(threeToSix, fiveToEight)
    val expected = (x: Int) => {
      if (x>=3 && x<=8) true
      else false
    }
    assert(forall(unioned, expected))
    printSet(unioned)
  }

  it should "contain the overlap when intersected" in {
    val intersected = intersect(threeToSix, fiveToEight)
    val expected = (x: Int) => {
      if (x>=5 && x<=6) true
      else false
    }
    printSet(intersected)
    assert(forall(intersected, expected))
  }

  it should "contain the contents of the first set that are not in the second when diff" in {
    val dif = diff(threeToSix,fiveToEight)
    val expected = (x: Int) => {
      if (x>=3 && x<=4) true
      else false
    }
    assert(forall(dif, expected))
    printSet(dif)
  }

  it should "contain the intersection when a filter is applied" in {
    val filtered = filter(threeToSix, fiveToEight)
    val expected = (x: Int) => {
      if (x>=5 && x<=6) true
      else false
    }
    assert(forall(filtered, expected))
    printSet(filtered)
  }




}
