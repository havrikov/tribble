package saarland.cispa.se.tribble
package generation

import java.util.{HashSet => JSet}

import scala.collection.JavaConverters
import scala.util.Random

class UtilSpec extends TestSpecification {

  "The choose function" should "work properly" in {
    val seed = Random.nextLong()
    implicit val random = new Random(seed)
    for (i <- 1 to 1000) {
      withClue(s"Test at index $i with seed $seed") {
        val num = 1+random.nextInt(50)
        val set = new JSet(JavaConverters.asJavaCollection(0 to num))
        set should contain(choose(set))
      }
    }
  }

  it should "throw an Exception on an empty set" in {
    an [IllegalArgumentException] should be thrownBy choose(new JSet())(new Random())
  }

}
