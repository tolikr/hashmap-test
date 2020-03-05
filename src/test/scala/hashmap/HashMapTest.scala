package hashmap

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HashMapTest
    extends AnyFlatSpec
        with Matchers
{
    "HashMapTest" should "not init with invalid size" in {
        assertThrows[IllegalArgumentException](new HashMapImpl[String](0))
        assertThrows[IllegalArgumentException](new HashMapImpl[String](-10))
    }

    it should "add element in hashmap" in {
        val hashMap = new HashMapImpl[String](11)

        assert(hashMap.put(1, "1"))
    }

    it should "put and get element" in {
        val hashMap = new HashMapImpl[String](11)
        val (key , value) = (1, "1")

        assert(hashMap.put(key, value))
        assert(hashMap.get(key).contains(value))
    }

    it should "put and remove" in {
        val hashMap = new HashMapImpl[String](11)
        val (key , value) = (1, "1")

        assert(hashMap.put(key, value))
        assert(hashMap.get(key).contains(value))

        assert(hashMap.remove(key))
        assert(hashMap.get(key).isEmpty)
    }

    it should "replace elements if keys is the same" in {
        val hashMap = new HashMapImpl[String](11)
        val (firstKey, firstValue) = (1, "1")
        val secondValue = "12"

        assert(hashMap.put(firstKey, firstValue))
        assert(hashMap.put(firstKey, secondValue))

        assert(hashMap.get(firstKey).contains(secondValue))
    }

    it should "delete element only once" in {
        val hashMap = new HashMapImpl[String](11)
        val (key , value) = (1, "1")

        assert(hashMap.put(key, value))
        assert(hashMap.get(key).contains(value))

        assert(hashMap.remove(key))
        assert(!hashMap.remove(key))
    }

    it should "put 2 elements and don't replace if keys are different" in {
        val hashMap = new HashMapImpl[String](11)

        val (firstKey, firstValue) = (1, "1")
        val (secondKey, secondValue) = (12, "12")

        assert(hashMap.put(firstKey, firstValue))
        assert(hashMap.put(secondKey, secondValue))

        assert(hashMap.get(firstKey).contains(firstValue))
        assert(hashMap.get(secondKey).contains(secondValue))
    }

    it should "put elements to length, get it back, remove" in {
        val size = 11
        val hashMap = new HashMapImpl[String](size)
        val random = scala.util.Random

        val testedSequence = (1 to size * 2).map(_ => random.nextInt(1000)).distinct.take(size - 1)

        testedSequence foreach(i => assert(hashMap.put(i, i.toString)))
        testedSequence foreach(i => assert(hashMap.get(i).contains(i.toString)))
        testedSequence foreach(i => assert(hashMap.remove(i)))
    }

    it should "put elements more than initial size" in {
        val initialSize = 1
        val hashMap = new HashMapImpl[String](initialSize)

        val size = initialSize + 20
        val random = scala.util.Random

        val testedSequence = (1 to size).map(_ => random.nextInt(1000)).distinct.take(size - 1)

        testedSequence foreach(i => assert(hashMap.put(i, i.toString)))
        testedSequence foreach(i => assert(hashMap.get(i).contains(i.toString)))
        testedSequence foreach(i => assert(hashMap.remove(i)))
    }

    it should "add all elements in some thread then get it all" in {
        import scala.concurrent.{Await, ExecutionContext, Future}
        import scala.concurrent.duration._

        implicit val ec: ExecutionContext = ExecutionContext.global

        val initialSize = 3000
        val hashMap = new HashMapImpl[String](initialSize)

        val size = initialSize + 2000
        val random = scala.util.Random

        val testedSequence = (1 to size).map(_ => random.nextInt(1000)).distinct.take(size - 1)

        val pushResults: Seq[Boolean] = Await.result(
            Future.traverse(testedSequence)(i => Future(hashMap.put(i, i.toString)))
            , 1.seconds
        )
        assert(pushResults.forall(identity))

        val checkOnExistenceResults: Seq[Boolean] = Await.result(
            Future.traverse(testedSequence)(i => Future(hashMap.get(i).contains(i.toString))),
            1.seconds
        )
        assert(checkOnExistenceResults.forall(identity))
    }
}
