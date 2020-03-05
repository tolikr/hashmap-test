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

    it should "put 2 elements and don't replace if keys are different" in {
        val hashMap = new HashMapImpl[String](11)

        val (firstKey, firstValue) = (1, "1")
        val (secondKey, secondValue) = (12, "12")

        assert(hashMap.put(firstKey, firstValue))
        assert(hashMap.put(secondKey, secondValue))

        assert(hashMap.get(firstKey).contains(firstValue))
        assert(hashMap.get(secondKey).contains(secondValue))
    }

    it should "put elements to length and get it back" in {
        val size = 11
        val hashMap = new HashMapImpl[String](size)

        (0 until size) foreach( i => assert(hashMap.put(i, i.toString)) )

        (0 until size) foreach(i => assert(hashMap.get(i).contains(i.toString)))
    }
}
