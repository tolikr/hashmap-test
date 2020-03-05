package hashmap

import scala.annotation.tailrec
import scala.collection.mutable

trait HashMap[T] {
    def put(key: Int, value: T): Boolean
    def get(key: Int): Option[T]
    def remove(key: Int): Boolean
}

class HashMapImpl[T](initialSize: Int)
    extends HashMap[T]
{
    import Space._

    if (initialSize <= 0) throw new IllegalArgumentException("Initial size cannot be zero or negative")

    private var hashBucket: mutable.ArrayBuffer[Space] = mutable.ArrayBuffer.tabulate(11)(_ => Free)

    override def put(key: Int, value: T): Boolean = {
        putElement(key, value)
    }

    override def get(key: Int): Option[T] = {
        getElement(key).map(_._2.value)
    }

    override def remove(key: Int): Boolean = {
        getElement(key).map { case (bucketIndex, element) =>
            deletedElement(bucketIndex, element)
        }.isDefined
    }

    @tailrec
    private def putElement(key: Int, value: T, runCount: Int = 0): Boolean = {
        val bucketSize = size()
        if (runCount == bucketSize - 1) {
            putIfMay(0, key, value)
        } else if (runCount >= bucketSize) {
            false
        } else {
            val bucketIndex = linearProbe(key, runCount, bucketSize)
            val hasInserted = putIfMay(bucketIndex, key, value)
            if (hasInserted) {
                hasInserted
            } else {
                putElement(key, value, runCount + 1)
            }
        }
    }

    private def putIfMay(bucketIndex: Int, key: Int, value: T): Boolean = {
        hashBucket(bucketIndex) match {
            case Free =>
                putNewElement(bucketIndex, key, value)
                true
            case Element(removed, existingKey, _) if removed || existingKey == key =>
                putNewElement(bucketIndex, key, value)
                true
            case _ =>
                false
        }
    }

    @tailrec
    private def getElement(key: Int, runCount: Int = 0): Option[(Int, Element)] = {
        val bucketSize = size()
        if (runCount == bucketSize - 1) {
            hashBucket(0) match {
                case el@Element(removed, existingKey, _) if !removed && existingKey == key =>
                    Some(0, el)
                case _ => None
            }
        } else if (runCount >= bucketSize) {
            None
        }else {
            val bucketIndex = linearProbe(key, runCount, bucketSize)
            hashBucket(bucketIndex) match {
                case Free => None
                case el@Element(removed, existingKey, _) if !removed && existingKey == key =>
                    Some(bucketIndex, el)
                case _ => getElement(key, runCount + 1)
            }
        }
    }

    private def linearProbe(key: Int, runCount: Int, bucketSize: Int): Int = {
        (key.hashCode() + runCount) % bucketSize
    }

    private def putNewElement(bucketIndex: Int, key: Int, value: T): Unit = {
        val newElement = Element(removed = false, key, value)
        hashBucket = hashBucket.updated(bucketIndex, newElement)
    }

    private def deletedElement(bucketIndex: Int, element: Element): Unit = {
        hashBucket = hashBucket.updated(bucketIndex, element.copy(removed = true))
    }

    private def size(): Int = hashBucket.size

    sealed trait Space
    object Space {
        case object Free extends Space
        case class Element(removed: Boolean, key: Int, value: T) extends Space
    }

    override def toString: String = hashBucket.toString()
}
