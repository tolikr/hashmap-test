package hashmap

import scala.annotation.tailrec
import scala.collection.mutable

trait HashMap[T] {
    def put(key: Int, value: T): Boolean
    def get(key: Int): Option[T]
    def remove(key: Int): Boolean
}

object HashMap {
    def apply[T](initialSize: Int): HashMap[T] = {
        if (initialSize <= 0) {
            throw new IllegalArgumentException("Initial size cannot be zero or negative")
        } else {
            new HashMapImpl[T](initialSize)
        }
    }
}

private class HashMapImpl[T](initialSize: Int)
    extends HashMap[T]
{
    import Space._

    @volatile
    private var hashBucket: mutable.ArrayBuffer[Space] = mutable.ArrayBuffer.tabulate(initialSize)(_ => Free)

    override def put(key: Int, value: T): Boolean = synchronized {
        putElement(key, value, this.hashBucket)
    }

    override def get(key: Int): Option[T] = synchronized {
        getElement(key, this.hashBucket).map(_._2.value)
    }

    override def remove(key: Int): Boolean = synchronized {
        getElement(key, this.hashBucket).map { case (bucketIndex, element) =>
            deletedElement(bucketIndex, element, this.hashBucket)
        }.isDefined
    }

    @tailrec
    private def putElement(key: Int, value: T, hashBucket: mutable.ArrayBuffer[Space], runCount: Int = 0): Boolean = {
        val bucketSize = hashBucket.size
        if (runCount == bucketSize - 1) {
            val hasInserted = putIfMay(0, hashBucket, key, value)
            if (hasInserted) {
                hasInserted
            } else {
                this.hashBucket = reHash(hashBucket)
                putElement(key, value, this.hashBucket)
            }
        } else {
            val bucketIndex = linearProbe(key, runCount, bucketSize)
            val hasInserted = putIfMay(bucketIndex, hashBucket, key, value)
            if (hasInserted) {
                hasInserted
            } else {
                putElement(key, value, hashBucket, runCount + 1)
            }
        }
    }

    private def putIfMay(bucketIndex: Int, hashBucket: mutable.ArrayBuffer[Space], key: Int, value: T): Boolean = {
        hashBucket(bucketIndex) match {
            case Free =>
                putNewElement(bucketIndex, key, value, hashBucket)
                true
            case Element(removed, existingKey, _) if removed || existingKey == key =>
                putNewElement(bucketIndex, key, value, hashBucket)
                true
            case _ =>
                false
        }
    }

    @tailrec
    private def getElement(key: Int, hashBucket: mutable.ArrayBuffer[Space], runCount: Int = 0): Option[(Int, Element)] = {
        val bucketSize = hashBucket.size
        if (runCount == bucketSize - 1) {
            hashBucket(0) match {
                case el@Element(removed, existingKey, _) if !removed && existingKey == key =>
                    Some(0, el)
                case _ => None
            }
        } else {
            val bucketIndex = linearProbe(key, runCount, bucketSize)
            hashBucket(bucketIndex) match {
                case Free => None
                case el@Element(removed, existingKey, _) if !removed && existingKey == key =>
                    Some(bucketIndex, el)
                case _ => getElement(key, hashBucket, runCount + 1)
            }
        }
    }

    private def linearProbe(key: Int, runCount: Int, bucketSize: Int): Int = {
        (key.hashCode() + runCount) % bucketSize
    }

    private def reHash(hashBucket: mutable.ArrayBuffer[Space]): mutable.ArrayBuffer[Space] = {
        val newSize = hashBucket.size * 2
        val newBucket: mutable.ArrayBuffer[Space] = mutable.ArrayBuffer.tabulate(newSize)(_ => Free)
        hashBucket.foreach{
            case Element(removed, key, value) if !removed =>
                putElement(key, value, newBucket)
            case _ => ()
        }
        newBucket
    }

    private def putNewElement(bucketIndex: Int, key: Int, value: T, hashBucket: mutable.ArrayBuffer[Space]): Unit = {
        val newElement = Element(removed = false, key, value)
        hashBucket(bucketIndex) = newElement
    }

    private def deletedElement(bucketIndex: Int, element: Element, hashBucket: mutable.ArrayBuffer[Space]): Unit = {
        hashBucket(bucketIndex) = element.copy(removed = true)
    }

    sealed trait Space
    object Space {
        case object Free extends Space
        case class Element(removed: Boolean, key: Int, value: T) extends Space
    }
}
