package hashmap

import java.util.concurrent.atomic.AtomicReferenceArray

import scala.annotation.tailrec

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

    private var hashBucket: AtomicReferenceArray[Space] = new AtomicReferenceArray[Space](initialSize)

    override def put(key: Int, value: T): Boolean = {
        putElement(Element(key, value), this.hashBucket)
    }

    override def get(key: Int): Option[T] = {
        getElement(key).map(_._2.value)
    }

    override def remove(key: Int): Boolean = {
        getElement(key).map { case (bucketIndex, element) =>
            hashBucket.compareAndSet(bucketIndex, element, Removed)
        }.exists(identity)
    }

    @tailrec
    private def putElement(element: Element, hashBucket: AtomicReferenceArray[Space], runCount: Int = 0): Boolean = {
        val bucketSize = hashBucket.length()
        if (runCount == bucketSize) {
            val hasInserted = putIfMay(0, hashBucket, element)
            if (hasInserted) {
                hasInserted
            } else {
                increaseHashTableSize(element)
            }
        } else {
            val bucketIndex = hashFunction(element.key, runCount, bucketSize)
            val hasInserted = putIfMay(bucketIndex, hashBucket, element)
            if (hasInserted) {
                hasInserted
            } else {
                putElement(element, hashBucket, runCount + 1)
            }
        }
    }

    private def increaseHashTableSize(element: Element): Boolean = synchronized {
        this.hashBucket = reHash(hashBucket)
        putElement(element, this.hashBucket)
    }

    @tailrec
    private def putIfMay(bucketIndex: Int, hashBucket: AtomicReferenceArray[Space], thisElement: Element): Boolean = {
        hashBucket.get(bucketIndex) match {
            case null =>
                if (hashBucket.compareAndSet(bucketIndex, null, thisElement)) {
                    true
                } else {
                    putIfMay(bucketIndex, hashBucket, thisElement)
                }
            case prevEl@Removed =>
                if (hashBucket.compareAndSet(bucketIndex, prevEl, thisElement)) {
                    true
                } else {
                    putIfMay(bucketIndex, hashBucket, thisElement)
                }
            case prevEl@Element(existingKey, _) if existingKey == thisElement.key =>
                if (hashBucket.compareAndSet(bucketIndex, prevEl, thisElement)) {
                    true
                } else {
                    putIfMay(bucketIndex, hashBucket, thisElement)
                }
            case _ =>
                false
        }
    }

    @tailrec
    private def getElement(key: Int, runCount: Int = 0): Option[(Int, Element)] = {
        val bucketSize = hashBucket.length()
        val bucketIndex = if (runCount != bucketSize) {
            hashFunction(key, runCount, bucketSize)
        } else {
            0
        }

        hashBucket.get(bucketIndex) match {
            case el@Element(existingKey, _) if existingKey == key =>
                Some(bucketIndex, el)
            case null =>
                None
            case _ =>
                if (runCount != bucketSize) {
                    getElement(key, runCount + 1)
                } else {
                    None
                }
        }
    }

    private def hashFunction(key: Int, runCount: Int, bucketSize: Int): Int = {
        (key.hashCode() + runCount) % bucketSize
    }

    private def reHash(hashBucket: AtomicReferenceArray[Space]): AtomicReferenceArray[Space] = {
        val newSize = hashBucket.length() * 2
        val newBucket = new AtomicReferenceArray[Space](newSize)
        (0 until hashBucket.length()).foreach(i =>
            hashBucket.get(i) match {
                case el: Element =>
                    putElement(el, newBucket)
                case _ => ()
            }
        )
        newBucket
    }

    sealed trait Space
    object Space {
        case object Removed extends Space
        case class Element(key: Int, value: T) extends Space
    }
}
