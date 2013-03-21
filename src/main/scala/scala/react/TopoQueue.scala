package scala.react
import java.util.Arrays

abstract class PropQueue[A >: Null: Manifest] {
  def clear()
  def dequeue(): A
  def isEmpty: Boolean
  def +=(elem: A)
  def reinsert(elem: A)
}

/**
 * A slightly specialized priority queue.
 */
abstract class PriorityQueue[A >: Null: Manifest] extends PropQueue[A] {

  private var array = new Array[A](16)
  private var size0 = 1 // array(0) unused

  protected def ensureSize(n: Int) {
    if (n > array.length) {
      var newsize = array.length * 2
      while (n > newsize)
        newsize = newsize * 2

      val newar = new Array[A](newsize)
      compat.Platform.arraycopy(array, 0, newar, 0, size0)
      array = newar
    }
  }

  protected def swap(a: Int, b: Int) {
    val h = array(a)
    array(a) = array(b)
    array(b) = h
  }

  protected def fixUp(m: Int): Unit = {
    val as = array
    var k = m
    while (k > 1 && priority(as(k / 2)) > priority(as(k))) {
      swap(k, k / 2)
      k = k / 2
    }
  }

  protected def fixDown(m: Int, n: Int): Unit = {
    val as = array
    var k = m
    while (n >= 2 * k) {
      var j = 2 * k
      if (j < n && priority(as(j)) > priority(as(j + 1)))
        j += 1
      if (priority(as(k)) <= priority(as(j)))
        return
      else {
        swap(k, j)
        k = j
      }
    }
  }

  def +=(elem: A) {
    ensureSize(size0 * 3/2 + 1)
    array(size0) = elem
    fixUp(size0)
    size0 += 1
  }

  def dequeue(): A = if (size0 > 1) {
    size0 -= 1
    val res = array(1)
    array(1) = array(size0)
    array(size0) = null // need to clear, don't want to keep nodes alive longer than necessary
    fixDown(1, size0 - 1)
    res
  } else
    throw new NoSuchElementException("no element to remove from heap")

  def priority(a: A): Int

  def isEmpty = size0 == 1
  def clear() { size0 = 1 }

  /*private def remove(elem: A) {
    var i = 1
    while(i < size0) {
      if (array(i) == elem) {
        size0 -= 1
        array(i) = null
        if(i == size0) {
          return
        } else {
          array(i) = null
        }
        compat.Platform.arraycopy(array, i+1, array, i, size0-i)

      }
      return
    }
  }*/

  private def indexOf(elem: A): Int = {
    var i = 1
    while(i < size0) {
      if (array(i) == elem) return i
      i += 1
    }
    -1
  }

  def reinsert(elem: A) {
    //remove(elem)
    val idx = indexOf(elem)
    if(idx == -1) this += elem
    else {
      fixDown(idx, size0-1)
    }

  }

  override def toString() =
    "PrioQueue" + array.mkString("[", ",", "]")
}

/**
 * A fast, special purpose priority queue. Priorities are topological depth values.
 * This queue assumes that between initialization and `clear` calls, elements are inserted with
 * monotonically increasing depths as it happens during topologically ordered graph traversal
 * (it might and probaly will crash otherwise).
 */
abstract class TopoQueue[A >: Null: Manifest] extends PropQueue[A]  {
  private var curDepth, curIndex, maxDepth = 0
  private var sizes: Array[Int] = _
  private var levels: Array[Array[A]] = _
  private var maxLevelSize = 0
  private var maxLevel: Array[A] = _

  compact()

  def compact() {
    sizes = Array.fill(initDepth)(0)
    levels = Array.fill(initDepth) { new Array[A](initWidth) }
    maxLevel = new Array[A](initWidth)
  }

  def initDepth = 2
  def initWidth = 2

  /**
   * The depth value of the given element. To be implemented by subclasses.
   */
  def depth(a: A): Int

  def clear() {
    assert(isEmpty)
    curDepth = 0
    curIndex = 0
    maxDepth = 0
    maxLevelSize = 0
    // it should be slightly faster to do this here than to adapt sizes during traversal:
    Arrays.fill(sizes, 0)
  }

  def +=(a: A) {
    val d = depth(a)
    assert(d >= curDepth)
    if (d < Int.MaxValue) {
      ensureDepthCapacity(d)
      val l = ensureLevelCapacity(d)
      l(sizes(d)) = a
      sizes(d) += 1
      if (maxDepth < d) maxDepth = d
    } else {
      val l = ensureLevelCapacity(d)
      l(maxLevelSize) = a
      maxLevelSize += 1
    }
  }

  /**
   * Removes the element with the least depth and makes sure that this queue
   * does not hold on to its reference.
   */
  def dequeue(): A = {
    assert(!isEmpty)
    while (curDepth < sizes.length && curIndex >= sizes(curDepth)) {
      curDepth += 1
      curIndex = 0
    }
    val level = if (curDepth < sizes.length) levels(curDepth) else { curDepth = Int.MaxValue; maxLevel }
    val a = level(curIndex)
    assert(a != null)
    level(curIndex) = null
    curIndex += 1
    a
  }

  /**
   * `true` if there are more elements after the current index.
   */
  def isEmpty: Boolean = if (maxLevelSize == 0)
    (curDepth == maxDepth) && (curIndex == sizes(curDepth))
  else (curDepth == Int.MaxValue) && (curIndex == maxLevelSize)

  // ensure that level d has capacity to store new elements
  private def ensureLevelCapacity(d: Int): Array[A] =
    if (d == Int.MaxValue) {
      val arr = maxLevel
      val oldSize = arr.length
      if (oldSize <= maxLevelSize) {
        var newsize = oldSize * 2

        val newArr = new Array[A](newsize)
        compat.Platform.arraycopy(arr, 0, newArr, 0, oldSize)
        maxLevel = newArr
        newArr
      } else arr
    } else {
      val arr = levels(d)
      val oldSize = arr.length
      if (oldSize <= sizes(d)) {
        var newsize = oldSize * 2

        val newArr = new Array[A](newsize)
        compat.Platform.arraycopy(arr, 0, newArr, 0, oldSize)
        levels(d) = newArr
        newArr
      } else arr
    }

  // ensure that we have levels allocated with indices at least up to d
  private def ensureDepthCapacity(d: Int) = {
    val arr = levels
    val oldSize = arr.length
    if (d >= oldSize) {
      var newSize = oldSize * 2
      while (d >= newSize) newSize *= 2

      val newArr = new Array[Array[A]](newSize)
      compat.Platform.arraycopy(arr, 0, newArr, 0, oldSize)

      val newSizeArr = new Array[Int](newSize)
      compat.Platform.arraycopy(sizes, 0, newSizeArr, 0, oldSize)

      // fill new levels and sizes with fresh arrays
      var i = oldSize
      while (i < newSize) {
        newArr(i) = new Array[A](initWidth)
        i += 1
      }
      levels = newArr
      sizes = newSizeArr
    }
  }

  def reinsert(elem: A) = sys.error("not implemented")
}