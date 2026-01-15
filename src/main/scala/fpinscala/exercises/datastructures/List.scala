package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
    // 3

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("Empty list")
      case Cons(x, xs) => xs

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil => sys.error("Empty list")
      case Cons(x, xs) => Cons(h, xs)

  def drop[A](l: List[A], n: Int): List[A] = {
    if n <= 0 then
      l
    else
      l match
        case Nil => l
        case Cons(x, xs) => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Nil => l
      case Cons(x, xs) => if f(x) then dropWhile(xs, f) else l

  def init[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("Empty list")
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (x, y) => 1 + y)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, (x, y) => x + y)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0, (x, y) => x * y)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (x, y) => 1 + x)

  def reverse[A](l: List[A]): List[A] = {
    def func(acc: List[A], next: A): List[A] =
      acc match
        case Nil => Cons(next, Nil)
        case Cons(x, xs) => Cons(next, Cons(x, xs))

    foldLeft(l, Nil, func)
  }

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =

    def func(next: A, acc: List[A]): List[A] =
      Cons(next, acc)

    foldRight(l, r, func)

  def concat[A](l: List[List[A]]): List[A] = {
    def func(next: List[A], acc: List[A]): List[A] = {
      appendViaFoldRight(next, acc)
    }

    foldRight(l, Nil, func)
  }


  def incrementEach(l: List[Int]): List[Int] = {
    def increment(l: List[Int]): List[Int] =
      l match
        case Nil => Nil
        case Cons(x, xs) => Cons(x + 1, increment(xs))

    increment(l)
  }

  def doubleToString(l: List[Double]): List[String] =
    l match
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString(), doubleToString(xs))

  def map[A,B](l: List[A], f: A => B): List[B] =
    l match
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs, f))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil => Nil
      case Cons(x, xs) => if f(x) then Cons(x, filter(xs, f)) else filter(xs, f)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    as match
      case Nil => Nil
      case Cons(x, xs) => append(f(x), flatMap(xs, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    def func(a: A): List[A] =
      if f(a) then Cons(a, Nil) else Nil

    flatMap(as, func)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (Nil, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
      case _ => Nil // Truncate to shorter list

  def zipWith[A](l1: List[A], l2: List[A], f: (A, A) => A): List[A] =
    (l1, l2) match
      case (Nil, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys, f))
      case _ => Nil // Truncate to shorter list

  def hasPrefix[A](l: List[A], prefix: List[A]): Boolean =
    (l, prefix) match
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) => (x == y) && hasPrefix(xs, ys)
      case (Nil, _) => false

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if sub == Nil then
      true
    else
      sup match
        case Nil => false
        case Cons(x, xs) => hasPrefix(sup, sub) || hasSubsequence(xs, sub)
  }

