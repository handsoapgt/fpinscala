package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    this match
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    this match
      case Cons(h, t) if n > 0 => LazyList.cons(h(), t().take(n - 1))
      case _ => Empty

  def drop(n: Int): LazyList[A] =
    this match
      case Empty => Empty
      case Cons(h, t) => if n <= 0 then LazyList.cons(h(), t()) else t().drop(n - 1)

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((a, b) => if p(a) then LazyList.cons(a, b) else Empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A]) ((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(Empty: LazyList[B])( (b, bs) => LazyList.cons(f(b), bs))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((a, as) => if f(a) then LazyList.cons(a, as) else as)

  def append[B >: A](bs: LazyList[B]): LazyList[B] =
    foldRight(bs)((a, bss) => LazyList.cons(a, bss))

  def flatMap[B >: A](f: A => LazyList[B]): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, bs) => f(a).append(bs))

  def startsWith[B](s: LazyList[B]): Boolean =
    (this, s) match
      case (_, Empty) => true
      case (Cons(a, as), Cons(b, bs)) => a() == b() && as().startsWith(bs())
      case _ => false

  def hasSubsequence[B >: A](l: LazyList[B]): Boolean =
    tails.exists(ll => ll.startsWith(l))

  def mapViaUnfold[B](f: A => B): LazyList[B] = {
    LazyList.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }
  }

  def takeViaUnfold(n: Int): LazyList[A] =
    LazyList.unfold((n, this)) {
      case (0, _) => None
      case (_, Empty) => None
      case (m, Cons(h, t)) => Some(h(), (m - 1, t()))
    }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](other: LazyList[B])(f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, other)) {
      case (Cons(a, as), Cons(b, bs)) => Some(f(a(), b()), (as(), bs()))
      case _ => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, that)) {
      case (Cons(a, as), Cons(b, bs)) => Some((Some(a()), Some(b())), (as(), bs()))
      case (Cons(a, as), Empty) => Some((Some(a()), None), (as(), Empty))
      case (Empty, Cons(b, bs)) => Some((None, Some(b())), (Empty, bs()))
      case _ => None
    }

  def tails: LazyList[LazyList[A]] =
    LazyList.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(Cons(h, t), t())
    }
      .append(LazyList(Empty))

  def scanRight[B >: A](cur: B)(f: (A, B) => B): LazyList[B] =
    this match
      case Empty => LazyList.cons(cur, Empty)
      case Cons(h, t) =>
        lazy val res = t().scanRight(cur)(f)
        res match
          case Cons(b, bs) => LazyList.cons(f(h(), b()), res)
          case Empty => LazyList.cons(cur, Empty)


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    cons(a, continually(a))

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  def genFib(prev: Int, cur: Int): LazyList[Int] =
    cons(cur, genFib(cur, prev + cur))

  lazy val fibs: LazyList[Int] = cons(0, genFib(0, 1))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None => Empty
      case Some((a, s)) => cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1))((p, c) => Some((p, (c, p + c))))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(m => Some(m, m + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)(m => Some(m, m))

  lazy val onesViaUnfold: LazyList[Int] = unfold(1)(_ => Some(1, 1))
