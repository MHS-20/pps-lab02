package u02

object Hello extends App:
  println("Hello Scala")

  // POSITIVE
  def positive(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"

  println(positive(0))

  val positivefunc: Int => String = x => x match
    case x if x >= 0 => "positive"
    case _ => "negative"
  println(positivefunc(0))


  // NEGATIVE
  def fakeNegative(s: String => Boolean): (String => Boolean) = s => true

  println(fakeNegative(s => false)("hello scala"))

  def negative(p: String => Boolean): (String => Boolean) =
    s => !p(s)

  println(negative(s => true)("hello scala"))

  def genericNegative[X](p: X => Boolean): (X => Boolean) =
    s => !p(s)

  println(genericNegative(s => true)(5))


  // CURRYING
  def curriedCheck(x: Int)(y: Int)(z: Int): Boolean =
    x <= y && y == z

  println(curriedCheck(2)(3))
  println(curriedCheck(2)(3)(3))

  val p1: Int => (Int => (Int => Boolean)) = x => y => z =>
    x <= y && y == z
  println(p1(2)(3))
  println(p1(2)(3)(3))

  // COMPOSITION
  def compose(f: Int => Int, g: Int => Int): Int => Int = n =>
    f(g(n))

  println(compose(_ - 1, _ * 2)(5))

  def genericCompose[A, B, C](f: B => C, g: A => B): A => C = x =>
    f(g(x))

  println(genericCompose[Int, Int, Int](_ - 1, _ * 2)(5))
  println(genericCompose[String, String, String](_ + "1", _ + "2")("hello "))
  println(genericCompose[Int, Int, String](_ + "hello scala", _ + 2)(5))

  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = n =>
    // f(g(h(n)))
    genericCompose(genericCompose(f, g), h)(n)

  println(composeThree[Int, Int, Int, Int](_ - 1, _ * 2, _ + 2)(5))
  println(composeThree[String, String, String, String](_ + "1", _ + "2", _ * 2)("hello "))
  println(composeThree[Int, Int, String, String](_ + "world", _ + "hello ", _ * 5)(5))


  // POWER
  def power(base: Double, exp: Int): Double = exp match
    case 0 => 1
    case _ => base * power(base, exp - 1)
  println(power(2, 4))

  def powerTail(base: Double, exp: Int): Double =
    @annotation.tailrec
    def _power(base: Double, exp: Int, acc: Double): Double = exp match
      case 0 => acc
      case _ => _power(base, exp - 1, acc * base)
    _power(base, exp, 1)
  println(powerTail(2, 4))


  // REVERSE DIGIT
  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _reverse(n: Int, acc: Int): Int = n match
      case 0 => acc
      case _ => _reverse(n/10, acc*10 + n%10)
    _reverse(n, 0)
  println(reverseNumber(12345))


