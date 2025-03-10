package u02

object Hello extends App:
  println("Hello Scala")

  def positive(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"
  println(positive(0))

  val positivefunc: Int => String = x => x match
    case x if x >= 0 => "positive"
    case _ => "negative"
  println(positivefunc(0))


  def fakeNegative(s: String => Boolean): (String => Boolean) = s => true
  println(fakeNegative(s => false)("hello scala"))

  def negative(p: String => Boolean): (String => Boolean) =
    s => !p(s)
  println(negative(s => true)("hello scala"))

  def genericNegative[X](p: X => Boolean): (X => Boolean) =
    s => !p(s)
  println(genericNegative(s => true)(5))

  def curriedCheck(x: Int)(y: Int)(z: Int): Boolean =
    x <= y && y == z
  println(curriedCheck(2)(3))
  println(curriedCheck(2)(3)(3))

  val p1: Int => (Int => (Int => Boolean)) = x => y => z =>
    x <= y && y == z
  println(p1(2)(3))

