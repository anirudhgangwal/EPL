import scala.collection.immutable.ListMap

/* Part 2 */

def incr(x: Int): Int = x + 1
def double(x: Int): Int = x + x
def square(x: Int): Int = x * x

def factorial(n: Int): Int = 
  if (n == 0) {1} else {n * factorial(n-1)}


def power(x: Int, n: Int): Int =
  if (n == 0) {1} else {x * power(x,n-1)}

def factorial1(n: Int): Int = {
  val m = n-1 ; if (n == 0) {1} else {n * factorial1(m)}
}


def factorial2(n: Int): Int = {
  val m = n-1;
  if (n == 0) {1} else {n * factorial2(m)}
}


def factorial3(n: Int): Int = {
  val m = n-1;
  if (n == 0) {
    return 1;
  } else {
    return n * factorial3(m);
  }
}

/* Exercise 1 */
def p(x: Int, y:Int): Int = 
  power(x, 2) + 2 * x * y + power(y, 3) - 1

/* Exercise 2 */
def sum(n: Int): Int = 
  if (n == 1) {1} else {n + sum(n-1)}

/* Part 3 */

/* Exercise 3 */
def cycle(q:(Int,Int,Int)): (Int,Int,Int) =
  (q._2, q._3, q._1)

/* Part 4 */


def nameFromNum(presidentNum: Int): String = presidentNum match {
  case 41 => "George H. W. Bush"
  case 42 => "Bill Clinton"
  case 43 => "George W. Bush"
  case 44 => "Barack Obama"
  case 45 => "Donald J. Trump"
  case n => "I don't know who president number " + n + " is"
}

def numFromName(presidentName: String): Int = presidentName match {
  case "George H. W. Bush" => 41
  case "Bill Clinton" => 42
  case "George W. Bush" => 43
  case "Barack Obama" => 44
  case "Donald J. Trump" => 44
}

/* Exercise 4 */
def suffix(n: Int): String = 
{
  (n % 100) match 
  {
    case 11 => "th"
    case 12 => "th"
    case 13 => "th" 
    case _ => (n % 10) match
      {
        case 1 => "st"
        case 2 => "nd"
        case 3 => "rd"
        case n => "th"
      }
  }
}

/*def suffix1(n: Int): String = 
{
  val num = n % 100;
  if (num == 11 || num == 12 || num = 13) {"th"}
  else {if (num % 10 ) }....
}*/

abstract class Colour
case class Red() extends Colour
case class Green() extends Colour
case class Blue() extends Colour

/* Exercise 5 */
def favouriteColour(c: Colour): Boolean = c match { 
  case Red() => false
  case Blue() => true
  case Green() => false
}


abstract class Shape
case class Circle(r: Double, x: Double, y: Double) extends Shape
case class Rectangle(llx: Double, lly: Double, w:Double, h:Double) extends Shape

def center(s: Shape): (Double,Double) = s match {
  case Rectangle(llx,lly,w,h) => (llx+w/2, lly+h/2)
  case Circle(r,x,y) => (x,y)
}

/* Exercise 6 */
def boundingBox(s: Shape): Rectangle = s match {
  case Rectangle(llx, lly, w, h) => Rectangle(llx, lly, w, h)
  case Circle(r, x, y)   => Rectangle(x-r, y-r, 2*r, 2*r)
}

/* Exercise 7 */
def mayOverlap(s1: Shape, s2: Shape): Boolean = (boundingBox(s1),boundingBox(s2)) match {
  case (Rectangle(llx1,lly1,w1,h1),Rectangle(llx2,lly2,w2,h2)) => true
}


/* Part 5 */

val anonIncr = {x: Int => x+1} // anonymous version of incr
val anonAdd = {x: Int => {y: Int => x + y}}

/* Exercise 8 */
def compose1[A, B, C](f: A => B, g: B => C)(x: A) = g(f(x))

/* Exercise 9 */
def compose[A, B, C](f: A => B, g: B => C) = {x:A => g(f(x))}

/* Exercise 10 */
def e1 = {x:Int => x.toString}
def e2 = {x:String => if (x.length < 3) {true} else {false}}

def isEmpty[A](l: List[A]) = l match { 
  case Nil => true
  case x :: y => false
}


/* Exercise 11 */
def map[A, B](f: A => B, l: List[A]): List[B] = l match {
  case x :: xs => f(x) :: map(f, xs)
  case Nil => Nil
}

/* Exercise 12 */
def filter[A](f: A => Boolean, l: List[A]): List[A] = l match {
  case x :: y :: xs => if (f(x)) {x :: filter(f, y :: xs)} else {filter(f,y :: xs)}
  case y :: xs => if (f(y)) (y :: Nil) else {Nil}
  case Nil => Nil
}

/* Exercise 13 */
def reverse[A](l: List[A]): List[A] = l match {
  case x :: xs => reverse(xs) ::: List(x)  
  case Nil => Nil
}


/* Part 6 */

def empty[K,V]: List[(K,V)] = List()

/* Exercise 14 */
def lookup[K, V](m: List[(K, V)], k: K): V = m match {
  case (x,y) :: xs => if (k == x) {y} else {lookup(xs, k)}
  case Nil => throw new Exception("key not in List Map")
}

/* Exercise 15 */
def update[K, V](m: List[(K, V)], k: K, v: V): List[(K, V)] = m match {
  case ((x,y) :: xs) => if (x == k ) {(k,v) :: xs} else {(x,y) :: update(xs, k ,v)} 
  case Nil => List((k,v))
}

/* Exercise 16 */
def keys[K,V](m: List[(K,V)]): List[K] = m match {
  case ((x,y) :: xs) => x :: keys(xs)
  case Nil => Nil
}

/* Exercise 17 */
val presidentListMap = ListMap() // TODO

/* Exercise 18 */
def m0_withUpdate = sys.error("todo")

/* Exercise 19 */
def list2map[K,V](l: List[(K,V)]): ListMap[K,V] = sys.error("todo")

/* Exercise 20 */
def election(votes: List[String]): ListMap[String,Int] = sys.error("todo")
