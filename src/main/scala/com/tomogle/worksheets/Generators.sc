import scala.util.Random

trait Generator[+T] {
  self =>

  def generate(): T
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate(): S = f(self.generate())
  }
  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate(): S = f(self.generate()).generate()
  }
}

object IntGenerator extends Generator[Int] {
  private val rand = new Random
  override def generate(): Int = rand.nextInt()
}

class StringGenerator(maxLength: Int) extends Generator[String] {
  private val rand = new Random
  override def generate(): String = rand.nextString(rand.nextInt(maxLength))
}

val booleanGenerator: Generator[Boolean] = for (x <- IntGenerator) yield x > 0
booleanGenerator.generate()
booleanGenerator.generate()
booleanGenerator.generate()
booleanGenerator.generate()

def pairGenerator[A, B](a: Generator[A], b: Generator[B]): Generator[(A,B)] =
  for {
    p1 <- a
    p2 <- b
  } yield (p1, p2)

val maxStringLength = 10
val intAndStringGenerator = pairGenerator(IntGenerator, new StringGenerator(maxStringLength))
intAndStringGenerator.generate()
intAndStringGenerator.generate()
intAndStringGenerator.generate()
intAndStringGenerator.generate()
intAndStringGenerator.generate()
intAndStringGenerator.generate()

def unitGenerator[A](a: A) = new Generator[A] {
  override def generate(): A = a
}

unitGenerator("x").generate()
