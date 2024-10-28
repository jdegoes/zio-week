/** FAILURE
  *
  * The best effect systems help you reason about failure scenarios, and deal with them appropriately. Like Rust and Go,
  * effect systems use static types to help you understand if code can fail, and how.
  *
  * However, functional effect systems like ZIO provide a more nuanced take on error handling that recognizes the
  * existence of both recoverable and unrecoverable errors, and provides a rich set of tools for dealing with both.
  */
package net.degoes.failure

import zio.test._
import zio.test.TestAspect.ignore

object UntypedCheckoutExample:
  /** UNTYPED EXAMPLE
    *
    * Reason about the correctness of failure handling in the following code.
    *
    * What are your observations?
    */
  def checkout(cart: Cart): Unit =
    reserveInventory(cart)
    chargeCustomer(cart)
    dispatchOrder(cart)
    generateShippingLabel(cart)
    sendEmailConfirmation(cart)

  case class Cart(items: List[String])

  def reserveInventory(cart: Cart): Unit = ???

  def chargeCustomer(cart: Cart): Unit = ???

  def dispatchOrder(cart: Cart): Unit = ???

  def generateShippingLabel(cart: Cart): Unit = ???

  def sendEmailConfirmation(cart: Cart): Unit = ???

/** TYPED EXAMPLE
  *
  * This is a checkout example where the errors are all strongly statically typed, using Scala 3 enums for each class of
  * error, and with clearly distinguished recoverable and unrecoverable errors.
  *
  * Try to reason about the correctness of the error handling in this code.
  */
object TypedCheckoutExample:

  def checkout(cart: Cart) =
    for
      _ <- reserveInventory(cart)
      _ <- chargeCustomer(cart)
      _ <- dispatchOrder(cart)
      _ <- generateShippingLabel(cart)
      _ <- sendEmailConfirmation(cart)
    yield ()

  case class Cart(items: List[String])

  def reserveInventory(cart: Cart): Fallible[ReservationError, Cart] = ???

  def chargeCustomer(cart: Cart): Fallible[ChargeError, Cart] = ???

  def dispatchOrder(cart: Cart): Fallible[DispatchError, Cart] = ???

  def generateShippingLabel(cart: Cart): Fallible[ShippingLableError, Cart] = ???

  def sendEmailConfirmation(cart: Cart): Fallible[EmailSendError, Cart] = ???

  enum ReservationError:
    case OutOfStock
    case BackOrder
    case Discontinued

  enum ChargeError:
    case InsufficientCredit
    case AddressVerificationFailed
    case PaymentGatewayDown

  enum DispatchError:
    case ServiceDown

  enum ShippingLableError:
    case ServiceDown
    case AddressNotFound

  enum EmailSendError:
    case ServiceDown
    case InvalidServer

object Fallible:
  def succeed[A](a: A): Fallible[Nothing, A] = ???

  def fail[E](e: E): Fallible[E, Nothing] = ???

  def attempt[A](f: => A): Fallible[Throwable, A] = ???

  def fromEither[E, A](either: Either[E, A]): Fallible[E, A] =
    either.fold(fail, succeed)

trait Fallible[+E, +A]:
  def map[B](f: A => B): Fallible[E, B] =
    flatMap(a => Fallible.succeed(f(a)))

  def flatMap[E1 >: E, B](f: A => Fallible[E1, B]): Fallible[E1, B] =
    attempt match
      case Left(e)  => Fallible.fail(e)
      case Right(a) => f(a)

  def attempt: Either[E, A]

  def get(using ev: E <:< Nothing): A = ???

  def orDie(using ev: E <:< Throwable): Fallible[Nothing, A] = ???

  def mapError[E1](f: E => E1): Fallible[E1, A] = ???

  def refineOrDie[E1](pf: PartialFunction[E, E1])(using ev: E <:< Throwable): Fallible[E1, A] = ???

object FallibleSpec extends ZIOSpecDefault:
  def spec =
    suite("FallibleSpec")(
      test("succeed") {
        val result = Fallible.succeed(42)

        /** EXERCISE 1
          *
          * Implement the `succeed` method on `Fallible` so that it returns a `Fallible` that succeeds with the
          * specified value.
          */
        assertTrue(result.attempt == Right(42))
      } @@ ignore,
      test("fail") {
        val result = Fallible.fail("Uh oh")

        /** EXERCISE 2
          *
          * Implement the `fail` method on `Fallible` so that it returns a `Fallible` that fails with the specified
          * error.
          */
        assertTrue(result.attempt == Left("Uh oh"))
      } @@ ignore,
      test("attempt") {
        val success = Fallible.attempt(42)
        val failure = Fallible.attempt(throw new Error("Uh oh"))

        /** EXERCISE 3
          *
          * Implement the `attempt` method on `Fallible` so that it returns a `Fallible` that succeeds with the value
          * produced by the specified effect, or fails with the error produced by the effect.
          */
        assertTrue(success.attempt == Right(42)) &&
        assertTrue(failure.attempt.isLeft)
      } @@ ignore,
      test("fromEither") {
        val result = Fallible.fromEither(Right(42))

        /** EXERCISE 4
          *
          * Implement the `fromEither` method on `Fallible` so that it returns a `Fallible` that succeeds with the value
          * produced by the specified `Either`, or fails with the error produced by the `Either`.
          */
        assertTrue(result.attempt == Right(42))
      } @@ ignore,
      test("map") {
        val result = Fallible.succeed(42).map(_ + 1)

        /** EXERCISE 5
          *
          * Implement the `map` method on `Fallible` so that it returns a `Fallible` that applies the specified function
          * to the value produced by the source `Fallible`.
          */
        assertTrue(result.attempt == Right(43))
      } @@ ignore,
      test("flatMap") {
        val result = Fallible.succeed(42).flatMap(a => Fallible.succeed(a + 1))

        /** EXERCISE 6
          *
          * Implement the `flatMap` method on `Fallible` so that it returns a `Fallible` that applies the specified
          * function to the value produced by the source `Fallible`.
          */
        assertTrue(result.attempt == Right(43))
      } @@ ignore,
      test("get") {
        val result = Fallible.succeed(42)

        /** EXERCISE 7
          *
          * Implement the `get` method on `Fallible` so that it returns the value produced by the `Fallible`, or throws
          * an exception if the `Fallible` failed.
          */
        assertTrue(result.get == 42)
      } @@ ignore,
      test("orDie") {
        val result = Fallible.succeed(42)

        /** EXERCISE 8
          *
          * Implement the `orDie` method on `Fallible` so that it returns a `Fallible` that succeeds with the value
          * produced by the source `Fallible`, or fails with the error produced by the source `Fallible`.
          */
        assertTrue(result.orDie.attempt == Right(42))
      } @@ ignore,
      test("mapError") {
        val result = Fallible.fail("Uh oh").mapError(_.length)

        /** EXERCISE 9
          *
          * Implement the `mapError` method on `Fallible` so that it returns a `Fallible` that applies the specified
          * function to the error produced by the source `Fallible`.
          */
        assertTrue(result.attempt == Left(5))
      } @@ ignore,
      test("refineOrDie") {
        val result = Fallible.fail(new Error).refineOrDie { case _ => 42 }

        /** EXERCISE 10
          *
          * Implement the `refineOrDie` method on `Fallible` so that it returns a `Fallible` that succeeds with the
          * value produced by the source `Fallible`, or fails with the error produced by the source `Fallible`, refined
          * by the specified partial function (by refining, it is meant that if the error doesn't match the refined
          * type, it will be thrown as an exception).
          */
        assertTrue(result.attempt.isLeft)
      } @@ ignore,
    )
