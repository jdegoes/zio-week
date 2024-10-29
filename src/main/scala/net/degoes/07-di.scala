/** DEPENDENCY INJECTION
  *
  * ZIO provides a comprehensive solution to the problem of type-safe dependency injection. Coding to interfaces, rather
  * than classes, and factoring an application into reusable components, is a powerful technique for building testable
  * and modular applications.
  *
  * Providing you use the appropriate architecture, layers provide all of the tools necessary build applications in this
  * testable and modular way.
  */
package net.degoes.di

import zio._
import zio.test._
import zio.test.TestAspect._

final case class TodoId(value: String)

final case class User(id: String, name: String, email: String)

final case class Todo(description: String, completed: Boolean)

final case class TodoConfig()
object TodoConfig:
  /** EXERCISE 1
    *
    * Implement a layer that provides a default configuration for the Todo application.
    */
  lazy val defaultLayer: ZLayer[Any, Nothing, TodoConfig] = ???

trait TodoRepo:
  def getAll(user: User): ZIO[Any, Nothing, List[Todo]]

  def delete(id: TodoId): ZIO[Any, Nothing, Unit]

  def create(user: User, todo: Todo): ZIO[Any, Nothing, TodoId]

  def update(id: TodoId, description: String, completed: Boolean): ZIO[Any, Nothing, Boolean]

  def get(id: TodoId): ZIO[Any, Nothing, Option[Todo]]

object TodoRepo:
  /** EXERCISE 2
    *
    * Implement a layer that provides a TodoRepo backed by an in-memory data structure.
    */
  lazy val testLayer: ZLayer[Any, Nothing, TodoRepo] = ???

trait EmailService:
  def sendEmail(user: User, subject: String, body: String): ZIO[Any, Nothing, Unit]

object EmailService:
  trait TestEmailService extends EmailService {
    def emails: List[(User, String, String)]
  }

  /** EXERCISE 3
    *
    * Implement a layer that provides an email service that logs emails to the console and to an in-memory data
    * structure.
    */
  lazy val testLayer: ZLayer[Any, Nothing, EmailService] = ???

class TodoApp(todoRepo: TodoRepo, todoConfig: TodoConfig, emailService: EmailService):
  /** EXERCISE 4
    *
    * Implement the `run` method for the TodoApp. Rather than provide a full REST API, simply implement a console-based
    * application that prompts users for management of their todos.
    */
  val run: ZIO[Any, Nothing, Unit] =
    ???
object TodoApp:
  /** EXERCISE 5
    *
    * Create a layer that assembles a `TodoApp` given a `TodoRepo`, `TodoConfig`, and `EmailService`.
    */
  lazy val layer: ZLayer[TodoRepo & TodoConfig & EmailService, Nothing, TodoApp] =
    ZLayer {
      ???
    }

object TodoAppMain extends ZIOAppDefault:
  /** EXERCISE 6
    *
    * Wire up the entire application by providing the `TodoApp` with all the layers that are required for its
    * dependencies.
    */
  val run =
    ???
