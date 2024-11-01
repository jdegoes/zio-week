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
import zio.http.Header.Te
import zio.kafka.admin.acl.AclOperation.Create
import scala.collection.mutable.ArrayBuilder.ofUnit
import java.io.IOException
import zio.json._ 
import zio.config.magnolia.examples.E

final case class TodoId(value: String)
object TodoId:
  given JsonCodec[TodoId] = JsonCodec.string.transform(TodoId(_), _.value)

final case class User(id: String, name: String, email: String)
object User:
  given JsonCodec[User] = DeriveJsonCodec.gen[User]

final case class Todo(description: String, completed: Boolean)
object Todo: 
  given JsonCodec[Todo] = DeriveJsonCodec.gen[Todo]

final case class TodoConfig()
object TodoConfig:
  /** EXERCISE 1
    *
    * Implement a layer that provides a default configuration for the Todo application.
    */
  lazy val defaultLayer: ZLayer[Any, Nothing, TodoConfig] = 
    ZLayer.succeed(TodoConfig())

trait TodoRepo:
  def getAll(user: User): ZIO[Any, Nothing, List[Todo]]

  def delete(id: TodoId): ZIO[Any, Nothing, Unit]

  def create(user: User, todo: Todo): ZIO[Any, Nothing, TodoId]

  def update(id: TodoId, description: String, completed: Boolean): ZIO[Any, Nothing, Boolean]

  def get(id: TodoId): ZIO[Any, Nothing, Option[Todo]]

object TodoRepo:
  final case class TestTodoRepoState(todos: Map[TodoId, (User, Todo)], users: Map[User, List[TodoId]]):
    def getAll(user: User): List[Todo] = 
      users.getOrElse(user, Nil).map(id => todos(id)._2)

    def delete(id: TodoId): TestTodoRepoState = 
      val user = todos(id)._1

      TestTodoRepoState(
        todos.removed(id), 
        users.updatedWith(user) { 
          case None       => Some(Nil)
          case Some(list) => Some(list.filter(_ == id))
        })

    def create(user: User, todoId: TodoId, todo: Todo): TestTodoRepoState = 
      val newTodos = todos.updated(todoId, (user, todo))

      val newUsers = users.updatedWith(user) {
        case None => Some(todoId :: Nil)
        case Some(todos) => Some(todoId :: todos)
      }

      TestTodoRepoState(newTodos, newUsers)

    def update(id: TodoId, description: String, completed: Boolean): TestTodoRepoState = 
      copy(todos = todos.updatedWith(id) {
        case None => throw new IllegalStateException(s"Expected $id in todos")
        case Some((user, todo)) => Some((user, Todo(description, completed)))
      })

    def get(id: TodoId): Option[Todo] = todos.get(id).map(_._2)

  object TestTodoRepoState:
    val empty: TestTodoRepoState = TestTodoRepoState(Map(), Map())

  final case class TestTodoRepo(state: Ref[TestTodoRepoState]) extends TodoRepo:
    def getAll(user: User): ZIO[Any, Nothing, List[Todo]] = 
      for 
        state <- state.get
      yield state.getAll(user)

    def delete(id: TodoId): ZIO[Any, Nothing, Unit] = 
      state.update(_.delete(id))

    def create(user: User, todo: Todo): ZIO[Any, Nothing, TodoId] = 
      for 
        id <- Random.nextUUID.map(uuid => TodoId(uuid.toString))
        _  <- state.update(_.create(user, id, todo))
      yield id

    def update(id: TodoId, description: String, completed: Boolean): ZIO[Any, Nothing, Boolean] = 
      state.modify { state => 
        val oldValue = state.get(id)
        (oldValue != Todo(description, completed), state.update(id, description, completed))
      }

    def get(id: TodoId): ZIO[Any, Nothing, Option[Todo]] = 
      state.get.map(_.get(id))


  /** EXERCISE 2
    *
    * Implement a layer that provides a TodoRepo backed by an in-memory data structure.
    */
  lazy val testLayer: ZLayer[Any, Nothing, TodoRepo] = 
    ZLayer {
      for 
        ref <- Ref.make(TestTodoRepoState.empty)
      yield TestTodoRepo(ref)
    }

trait EmailService:
  def sendEmail(user: User, subject: String, body: String): ZIO[Any, Nothing, Unit]

object EmailService:
  final case class TestEmailService(emailsRef: Ref[Chunk[(User, String, String)]]) extends EmailService {
    def sendEmail(user: User, subject: String, body: String): ZIO[Any, Nothing, Unit] = 
      for 
        _ <- Console.printLine(s"Sending email from $user, $subject, $body").orDie
        _ <- emailsRef.update(_ :+ (user, subject, body))
      yield ()

    def emails: ZIO[Any, Nothing, Chunk[(User, String, String)]] = emailsRef.get
  }

  /** EXERCISE 3
    *
    * Implement a layer that provides an email service that logs emails to the console and to an in-memory data
    * structure.
    */
  lazy val testLayer: ZLayer[Any, Nothing, TestEmailService] = 
    ZLayer {
      for 
        ref <- Ref.make(Chunk.empty[(User, String, String)])
      yield TestEmailService(ref)
    }

case class TodoApp(todoRepo: TodoRepo, todoConfig: TodoConfig, emailService: EmailService):
  val testUser = User("jdegoes", "John De Goes", "john@degoes.net")

  enum Choice:
    case ListTodos
    case CreateTodo 
    case CompleteTodo 
    case ExitApp

  def parseChoice(line: String): ZIO[Any, Throwable, Choice] = 
    (ZIO.attempt: 
      line.trim match
        case "1" => Choice.ListTodos 
        case "2" => Choice.CreateTodo 
        case "3" => Choice.CompleteTodo 
        case "4" => Choice.ExitApp)

  val listTodos: ZIO[Any, IOException, Unit] = 
    for 
      todos <- todoRepo.getAll(testUser)
      _     <- ZIO.foreach(todos)(todo => Console.printLine(todo))
    yield () 

  val createTodo: ZIO[Any, IOException, Unit] = 
    for 
      _           <- Console.printLine("Please describe the todo:")
      description <- Console.readLine
      id          <- todoRepo.create(testUser, Todo(description, false))
      _           <- Console.printLine(s"Id: $id")
    yield ()

  val completeTodo: ZIO[Any, IOException, Unit] = 
    def updateTodo(id: TodoId): ZIO[Any, Option[Nothing], Unit] = 
      for
        todo <- todoRepo.get(id).some
        _    <- todoRepo.update(id, todo.description, true)
      yield ()

    for 
      _    <- Console.printLine("Please enter your todo id:")
      id   <- Console.readLine.map(TodoId(_))
      _    <- updateTodo(id).ignore
    yield () 

  val menuTable: Choice => ZIO[Any, IOException, Boolean] = {
    case Choice.ListTodos     => listTodos.as(true)
    case Choice.CreateTodo    => createTodo.as(true)
    case Choice.CompleteTodo  => completeTodo.as(true)
    case Choice.ExitApp       => ZIO.succeed(false)
  }

  val runLoopStep: ZIO[Any, Any, Boolean] = 
    val prompt = 
      Console.printLine("Please enter your option:")  *> 
      Console.printLine("1. List todos")              *> 
      Console.printLine("2. Create todo")             *> 
      Console.printLine("3. Complete todo")           *> 
      Console.printLine("4. Exit app")

    val attemptInput = 
      (prompt *> Console.readLine.orDie.flatMap(parseChoice)).tapError:
        _ => Console.printLine("Unrecognized choice!").ignore

    for 
      choice <- attemptInput.eventually
      bool   <- menuTable(choice)
    yield bool

  /** EXERCISE 4
    *
    * Implement the `run` method for the TodoApp. Rather than provide a full REST API, simply implement a console-based
    * application that prompts users for management of their todos.
    */
  val run: ZIO[Any, Any, Unit] =
    for 
      _ <- Console.printLine("Welcome to TodoApp v0.01!")
      _ <- runLoopStep.repeatWhileEquals(true)
      _ <- Console.printLine("Goodbye!")
    yield () 

object TodoApp:
  /** EXERCISE 5
    *
    * Create a layer that assembles a `TodoApp` given a `TodoRepo`, `TodoConfig`, and `EmailService`.
    */
  lazy val layer: ZLayer[TodoRepo & TodoConfig & EmailService, Nothing, Unit] =
    ZLayer:
      for 
        todoRepo <- ZIO.service[TodoRepo] 
        todoConfig <- ZIO.service[TodoConfig]
        emailService <- ZIO.service[EmailService]
        todoApp = TodoApp(todoRepo, todoConfig, emailService)
        _ <- todoApp.run.orDie
      yield ()
object TodoAppMain extends ZIOAppDefault:
  /** EXERCISE 6
    *
    * Wire up the entire application by providing the `TodoApp` with all the layers that are required for its
    * dependencies.
    */
  // TodoApp.layer.runWith(TodoRepo.testLayer, TodoConfig.defaultLayer, EmailService.testLayer)
  val run =    
    // TodoApp.layer.runWith(TodoRepo.testLayer, TodoConfig.defaultLayer, EmailService.testLayer)
    ZIO.serviceWithZIO[TodoApp](_.run).provide(TodoApp.layer, TodoRepo.testLayer, TodoConfig.defaultLayer, EmailService.testLayer)
