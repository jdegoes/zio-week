package net.degoes.http

import zio._
import zio.http._
import zio.http.codec._
import zio.http.codec.PathCodec._
import zio.http.endpoint._
import zio.http.endpoint.openapi._
import zio.http.netty.NettyConfig
import zio.http.netty.NettyConfig.LeakDetectionLevel
import zio.schema.{ DeriveSchema, Schema }
import zio.schema.annotation.description

import scala.util.Try

import net.degoes.di._ 

final case class TodoHttpApp(todoRepo: TodoRepo): 
  import PathCodec.string

  private def listTodos(request: Request) = Response()

  private def addTodo(request: Request) = Response()

  private def getTodo(id: String, request: Request) = Response()

  private def updateTodo(id: String, request: Request) = Response()

  private def deleteTodo(id: String, request: Request) = Response()

  val routes = Routes(
    Method.GET    / ""            -> handler(listTodos),
    Method.POST   / ""            -> handler(addTodo),
    Method.GET    / string("id")  -> handler(getTodo),
    Method.PUT    / string("id")  -> handler(updateTodo),
    Method.DELETE / string("id")  -> handler(deleteTodo),
  ).nest("todos")

object TodoHttpApp:
  val layer = 
    ZLayer:
      for 
        server   <- ZIO.service[Server]
        todoRepo <- ZIO.service[TodoRepo]
        httpApp   = TodoHttpApp(todoRepo)
        _        <- server.install(httpApp.routes)
      yield ()

object TodoAppHttpMain extends ZIOAppDefault {
  // Set a port
  val PORT = 58080

  val run = ZIOAppArgs.getArgs.flatMap { args =>
    // Configure thread count using CLI
    val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

    val config           = Server
      .Config
      .default
      .port(PORT)
    val nettyConfig      = NettyConfig
      .default
      .leakDetection(LeakDetectionLevel.PARANOID)
      .maxThreads(nThreads)
    val configLayer      = ZLayer.succeed(config)
    val nettyConfigLayer = ZLayer.succeed(nettyConfig)

    ZIO.never.provide(
      TodoRepo.testLayer, 
      TodoHttpApp.layer, 
      configLayer, 
      nettyConfigLayer, 
      Server.customized)
  }
}

object BooksEndpointExample extends ZIOAppDefault {
  case class Book(
      @description("Title of the book")
      title: String,
      @description("List of the authors of the book")
      authors: List[String],
    )
  object Book {
    implicit val schema: Schema[Book] = DeriveSchema.gen
  }

  object BookRepo {
    val book1                       = Book("Programming in Scala", List("Martin Odersky", "Lex Spoon", "Bill Venners", "Frank Sommers"))
    val book2                       = Book("Zionomicon", List("John A. De Goes", "Adam Fraser"))
    val book3                       = Book("Effect-Oriented Programming", List("Bill Frasure", "Bruce Eckel", "James Ward"))
    def find(q: String): List[Book] =
      if (q.toLowerCase == "scala") List(book1, book2, book3)
      else if (q.toLowerCase == "zio") List(book2, book3)
      else List.empty
  }

  val listBooks =
    Endpoint((RoutePattern.GET / "books") ?? Doc.p("Route for querying books"))
      .query(
        HttpCodec.query[String]("q").examples(("example1", "scala"), ("example2", "zio")) ?? Doc.p(
          "Query parameter for searching books"
        )
      )
      .out[List[Book]](Doc.p("List of books matching the query")) ?? Doc.p(
      "Endpoint to query books based on a search query"
    )

  def example(exec: EndpointExecutor[Any, AuthType.None.type]) = 
    for 
      result <- exec(listBooks("scala"))
    yield () 

  val booksRoute    = listBooks.implementHandler(handler((query: String) => BookRepo.find(query)))
  val openAPI       = OpenAPIGen.fromEndpoints(title = "Library API", version = "1.0", listBooks)
  val swaggerRoutes = SwaggerUI.routes("docs" / "openapi", openAPI)
  val routes        = Routes(booksRoute) ++ swaggerRoutes

  def run = Server.serve(routes).provide(Server.default)
}
