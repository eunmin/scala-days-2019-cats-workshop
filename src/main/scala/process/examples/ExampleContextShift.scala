package process.examples

import cats.effect.IO

import scala.concurrent.ExecutionContext

object ExampleIOShift extends App {
  import ExecutionContext.global

  val currentThread = IO(Thread.currentThread.getName())

  println(s"${Thread.currentThread.getName}")

  val io1 = IO(println("Hello")).flatMap(_ => currentThread)
  val io2 = IO.shift(global).flatMap(_ => io1)

  println(s"io1 ${io1.unsafeRunSync()}")
  io1.unsafeRunAsync(result => println(s"io1 async $result"))

  println(s"io2 ${io2.unsafeRunSync()}")
  io2.unsafeRunAsync(result => println(s"io2 async $result"))

  import java.util.concurrent.Executors

  import scala.concurrent.ExecutionContext

  val cachedThreadPool = Executors.newCachedThreadPool()
  val BlockingFileIO   = ExecutionContext.fromExecutor(cachedThreadPool)
  implicit val Main = ExecutionContext.global

  implicit val contextShift = IO.contextShift(global)

  val ioa: IO[Unit] =
    for {
      _     <- IO(println(s"${Thread.currentThread.getName}"))
      _     <- IO(println("Enter your name: "))
      _     <- IO.shift(BlockingFileIO)
      _     <- IO(println(s"${Thread.currentThread.getName}"))
      //name  <- IO(scala.io.StdIn.readLine())
      _     <- IO.shift
      _     <- IO(println(s"${Thread.currentThread.getName}"))
      // _     <- IO(println(s"Welcome"))
      _     <- IO(cachedThreadPool.shutdown())
    } yield ()

//  Future { println(s"future: ${Thread.currentThread.getName}") }.onComplete(_ => ())

  ioa.unsafeRunAsync(_ => ())

  Thread.sleep(2000)
}
