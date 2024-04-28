import cats.effect.{ExitCode, IO, IOApp}

object ProducerConsumerProblem extends IOApp{

  import cats.effect.{Deferred, Ref, Async}
  import cats.effect.std.Console
  import cats.syntax.all._
  import scala.collection.immutable.Queue

  case class State[F[_], A](queue: Queue[A], takers: Queue[Deferred[F,A]])

  def consumer[F[_]: Async: Console](id: Int, stateR: Ref[F, State[F, Int]]): F[Unit] = {

    val take: F[Int] =
      Deferred[F, Int].flatMap { taker =>
        stateR.modify {
          case State(queue, takers) if queue.nonEmpty =>
            val (i, rest) = queue.dequeue
            State(rest, takers) -> Async[F].pure(i) // Got element in queue, we can just return it
          case State(queue, takers) =>
            State(queue, takers.enqueue(taker)) -> taker.get // No element in queue, must block caller until some is available
        }.flatten
      }

    for {
      i <- take
      _ <- if(i % 10000 == 0) Console[F].println(s"Consumer $id has reached $i items") else Async[F].unit
      _ <- consumer(id, stateR)
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = ???
}
