package tutorial

import cats.effect._
import cats.effect.std.Console
import cats.instances.list._
import cats.syntax.all._
import scala.collection.immutable.Queue


object ProducerConsumerBoundedQueue extends IOApp {

  case class State[F[_], A](queue: Queue[A], capacity: Int, takers: Queue[Deferred[F, A]], offerers: Queue[(A, Deferred[F, Unit])])
  object State {
    def empty[F[_], A](capacity: Int): State[F, A] = State(Queue.empty, capacity, Queue.empty, Queue.empty)
  }

  def producer[F[_]: Async: Console](id: Int, counterR: Ref[F, Int], stateR: Ref[F, State[F, Int]]): F[Unit] = {

    def offer(i: Int): F[Unit] =
      Deferred[F, Unit].flatMap[Unit] { offerer =>
        stateR.modify {
          case State(queue, capacity, takers, offerers) if takers.nonEmpty =>
            val (taker, rest) = takers.dequeue
            State(queue, capacity, rest, offerers) -> taker.complete(i).void
          case State(queue, capacity, takers, offerers) if queue.size < capacity =>
            State(queue.enqueue(i), capacity, takers, offerers) -> Async[F].unit
          case State(queue, capacity, takers, offerers) =>
            State(queue, capacity, takers, offerers.enqueue(i -> offerer)) -> offerer.get
        }.flatten
      }

    for {
      i <- counterR.getAndUpdate(_ + 1)
      _ <- offer(i)
      _ <- if (i % 10_000 == 0) Console[F].println(s"Producer $id has reached $i items") else Async[F].unit
      _ <- producer(id, counterR, stateR)
    } yield ()
  }

  def consumer[F[_]: Async: Console](id: Int, stateR: Ref[F, State[F, Int]]): F[Unit] = {

    val take: F[Int] =
      Deferred[F, Int].flatMap { taker =>
        stateR.modify {
          case State(queue, capacity, takers, offerers) if queue.nonEmpty && offerers.isEmpty =>
            val (i, rest) = queue.dequeue
            State(rest, capacity,  takers, offerers) -> Async[F].pure(i)
          case State(queue, capacity, takers, offerers) if queue.nonEmpty =>
            val (i, rest) = queue.dequeue
            val ((move, release), tail) = offerers.dequeue
            State(rest.enqueue(move), capacity, takers, tail) -> release.complete(()).as(i)
          case State(queue, capacity, takers, offerers) if offerers.nonEmpty =>
            val ((i, release), rest) = offerers.dequeue
            State(queue, capacity, takers, rest) -> release.complete(()).as(i)
          case State(queue, capacity, takers, offerers) =>
            State(queue, capacity, takers.enqueue(taker), offerers) -> taker.get
        }.flatten
      }

    for {
      i <- take
      _ <- if (i % 10_000 == 0) Console[F].println(s"Consumer $id has reached $i items") else Async[F].unit
      _ <- consumer(id, stateR)
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      stateR <- Ref.of[IO, State[IO, Int]](State.empty[IO, Int](capacity = 100))
      counterR <- Ref.of[IO, Int](1)
      producers = List.range(1, 11).map(producer(_, counterR, stateR))
      consumers = List.range(1, 11).map(consumer(_, stateR))
      res <- (producers ++ consumers)
        .parSequence.as(ExitCode.Success)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res
}
