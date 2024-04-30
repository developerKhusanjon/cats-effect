package tutorial

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.kernel.Resource


import java.io._

object FileCopier extends IOApp {
  private def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.fromAutoCloseable(IO(new FileInputStream(f)))

  private def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO.blocking(new FileOutputStream(f)) // build
    } { outStream =>
      IO.blocking(outStream.close()).handleErrorWith(_ => IO.unit) // release
    }

  private def inputOutputStreams(in: File, out: File): Resource[IO, (FileInputStream, FileOutputStream)] =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)

  private def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO.blocking(origin.read(buffer, 0, buffer.size))
      count <- if (amount > -1) IO.blocking(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      // first.flatMap(_ => second)
      else IO.pure(acc) // End of read stream reached (by java.io.InputStream contract), nothing to write
    } yield count

  // transfer will do the real work
  private def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    transmit(origin, destination, new Array[Byte](1024 * 10), 0L)

  private def copy(origin: File, destination: File): IO[Long] =
    inputOutputStreams(origin, destination).use {
      case (in, out) => transfer(in, out)
    }

  /*
  def copy(origin: File, destination: File): IO[Long] = {
    val inIO: IO[InputStream] =  IO(new FileInputStream(origin))
    val outIO: IO[OutputStream] = IO(new FileOutputStream(destination))

    (inIO, outIO)  // Stage 1: Getting resources
      .tupled                  // From (IO[InputStream], IO[OutputStream]) to IO[(InputStream, OutputStream)]
      .bracket {
        case (in, out) =>
          transfer(in, out)     // Stage 2: Using resources (for copying data, in this case)
      } {
        case (in, out) => // Stage 3: Freeing resources
          (IO(in.close()), IO(out.close()))
          .tupled   // From (IO[Unit], IO[Unit]) to IO[(Unit, Unit)]
            .void.handleErrorWith(_ => IO.unit)
      }
  }
   */


  override def run(args: List[String]): IO[ExitCode] =
    for {
      _      <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Need origin and destination files"))
                 else IO.unit
      orig = new File(args.head)
      dest = new File(args(1))
      count  <- copy(orig, dest)
      _      <- IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
    } yield ExitCode.Success
}






