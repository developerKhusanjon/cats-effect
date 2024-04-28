import cats.effect.kernel.{Resource, Sync}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import java.io._

object PolyMorphicFileCopier extends IOApp {

  def transfer[F[_]: Sync](origin: InputStream, destination: OutputStream): F[Long] = transmit(origin, destination, new Array[Byte](1024 * 10), 0L)

  def transmit[F[_] : Sync](origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): F[Long] =
    for {
      amount <- Sync[F].blocking(origin.read(buffer, 0, buffer.length))
      count <- if (amount > -1) Sync[F].blocking(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      else Sync[F].pure(acc)
    } yield count

  def inputOutputStream[F[_] : Sync](in: File, out: File): Resource[F, (InputStream, OutputStream)] =
    for {
      inputStream <- inputStream(in)
      outputStream <- outputStream(out)
    } yield (inputStream, outputStream)

  def inputStream[F[_] : Sync](f: File): Resource[F, FileInputStream] = Resource.fromAutoCloseable(Sync[F].blocking(new FileInputStream(f)))

  def outputStream[F[_] : Sync](f: File): Resource[F, FileOutputStream] = Resource.make(Sync[F].blocking(new FileOutputStream(f)))(outStream => Sync[F].blocking(outStream.close()))

  def copy[F[_]: Sync](origin: File, destination: File): F[Long] = inputOutputStream(origin, destination).use { case (in, out) => transfer(in, out) }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _     <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Needed origin and destination files"))
             else IO.unit
      orig = new File(args.head)
      dest = new File(args(1))
      count <- copy[IO](orig, dest)
      _     <- IO.println(s"$count bytes transferred from ${orig.getPath} to ${dest.getPath}")
    } yield ExitCode.Success
}
