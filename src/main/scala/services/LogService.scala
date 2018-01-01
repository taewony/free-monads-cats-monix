package services.free

import cats.free.{Free, Inject}
import cats.~>
import monix.eval.Task

object Logs {
  sealed trait DSL[A]
  final case class Info(line: String) extends DSL[Unit]
  final case class Warn(line: String) extends DSL[Unit]
  final case class Error(line: String) extends DSL[Unit]
  final case class Debug(line: String) extends DSL[Unit]
  final case class Show() extends DSL[List[String]]
}

final class LogService[F[_]](implicit I: Inject[Logs.DSL, F]) {
  import Logs._
  def info(a: String): Free[F, Unit] = Free.inject[DSL, F](Info(a))
  def warn(a: String): Free[F, Unit] = Free.inject[DSL, F](Warn(a))
  def error(a: String): Free[F, Unit] = Free.inject[DSL, F](Error(a))
  def debug(a: String): Free[F, Unit] = Free.inject[DSL, F](Debug(a))
  def show: Free[F, List[String]] = Free.inject[DSL, F](Show())
}
object LogService {
  implicit def logs[F[_]](implicit I: Inject[Logs.DSL, F]): LogService[F] = new LogService[F]
}


final class LogInterpreter extends (Logs.DSL ~> Task) {
  import Logs._

  private[this] val storage = new scala.collection.mutable.ListBuffer[String]

  def apply[A](l: DSL[A]) = l match {
    case Info(a) => Task { storage.append("Info: " + a) }
    case Warn(a) => Task { storage.append("Warn: " + a) }
    case Error(a) => Task { storage.append("Error: " + a) }
    case Debug(a) => Task { storage.append("Debug: " + a) }
    case Show() => Task { storage.toList }
  }
}
