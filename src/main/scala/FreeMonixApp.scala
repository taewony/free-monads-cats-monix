import cats._, cats.data._, cats.free._
import monix.cats._, monix.eval.Task
import services.free._

class FreeMonixApp {
  protected type RecordedActionsApp[A] = Coproduct[DataOps.DSL, Interactions.DSL, A]
  protected type AuditedRecordedActionsApp[A] = Coproduct[Logs.DSL, RecordedActionsApp, A]

  private def program(implicit
    A: InteractionService[AuditedRecordedActionsApp],
    D: DataOpService[AuditedRecordedActionsApp],
    L: LogService[AuditedRecordedActionsApp]
  ): Free[AuditedRecordedActionsApp, String] = {
    import A._, D._, L._

    for {
      response <- get("First ?")
      _ <- D.add(response)
      _ <- L.info(s"Log: Recorded: $response")
      response <- get("Second ?")
      _ <- D.add(response)
      _ <- L.debug(s"Log: Recorded: $response")
      responses <- findAll
      _ <- print(responses.toString)
      _ <- L.info(s"Log: Printed: $responses")
      logs <- show
    } yield logs.mkString("\n")
  }

  protected def interpreter(in: () => String): AuditedRecordedActionsApp ~> Task = {
    val recordedActionsInterpreter: RecordedActionsApp ~> Task =
      new InMemoryDataOpInterpreter or new InteractionInterpreter(in)
    new LogInterpreter or recordedActionsInterpreter
  }

  def run(in: () => String): Task[String] = {
    program.foldMap(interpreter(in))
  }
}

/*
object UserApp2 extends App {
  type Eff[A] = Coproduct[InOut, ManipulateAccount, A]
  
  def program[S[_]](implicit
    io: InOut.Ops[S],
    ma: ManipulateAccount.Ops[S]
  ): Free[S, Option[UserAccount]] = for {
    name <- io.ask("What is your login?")
    age <- io.ask("What is your age?")
    h <- ma.create(name, 25)
    user <- ma.fetch(h)
  } yield(user)

  val prog = program[Eff]

  def interpreter[S[_]](implicit
    s0: KVS[Long, UserAccount, ?] :<: S,
    s1: MonotonicSeq :<: S,
    s2: Task :<: S
  ): Eff ~> Free[S, ?] = {
    type G[A] = Free[S, A]

    val first: InOut ~> G = InOutInterpreter.interpreter[S]
    val second: ManipulateAccount ~> G =  MAInterpreters.interpreter[S]

    first :+: second
  }

  type LowerEff0[A] = Coproduct[MonotonicSeq, KVS[Long, UserAccount, ?], A]
  type LowerEff[A] = Coproduct[Task, LowerEff0, A]

  def lowelevelInterpreter: Free[LowerEff, ?] ~> Task = ???

  implicit val kvs: KVS[Long, UserAccount, ?] :<: LowerEff = implicitly[KVS[Long, UserAccount, ?] :<: LowerEff]
  implicit val ms: MonotonicSeq :<: LowerEff = implicitly[MonotonicSeq :<: LowerEff]

  val theInt: Eff ~> Task = interpreter[LowerEff] andThen lowelevelInterpreter

}

*/

/* Simple FreeApp Version
sealed trait InOut[A]
case class PrintLine(line: String) extends InOut[Unit] 
case object GetLine extends InOut[String]

object InOut {
  class Ops[S[_]](implicit s0: InOut :<: S) {
    def printLine(line: String): Free[InOut, Unit] = Free.liftF(PrintLine(line))
    def getLine(): Free[InOut, String] = Free.liftF(GetLine)
    def ask(question: String): Free[S, String] = for {
      _ <- printLine(question)
      answer <- getLine()
    } yield answer
  }
}

object ConsoleInterpreter extends (InOut ~> Task) {
  def apply[A](inout: InOut[A]): Task[A] = inout match {
    case PrintLine(line) => Task.delay {
      println(line)
    }
    case GetLine => Task.delay {
      scala.io.StdIn.readLine()
    }
  }
}

object FreeMonixApp {
  import InOut._
  val program: Free[InOut, String] = for {
    name <- ask("What is your name")
    _ <- printLine(s"Nice to meet you $name")
  } yield(name)

  def run(): Task[String] = {
    program.foldMap(ConsoleInterpreter)
  }
}*/