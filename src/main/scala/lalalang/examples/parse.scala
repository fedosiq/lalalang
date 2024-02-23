package lalalang.examples

import lalalang.lib.LCParser
import lalalang.lib.Show.instances.given
import lalalang.lib.expr.Expr
import lalalang.lib.interpreters.TreeInterpreter.Error
import lalalang.lib.interpreters.{EnvInterpreter, TreeInterpreter}
import cats.effect.IO
import cats.effect.concurrent.Ref

@main def parseTest: Unit =
  println(LCParser.parse("λa.λb.a"))
  println(LCParser.parse("(λa.λb.a) 2 4"))

  val examples = List(
    "(λa.λb.a) 2 4",
    "((λt.λf.t) λt.λf.f) λt.λf.t",
    "((λp.λq.((p) q) p) λt.λf.t) λt.λf.f",
    "λf.(λx.f (x x)) λx.f (x x)"
  )

  examples.foreach(LCParser.parse(_).map(reduceExample))

def reduceExample(expr: Expr): Unit =
  println("input inner repr:")
  pprint.pprintln(expr)

  // val (res, time1)        = timed(SubstituteTreeInterpreter.eval(expr))
  // val (envEvalRes, time2) = timed(EnvInterpreter.eval(Map.empty)(expr))
  val res        = TreeInterpreter.eval[Either[Error, *]](expr).toOption.get
  val envEvalRes = EnvInterpreter[IO](debug = false).eval(Ref.unsafe(Map.empty))(expr).unsafeRunSync

  // println(s"[${time1}ms] inner repr of substitution result:")
  pprint.pprintln(res)

  println(s"${expr.show} ~> ${res.show}")
  println("-" * 50 + "\n")

  // println(s"[${time2}ms] inner repr of env result:")
  pprint.pprintln(envEvalRes)
  println("-" * 50 + "\n")
