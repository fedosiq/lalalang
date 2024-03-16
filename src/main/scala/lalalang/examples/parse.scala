package lalalang.examples

import cats.effect.IO
import lalalang.lib.Show.instances.given
import lalalang.lib.expr.Expr
import lalalang.lib.interpreters.TreeInterpreter.Error
import lalalang.lib.interpreters.{EnvInterpreter, TreeInterpreter}
import lalalang.lib.parser.LCParser

@main def parseTest: Unit =
  val examples = List(
    "2+2",
    "if ((λx.(x)) a) {T} else {F}",
    "(λa.λb.a) 2 4",
    "((λt.λf.t) λt.λf.f) λt.λf.t",
    "((λp.λq.((p) q) p) λt.λf.t) λt.λf.f",
    "λf.(λx.f (x x)) λx.f (x x)",
    "let x := 42 in x"
  )

  examples
    .map(
      LCParser().parse(_)
      // .map(reduceExample)
    )
    .foreach(println)

def reduceExample(expr: Expr): Unit =
  println("input inner repr:")
  pprint.pprintln(expr)

  // val (res, time1)        = timed(SubstituteTreeInterpreter.eval(expr))
  // val (envEvalRes, time2) = timed(EnvInterpreter.eval(Map.empty)(expr))
  val res        = TreeInterpreter.eval[Either[Error, *]](expr).toOption.get
  val envEvalRes = EnvInterpreter[IO](debug = false).initEval(Map.empty)(expr).unsafeRunSync

  // println(s"[${time1}ms] inner repr of substitution result:")
  pprint.pprintln(res)

  println(s"${expr.show} ~> ${res.show}")
  println("-" * 50 + "\n")

  // println(s"[${time2}ms] inner repr of env result:")
  pprint.pprintln(envEvalRes)
  println("-" * 50 + "\n")
