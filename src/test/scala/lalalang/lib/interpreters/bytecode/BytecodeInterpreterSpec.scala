package lalalang.lib.interpreters.bytecode

import munit.FunSuite
import lalalang.examples.functions.twoTimes3Plus4

class BytecodeInterpreterSpec extends FunSuite:
  test("simple expression") {
    assertEquals(Bytecode.eval(twoTimes3Plus4), VM.Value.Integer(14))
  }
