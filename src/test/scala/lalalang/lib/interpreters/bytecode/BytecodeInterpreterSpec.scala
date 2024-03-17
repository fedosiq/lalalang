package lalalang.lib.interpreters.bytecode

import munit.FunSuite
import lalalang.examples.functions.twoPlus3Times4

class BytecodeInterpreterSpec extends FunSuite:
  test("simple expression") {
    assertEquals(Bytecode.eval(twoPlus3Times4), VM.Value.Integer(14))
  }
