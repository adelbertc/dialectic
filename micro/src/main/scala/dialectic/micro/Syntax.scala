package dialectic.micro

object syntax extends SyntaxHelper

trait SyntaxHelper {
  implicit class IntOps(int: Int) {
    def const: Term = Term.Const(int)
  }
}
