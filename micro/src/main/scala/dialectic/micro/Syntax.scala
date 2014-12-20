package dialectic.micro

object syntax extends SyntaxHelper

trait SyntaxHelper {
  implicit class IdOps[A](a: A) {
    def const: Term[A] = Term.Const(a)
  }
}
