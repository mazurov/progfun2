import calculator.Calculator._
import calculator._


val namedExpressions = Map("a" -> Var[Expr](Ref("a")))
//namedExpressions("a")() = Ref("a")
val values = computeValues(namedExpressions)

