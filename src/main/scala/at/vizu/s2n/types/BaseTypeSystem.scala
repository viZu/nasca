package at.vizu.s2n.types

import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.result.ScalaFileWrapper
import at.vizu.s2n.types.symbol.TScope

/**
 * Phil on 06.11.15.
 */
class BaseTypeSystem(typeSystemInitializer: TypeSystemInitializer, typeChecker: ReflectTypeChecker) extends TypeSystem {

  override def checkTrees(trees: Seq[AST]): (TScope, Seq[ScalaFileWrapper]) = {
    val rootScope: TScope = typeSystemInitializer.initTypeSystem(trees)
    (rootScope, trees.map(typeChecker.checkTypes(rootScope, _)))
  }

}
