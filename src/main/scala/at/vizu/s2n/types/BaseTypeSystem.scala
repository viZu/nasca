package at.vizu.s2n.types

import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.symbol.TScope

/**
 * Phil on 06.11.15.
 */
class BaseTypeSystem(typeSystemInitializer: TypeSystemInitializer) extends TypeSystem {

  override def checkTrees(trees: Seq[AST]): Unit = {
    val rootScope: TScope = typeSystemInitializer.initTypeSystem(trees)
    val typeChecker: TypeChecker = new TypeChecker(rootScope)
    trees.foreach(typeChecker.checkTypes)
  }

}
