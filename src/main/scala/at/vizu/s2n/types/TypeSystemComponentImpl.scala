package at.vizu.s2n.types

import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.symbol.TScope

/**
 * Phil on 08.10.15.
 */
trait TypeSystemComponentImpl extends TypeSystemComponent {
  self: TypeSystemInitializerComponent =>

  class BaseTypeSystem() extends TypeSystem {

    override def checkTrees(trees: Seq[AST]): Unit = {
      val rootScope: TScope = typeSystemInitializer.initTypeSystem(trees)
      val typeChecker: TypeChecker = new TypeChecker(rootScope)
      trees.foreach(typeChecker.checkTypes)
    }

  }

}
