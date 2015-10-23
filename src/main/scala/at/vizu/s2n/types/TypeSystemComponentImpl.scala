package at.vizu.s2n.types

import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.symbol.{ScalaScopeInitializer, Scope}

/**
 * Phil on 08.10.15.
 */
trait TypeSystemComponentImpl extends TypeSystemComponent {


  class BaseTypeSystem extends TypeSystem {

    val typeSystemInitializer = new TypeSystemInitializer with ScalaScopeInitializer

    override def checkTrees(trees: Seq[AST]): Unit = {
      val rootScope: Scope = typeSystemInitializer.initTypeSystem(trees)
      val typeChecker: TypeChecker = new TypeChecker(rootScope)
      //val symbolTable: SymbolTable = buildSymbolTable(trees)
      trees.foreach(typeChecker.checkTypes)
    }

  }

}
