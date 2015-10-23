package at.vizu.s2n.types

import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.symbol.{ScalaScopeInitializer, SymbolTable}

/**
 * Phil on 08.10.15.
 */
trait TypeSystemComponentImpl extends TypeSystemComponent {

  class BaseTypeSystem extends TypeSystem {
    override def checkTrees(trees: Seq[AST]): Unit = {
      val symbolTable: SymbolTable = buildSymbolTable(trees)
      trees.foreach(symbolTable.checkTypes)
    }

    private def buildSymbolTable(trees: Seq[AST]): SymbolTable = {
      new SymbolTable(trees) with ScalaScopeInitializer
    }
  }

}
