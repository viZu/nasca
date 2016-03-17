package at.vizu.s2n.types

import at.vizu.s2n.args.Arguments
import at.vizu.s2n.log.ProfilerWithErrors._
import at.vizu.s2n.parser.AST
import at.vizu.s2n.types.result.ScalaFileWrapper
import at.vizu.s2n.types.symbol.TSymbolTable
import com.typesafe.scalalogging.LazyLogging

/**
 * Phil on 06.11.15.
 */
class BaseTypeSystem(typeSystemInitializer: TypeSystemInitializer, typeChecker: ReflectTypeChecker)
  extends TypeSystem with LazyLogging {

  override def checkTrees(args: Arguments, trees: Seq[AST]): (TSymbolTable, Seq[ScalaFileWrapper]) = {
    val rootScope = profile(logger, "Typesystem initialization", typeSystemInitializer.initTypeSystem(args, trees))
    //val rootScope: TScope = typeSystemInitializer.initTypeSystem(trees)
    profile(logger, "Type checker", (rootScope, trees.map(typeChecker.checkTypes(rootScope, _))))
  }

}
