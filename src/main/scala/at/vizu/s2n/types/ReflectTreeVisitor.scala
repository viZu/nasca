package at.vizu.s2n.types

import at.vizu.s2n.types.symbol.TScope

import scala.reflect.runtime.universe._

/**
 * Phil on 06.11.15.
 */
@Deprecated
abstract class ReflectTreeVisitor[T] {

  def visit(rootScope: TScope, tree: Tree): T = {
    visitTree(tree)
  }

  def visitTree(tree: Tree): T = ???

  def visitPackage(pkg: Package): T = ???

  def visitImport(importDef: Import): T = ???

  def visitModule(moduleDef: ModuleDef): T = ???

  def visitClass(classDef: ClassDef): T = ???

  def visitValMember(valDef: ValDef): T = ???

  def visitDefMember(defDef: DefDef): T = ???

  def visitMember(member: Tree): T = ???

  def visitDefBody() = ???

}
