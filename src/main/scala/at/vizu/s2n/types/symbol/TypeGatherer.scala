package at.vizu.s2n.types.symbol

import at.vizu.s2n.parser.AST

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

/**
 * Phil on 21.10.15.
 */
object TypeGatherer {

  def gatherTypes(trees: Seq[AST], scope: TScope) = {
    trees.foreach(tree => gatherTypesFromTree(tree, scope))
  }

  def gatherTypesFromTree(tree: AST, scope: TScope): Unit = {
    val traverser: ClassTraverser = new ClassTraverser(tree, scope)
    traverser.traverse(tree.internalTree)
  }

  private class ClassTraverser(ast: AST, scope: TScope) extends Traverser {
    val pkgBuilder = new ArrayBuffer[String]

    override def traverse(tree: Tree): Unit = {
      //println(tree.toString())
      tree match {
        case c: ClassDef =>
          buildEmptyClasses(ast.fileName, packageName, c)
        case m: ModuleDef =>
          buildEmptyObject(ast.fileName, packageName, m)
        case PackageDef(Ident(name), subtree) =>
          pkgBuilder.append(name.toString)
          super.traverse(tree)
        case _ => super.traverse(tree)
      }
    }

    def packageName = {
      pkgBuilder.mkString(".")
    }

    private def buildEmptyClasses(currentFile: String, pkgName: String, c: ClassDef) = {
      val ctx = Context(currentFile, c.pos.line)
      TypeUtils.addClass(scope, TType(ctx, c.name.toString, pkgName, TypeUtils.getModifiers(c.mods)))
    }

    private def buildEmptyObject(currentFile: String, pkgName: String, m: ModuleDef) = {
      val ctx = Context(currentFile, m.pos.line)
      val tpe: TType = TType(ctx, m.name.toString, pkgName, TypeUtils.getModifiers(m.mods))
      scope.addObject(tpe)
      scope.add(Identifier(ctx, tpe.fullClassName, tpe, mutable = false))
    }
  }

}
