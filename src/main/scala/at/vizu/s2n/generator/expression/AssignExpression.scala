package at.vizu.s2n.generator.expression

import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol.{Field, TType}

/**
  * Phil on 29.11.15.
  */
case class AssignExpression(lhs: Expression, rhs: Expression) extends Expression {
  override def exprTpe: TType = null

  override def generate: GeneratorContext = {
    val rhsCtx = rhs match {
      case b: BaseBlockExpression => rhs.generateReturn
      case _ => rhs.generate
    }
    if (isSetterField) {
      generateSetter(rhsCtx)
    } else {
      generateAssignment(lhs.generate, rhsCtx)
    }
  }

  override def skipSemiColon: Boolean = false

  override def generateReturn: GeneratorContext = generate

  private def generateAssignment(lhsCtx: GeneratorContext, rhsCtx: GeneratorContext) = {
    val generated = s"${lhsCtx.content} = ${rhsCtx.content}"
    GeneratorUtils.mergeGeneratorContexts(Vector(lhsCtx, rhsCtx), givenContent = generated)
  }

  private def generateSetter(rhsCtx: GeneratorContext) = {
    val (lhsCtx, field) = prepareSetterExpression()
    val fieldName = field.name.toUpperCase
    val content = s"$lhsCtx->set$fieldName($rhsCtx)"
    GeneratorUtils.mergeGeneratorContexts(Vector(lhsCtx, rhsCtx), givenContent = content)
  }

  private def prepareSetterExpression(): (GeneratorContext, Field) = {
    lhs match {
      case ChainedExpression(p +: IndexedSeq()) => p match {
        case NestedExpression(_, _, _, vn, f, _) => f match {
          case f: Field => (vn, f)
        }
      }
      case ChainedExpression(init :+ last) => last match {
        case NestedExpression(_, _, _, _, f, _) => f match {
          case f: Field => (ChainedExpression(init).generate, f)
        }
      }
    }
  }

  private def isSetterField = findSetterField.isDefined

  private def findSetterField = lhs match {
    case ChainedExpression(IndexedSeq()) => None
    case ChainedExpression(p +: IndexedSeq()) => p match {
      case NestedExpression(_, _, _, vn, f, _) => f match {
        case f: Field if vn.nonEmpty && f.isProperty => Some(f)
        case _ => None
      }
    }
    case ChainedExpression(path) => path.last match {
      case NestedExpression(_, _, _, _, f, _) => f match {
        case f: Field => if (f.isProperty) Some(f) else None
        case _ => None
      }
      case _ => None
    }
    case _ => None
  }
}
