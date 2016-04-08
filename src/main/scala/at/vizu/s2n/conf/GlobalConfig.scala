package at.vizu.s2n.conf

import at.vizu.s2n.generator.handles.{AngleWrapper, IncludeHandle}
import at.vizu.s2n.generator.{GeneratorContext, GeneratorUtils}
import at.vizu.s2n.types.symbol._

/**
  * Phil on 03.12.15.
  */
object GlobalConfig {

  val Any = TypeUtils.RootScalaPackage + ".Any"

  lazy val invocationConfig: MethodInvocationHandleConfig = initInvocationConfig()
  lazy val classConfig: ClassHandlesConfig = initClassHandlesConfig()

  private def initInvocationConfig() = {
    val root = new ClassInvocations("__root__") {
      withInvocation(new MethodInvocationHandle("println") {
        withParams(Any) handleAs { (varName, params) =>
          getPrintCtx(s"std::cout << ${params.head} << std::endl")
        }
        withParams() handleAs {
          getPrintCtx("std::cout << std::endl")
        }
      })

      withInvocation(new MethodInvocationHandle("print") {
        withParams(Any) handleAs { (varName, params) =>
          getPrintCtx(s"""std::cout << ${params.head}""")
        }
      })
    }

    val array = new ClassInvocations(TypeUtils.RootScalaPackage + ".Array") {
      withInvocation(new MethodInvocationHandle("length") {
        withParams() handleAs {
          GeneratorContext("->size()")
        }
      })
      withInvocation(new MethodInvocationHandle("apply") {
        withParams(TypeUtils.RootScalaPackage + ".Int") handleAs { (varName, params) =>
          GeneratorContext(s"->at(${params.head})")
        }
      })
      withInvocation(new MethodInvocationHandle("update") {
        //TODO remove Any
        withParams(TypeUtils.RootScalaPackage + ".Int", Any) handleAs { (varName, params) =>
          val first = params.head
          val second = params(1)
          GeneratorContext(s"->at($first) = $second")
        }
      })
    }

    val string = new ClassInvocations(TypeUtils.RootScalaPackage + ".String") {
      withInvocation(new MethodInvocationHandle("contains") {
        withParams(TypeUtils.RootScalaPackage + ".String") handleAs { (varName, params) =>
          GeneratorContext(s".find(${params.head}) != std::string::npos")
        }
      })

      withInvocation(new MethodInvocationHandle("toInt") {
        withParams() handleAs { (varName, params) =>
          GeneratorContext(s"std::stoi($varName)")
        } ignoreVariableUse
      })
    }

    val math = new ClassInvocations(TypeUtils.RootScalaPackage + ".Math") {
      withInvocation(new MethodInvocationHandle("sqrt") {
        withParams(TypeUtils.RootScalaPackage + ".Double") handleAs { (varName, params) =>
          GeneratorContext(s"std::sqrt(${params.head})", Set(IncludeHandle("cmath", AngleWrapper)))
        }
      })

      withInvocation(new MethodInvocationHandle("abs") {
        withParams(TypeUtils.RootScalaPackage + ".Double") handleAs { (varName, params) =>
          GeneratorContext(s"std::abs(${params.head})", Set(IncludeHandle("cmath", AngleWrapper)))
        }
      })

      withInvocation(new MethodInvocationHandle("cos") {
        withParams(TypeUtils.RootScalaPackage + ".Double") handleAs { (varName, params) =>
          GeneratorContext(s"std::cos(${params.head})", Set(IncludeHandle("cmath", AngleWrapper)))
        }
      })

      withInvocation(new MethodInvocationHandle("acos") {
        withParams(TypeUtils.RootScalaPackage + ".Double") handleAs { (varName, params) =>
          GeneratorContext(s"std::acos(${params.head})", Set(IncludeHandle("cmath", AngleWrapper)))
        }
      })

      withInvocation(new MethodInvocationHandle("sin") {
        withParams(TypeUtils.RootScalaPackage + ".Double") handleAs { (varName, params) =>
          GeneratorContext(s"std::sin(${params.head})", Set(IncludeHandle("cmath", AngleWrapper)))
        }
      })

      withInvocation(new MethodInvocationHandle("asin") {
        withParams(TypeUtils.RootScalaPackage + ".Double") handleAs { (varName, params) =>
          GeneratorContext(s"std::asin(${params.head})", Set(IncludeHandle("cmath", AngleWrapper)))
        }
      })

      withInvocation(new MethodInvocationHandle("hypot") {
        withParams(TypeUtils.RootScalaPackage + ".Double", TypeUtils.RootScalaPackage + ".Double") handleAs { (varName, params) =>
          GeneratorContext(s"std::hypot(${params.head}, ${params(1)})", Set(IncludeHandle("cmath", AngleWrapper)))
        }
      })
    }

    MethodInvocationHandleConfig(Vector(root, array, string, math))
  }

  private def getParams(classes: String*): Seq[String] = {
    classes.map(TypeUtils.RootScalaPackage + "." + _)
  }

  private def initClassHandlesConfig() = {
    new ClassHandlesConfig {
      addClassRenamingHandle(new ClassRenamingHandle {
        withMatcher(t => TypeUtils.isFunctionType(t))
        withRename(new Renamer((b, t) => {
          t match {
            case a: AppliedGenericType =>
              getFuncCtx(b, a.appliedTypes)
            case g: GenericType =>
              getFuncCtx(b, g.genericModifiers)
          }
        }))
        withIncludeHandle(IncludeHandle("functional", AngleWrapper))
      })
      addClassRenamingHandle(new ClassRenamingHandle {
        withMatcher(t => t.fullClassName == TypeUtils.RootScalaPackage + ".Array")
        withRename(new Renamer((b, t) => {
          val typeArgs = t match {
            case a: AppliedGenericType =>
              GeneratorUtils.generateTypeArgs(b, a.appliedTypes)
            case g: GenericType =>
              GeneratorUtils.generateTypeArgs(b, g.genericModifiers)
          }
          typeArgs.enhance(s"std::vector$typeArgs", Set(this.includeHandle))
        }, (b, t) => {
          val typeArgs = t match {
            case a: AppliedGenericType =>
              GeneratorUtils.generateTypeArgs(b, a.appliedTypes)
            case g: GenericType =>
              GeneratorUtils.generateTypeArgs(b, g.genericModifiers)
          }
          typeArgs.enhance(s"std::shared_ptr<std::vector$typeArgs>", Set(this.includeHandle))
        }))
        withIncludeHandle(IncludeHandle("vector", AngleWrapper))
      })
      addClassRenamingHandle(new ClassRenamingHandle {
        withMatcher(t => t.fullClassName == TypeUtils.RootScalaPackage + ".Math")
        withRename(new Renamer((b, t) => ""))
        withIncludeHandle(IncludeHandle("cmath", AngleWrapper))
      })
    }
  }

  private def getFuncCtx(b: BaseTypes, types: Seq[TType]): GeneratorContext = {
    val includeHandle = IncludeHandle("functional", AngleWrapper)
    val retMod = types.last
    val retStr = GeneratorUtils.generateCppTypeName(b, retMod)
    val params = GeneratorUtils.generateParamsStringWithTypes(b, types.init)
    GeneratorUtils.mergeGeneratorContexts(Vector(retStr, params), givenContent = s"std::function<$retStr($params)>") + includeHandle
  }

  private def getPrintCtx(content: String): GeneratorContext = {
    GeneratorContext(content, Set(IncludeHandle("iostream", AngleWrapper)))
  }
}
