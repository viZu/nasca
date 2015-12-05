package at.vizu.s2n.conf

import at.vizu.s2n.generator.GeneratorContext
import at.vizu.s2n.generator.handles.IncludeHandle

/**
  * Phil on 03.12.15.
  */
object GlobalConfig {

  lazy val invocationConfig: MethodInvocationHandleConfig = initInvocationConfig()

  private def initInvocationConfig() = {
    val root = new ClassInvocations("__root__") {
      withInvocation(new MethodInvocationHandle("println") {
        withParams("scala.Any") handleAs { (params) =>
          getPrintCtx( s"""std::cout << "${params.head}" << std::endl""")
        }
        withParams() handleAs {
          getPrintCtx("std::cout << std::endl")
        }
      })

      withInvocation(new MethodInvocationHandle("print") {
        withParams("scala.Any") handleAs { (params) =>
          getPrintCtx( s"""std::cout << "${params.head}"""")
        }
      })
    }
    MethodInvocationHandleConfig(Vector(root))
  }

  private def getPrintCtx(content: String): GeneratorContext = {
    GeneratorContext(content, Vector(IncludeHandle("<iostream>")))
  }
}
