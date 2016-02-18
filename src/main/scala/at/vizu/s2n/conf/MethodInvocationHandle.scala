package at.vizu.s2n.conf

import at.vizu.s2n.generator.GeneratorContext
import at.vizu.s2n.types.symbol.{TScope, TType, TypeUtils}

import scala.collection.mutable.ArrayBuffer

/**
  * Phil on 04.12.15.
  */
class MethodInvocationHandle(val methodName: String) {

  private[conf] var miwps: ArrayBuffer[MethodInvocationWithParams] = ArrayBuffer()

  def withParams(paramTypes: String*) = MethodInvocationWithParams(this, paramTypes)

  private[conf] def add(miwp: MethodInvocationWithParams) = miwps += miwp

  def build() = miwps.toSeq

}

class ClassInvocations(val className: String) {

  private[conf] var invocations: ArrayBuffer[MethodInvocationHandle] = ArrayBuffer()

  def withInvocation(invocation: MethodInvocationHandle) = invocations += invocation

  def build() = invocations.map(i => i.methodName -> i).toMap.mapValues(_.build())
}

case class MethodInvocationWithParams(methodIncov: MethodInvocationHandle, paramTypes: Seq[String]) {

  private[conf] var invocation: (String, Seq[String]) => GeneratorContext = null

  def handleAs(func: (String, Seq[String]) => GeneratorContext) = {
    invocation = func
    methodIncov.add(this)
  }

}

case class MethodInvocationHandleConfig(classInvocations: Seq[ClassInvocations]) {

  type ClassInvocationMap = Map[String, Map[String, Seq[MethodInvocationWithParams]]]

  val cInvocations: ClassInvocationMap = buildMap()

  private def buildMap() = classInvocations.map(ci => ci.className -> ci.build()).toMap

  def findInvocationHandle(scope: TScope, className: String,
                           methodName: String, params: Seq[TType]): (String, Seq[String]) => GeneratorContext = {
    val cName = if (className.isEmpty) "__root__" else className
    findInvocationHandleOpt(scope, cName, methodName, params).get
  }

  def hasInvocationHandle(scope: TScope, className: String, methodName: String, params: Seq[TType]): Boolean = {
    findInvocationHandleOpt(scope, className, methodName, params).isDefined
  }

  private def findInvocationHandleOpt(scope: TScope, className: String, methodName: String, params: Seq[TType]) = {
    val cName = if (className.isEmpty) "__root__" else className
    cInvocations.get(cName)
      .flatMap(_.get(methodName))
      .flatMap(_.find(mi => checkIfParamsAreSame(scope, mi.paramTypes, params)))
      .map(_.invocation)
  }

  private def checkIfParamsAreSame(scope: TScope, paramStrings: Seq[String], actualParams: Seq[TType]): Boolean = {
    if (paramStrings.size != actualParams.size) false
    else {
      val definedParams = TypeUtils.findClasses(scope, paramStrings)
      TypeUtils.areParamsApplicable(definedParams, actualParams)
    }
  }
}