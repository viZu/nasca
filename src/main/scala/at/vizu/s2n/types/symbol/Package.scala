package at.vizu.s2n.types.symbol

/**
 * Phil on 21.10.15.
 */
case class Package(name: String) {

  private var types: Seq[TType] = Vector()
  private var subPackages: Seq[Package] = Vector()

  def addType(tpe: TType): Unit = {
    val pkgSequence: List[String] = tpe.name.split(".").toList
    addType(pkgSequence, tpe)
  }

  private def addType(pkgSequence: List[String], tpe: TType): Unit = {
    pkgSequence match {
      case head :: tail => findPackage(head).get.addType(tail, tpe)
      case Nil => types = types :+ tpe
    }
  }

  private def findPackage(name: String) = {
    subPackages.find(_.name == name) orElse createPackage(name)
  }

  private def createPackage(name: String) = {
    val pkg: Package = Package(name)
    subPackages = subPackages :+ pkg
    Some(pkg)
  }

}
