val v = Vector(1, 2, 3)

v match {
  case xs +: rest +: haha => rest
  case _ => 0
}