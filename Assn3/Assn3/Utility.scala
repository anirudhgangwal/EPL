package Assignment3.Utility

object Utility {
  type Variable = String

  // This object provides a method to generate a "fresh" variable name
  object Gensym {
    private var id = 0
    def gensym(s: Variable): Variable = {
      val fresh_s = s + "_" + id
      id = id + 1
      fresh_s
    }
  }

  def swapVar(x: Variable, y: Variable, z: Variable): Variable = {
    if (x == y) {
      z
    } else if (x == z) {
      y
    } else {
      x
    }
  }
}



// vim: set ts=2 sw=2 et sts=2:
