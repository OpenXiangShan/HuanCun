package chisel3

object chisel3Hacks {
  def getDataDirection(target: Data, isFlipped: Boolean): Int = {
    target.specifiedDirection match {
      case chisel3.SpecifiedDirection.Unspecified => 0
      case chisel3.SpecifiedDirection.Flip => 1
      case chisel3.SpecifiedDirection.Output => if (isFlipped) 3 else 2
      case chisel3.SpecifiedDirection.Input => if (isFlipped) 2 else 3
    }
  }
}
