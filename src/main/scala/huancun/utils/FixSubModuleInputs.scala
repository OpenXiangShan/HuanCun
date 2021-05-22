package huancun.utils

import firrtl._
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.stage.TransformManager.TransformDependency

class FixSubModuleInputs extends Transform with DependencyAPIMigration {
  override def prerequisites: Seq[TransformDependency] = firrtl.stage.Forms.Resolved
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(
    Dependency(firrtl.passes.CheckInitialization)
  )
  override protected def execute(state: CircuitState): CircuitState = {

    def findModule(name: String) = state.circuit.modules.find(m => m.name == name)

    type Ref = Expression with RefLikeExpression

    def getFlow(parent: Ref, field: Field) = (parent.flow, field.flip) match {
      case (SourceFlow, Flip) => SinkFlow
      case (SinkFlow, Flip)   => SourceFlow
      case _                  => parent.flow
    }

    def isReset(parent: Ref): Boolean = parent match {
      case SubField(expr, name, tpe, SinkFlow) => if (name == "reset") true else false
      case _                                   => false
    }

    def getAllSinks(t: Type, parent: Ref): Seq[Expression] = {
      t match {
        case BundleType(fields) =>
          fields.flatMap(f => getAllSinks(f.tpe, SubField(parent, f.name, f.tpe, getFlow(parent, f))))
        case VectorType(tpe, size) =>
          Seq
            .tabulate(size) { i =>
              val elem = SubIndex(parent, i, tpe, parent.flow)
              getAllSinks(elem.tpe, elem)
            }
            .flatten
        case UIntType(width) if parent.flow == SinkFlow && !isReset(parent) =>
          Seq(parent)
        case SIntType(width) =>
          Seq(parent)
        case ClockType | ResetType =>
          Nil
        case _ => Nil
      }
    }

    def onStmt(s: Statement): Statement = {
      val newStmt = s match {
        case inst: DefInstance =>
          val ref = Reference(inst)
          val mod = findModule(inst.module).get
          val ports = mod.ports
          val conns = ports.flatMap { p =>
            val f = p.direction match {
              case Input  => SinkFlow
              case Output => SourceFlow
            }
            val sinks = getAllSinks(p.tpe, SubField(ref, p.name, p.tpe, f))
            sinks
          }.map { sink =>
            val src = sink.tpe match {
              case _: UIntType => UIntLiteral(0)
              case _: SIntType => SIntLiteral(0)
            }
            Connect(inst.info, sink, src)
          }
          Block(inst +: conns)
        case other: Statement =>
          other.mapStmt(onStmt)
      }
      newStmt
    }
    def onModule(m: DefModule): DefModule = {
      m.mapStmt(onStmt)
    }
    val c = state.circuit.mapModule(onModule)
    state.copy(c)
  }
}
