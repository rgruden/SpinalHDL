package spinal.lib.misc.aia

import spinal.core._
import spinal.lib._

case class APlicMsiParam(
  base: BigInt = 0,
  hhxs: Int = 0,
  lhxs: Int = 0,
  hhxw: Int = 0,
  lhxw: Int = 0
)

case class APlicGenParam(withDirect: Boolean,
                         withMSI: Boolean,
                         genIEP: Boolean = true,
                         withIForce: Boolean = false,
                         var _MMsiParams: APlicMsiParam = APlicMsiParam(),
                         var _SMsiParams: APlicMsiParam = APlicMsiParam(),
                         var _withMsiAddrcfg: Boolean = false,
                         var _lockMSI: Boolean = false) {

  def lockMSI(): this.type = {
    this._lockMSI = true
    this
  }

  def withMsiAddrcfg(): this.type = {
    this._withMsiAddrcfg = true
    this
  }

  def withMachineMsiParams(param: APlicMsiParam): this.type = {
    this._MMsiParams = param
    this
  }

  def withMachineMsiParams(address: BigInt = 0, hhxs: Int = 0, lhxs: Int = 0, hhxw: Int = 0, lhxw: Int = 0): this.type = {
    withMachineMsiParams(APlicMsiParam(base = address, hhxs = hhxs, lhxs = lhxs, hhxw = hhxw, lhxw = lhxw))
  }

  def withSupervisorMsiParams(param: APlicMsiParam): this.type = {
    this._SMsiParams = param
    this
  }

  def withSupervisorMsiParams(address: BigInt = 0, lhxs: Int = 0): this.type = {
    withSupervisorMsiParams(APlicMsiParam(base = address, lhxs = lhxs))
  }
}

object APlicGenParam {
  def test = APlicGenParam(
    withDirect  = true,
    withMSI     = true,
    genIEP      = true,
    withIForce  = true
  )

  def full = APlicGenParam(
    withDirect  = true,
    withMSI     = true,
    genIEP      = true
  )

  def msi = APlicGenParam(
    withDirect  = false,
    withMSI     = true
  )

  def direct = APlicGenParam(
    withDirect  = true,
    withMSI     = false
  )
}

case class APlicDomainParam(isRoot: Boolean,
                            isMDomain: Boolean,
                            genParam: APlicGenParam)

object APlicDomainParam {
  def root(genParam: APlicGenParam) = APlicDomainParam(
    isRoot    = true,
    isMDomain = true,
    genParam  = genParam
  )

  def M(genParam: APlicGenParam) = APlicDomainParam(
    isRoot    = false,
    isMDomain = true,
    genParam  = genParam
  )

  def S(genParam: APlicGenParam) = APlicDomainParam(
    isRoot    = false,
    isMDomain = false,
    genParam  = genParam
  )
}

case class APlicChildInfo(childIdx: Int, sourceIds: Seq[Int])

case class APlicMsiPayload() extends Bundle {
  val address = UInt(64 bits)
  val data = UInt(32 bits)
}

trait APlicMsiProducerFiber extends Nameable{
  def createMsiStreamProducer(): Stream[APlicMsiPayload]
}

trait APlicMsiConsumerFiber extends Nameable{
  def createMsiStreamConsumer(): Stream[APlicMsiPayload]
}

case class APlic(p: APlicDomainParam,
                 sourceParams: Seq[APlicSourceParam],
                 hartIds: Seq[Int],
                 childInfos: Seq[APlicChildInfo]) extends Area {
  val sourceIds = sourceParams.map(_.id)
  require(sourceIds.distinct.size == sourceIds.size, "APlic requires no duplicate interrupt source")
  require(hartIds.distinct.size == hartIds.size, "APlic requires no duplicate harts")

  val sources = Bits(sourceIds.size bits)
  val childSources = Vec(childInfos.map(childInfo => Bits(childInfo.sourceIds.size bits)))
  val mmsiaddrcfg = UInt(64 bits)
  val smsiaddrcfg = UInt(64 bits)

  val childInterruptIds = childInfos.flatMap(childInfo => childInfo.sourceIds).distinct
  val interruptDelegatable = for (sourceId <- sourceIds) yield childInterruptIds.find(_ == sourceId).isDefined

  val deliveryEnable = RegInit(False)
  val isMSI = False
  val bigEndian = False

  val interrupts: Seq[APlicSource] = for (((param, delegatable), i) <- sourceParams.zip(interruptDelegatable).zipWithIndex)
    yield APlicSource(param, APlicSourceState(delegatable, isMSI, sources(i)))

  val childMappings = for ((childInfo, childSource) <- childInfos.zip(childSources)) yield new Area {
    for ((childSourceId, idx) <- childInfo.sourceIds.zipWithIndex) yield new Area {
      interrupts.find(_.id == childSourceId).map(interrupt => new Area {
        when(interrupt.delegated && (Bool(childInfos.size == 1) || interrupt.childIdx === childInfo.childIdx)) {
          childSource(idx) := interrupt.input
        } otherwise {
          childSource(idx) := False
        }
      })
    }
  }

  val direct = new Area {
    val gateways = for (hartId <- hartIds) yield new APlicDirectGateway(interrupts, hartId, p.genParam.withIForce)

    val targets = Mux(isMSI, B(0), gateways.map(_.iep && deliveryEnable).asBits())
  }
}

object APlic {
  def doWhenMatch(interrupts: Seq[APlicSource], id: UInt, func: APlicSource => Unit) = new Area {
    switch(id) {
      for (interrupt <- interrupts) {
        is (interrupt.id) {
          func(interrupt)
        }
      }
    }
  }

  def doClaim(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doClaim())

  def doSet(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doSet())

  def doEnable(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doEnable())

  def doDisable(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doDisable())
}
