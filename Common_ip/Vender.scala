package EasonLib.Common_ip
//
//import EasonLib.Common_ip.clk_gate.clkGate
//
//trait Vendor {
//  //默认使用umc的wrapper
////  def build(mw: Ram1rw): MemBlackBox = new umc.mbb1rw(mw).Build()
////  def build(mw: Ram1r1w): MemBlackBox = new umc.mbb1r1w(mw).Build()
////  def build(mw: Ram2rw): MemBlackBox = new umc.mbb2rw(mw).Build()
////  def build(mw: Rom): MemBlackBox = new umc.mbbrom(mw).Build()
//  def build(ckgt: clkGate) = new clkGate()
//
//}

//case object Intel extends Vendor{
////  override def build(mw: Ram1rw): MemBlackBox = new intel.mbb1rw(mw).Build()
//
//}
//对于不同的公司不同的工艺可以定义不同的Wrapper，前提extends Vendor
//case object SPRD extends Vendor{...}
//case object FishSemi extends Vendor{...}
//case object AMD extends Vendor{...}
//case object HuaWei extends Vendor{...}
//case object HuaWei10nm extends Vendor{...}
//case object ZTE extends Vendor{...}
//case object Invida extends Vendor{...}