package EasonLib.Common_ip.clk_gate

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random
import scala.language.postfixOps
object clkGate {
  def apply(name: String, isFunction: Boolean, clkin: Bool, rstn: Bool, scan_en: Bool, enable: Bool) = {
    if(isFunction){
      val ckgt_domain = ClockDomain(clkin, rstn)
     val ckgt_cell = ckgt_domain(new clkGate_func()).setName(name)
      ckgt_cell.io.scan_en := scan_en
      ckgt_cell.io.enable := enable
      ckgt_cell.io.clkout
    }
    else{
      val ckgt_cell = new ckgated4_cell().setName(name)
      ckgt_cell.io.clkin := clkin
      ckgt_cell.io.scan_en := scan_en
      ckgt_cell.io.enable := enable
      ckgt_cell.io.clkout
    }
    }

  }


class ckgated4_cell() extends BlackBox { //创建接口模块
  val io = new Bundle {
    val clkin = in Bool()
    val scan_en = in Bool()
    val enable = in Bool()
    val clkout = out Bool()
  }
  noIoPrefix()
//  mapClockDomain(clock = io.clk)
}
class clkGate_func() extends Component { //创建接口模块
  val io = new Bundle {
    val scan_en = in Bool()
    val enable = in Bool()
    val clkout = out Bool()
  }
  noIoPrefix()
  val ck_latch = LatchWhen(io.scan_en | io.enable, ~clockDomain.readClockWire).addAttribute("verilator lint_off LATCH")
  io.clkout := ck_latch & clockDomain.readClockWire

}
//class clkGate(vendor: Vendor) extends Component{ //创建接口模块
//  val io = new Bundle{
//    val clk  = in Bool()
//    val tse  = in Bool()
//    val cgen  = in Bool()
//    val cgclk = out Bool()
//  }
//  vendor.Build(this)
//}
//implicit class ClockGateExtend(cd: ClockDomain){
//  def gateBy(en: Bool, tse: Bool): ClockDomain = {
//    val cg = new clkGate(globalData.getVendor)
//    cg.io.clk:= cd.readClockWire
//    cg.io.tse:= tse
//    cg.io.cgen := en
//    val cde = ClockDomain(clock = cg.io.cgclk,
//      reset = cd.readResetWire
//    )
//    cde.setSynchronousWith(cd) //gate后的时钟和原时钟应该是同步关系
//    cde
//  }
//}

