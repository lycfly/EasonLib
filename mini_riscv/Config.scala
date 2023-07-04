package EasonLib.mini_riscv

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

class Config(){
  implicit def hex2int(hex:String): Int = Integer.parseInt(hex, 16)
  def XLEN: Int = 32
  def ADDRWD: Int = 32
  def FETCH_WIDTH: Int = 64
  def REGNUM: Int = 32
  def INSTR16_NUM: Int = 8
  def FETCH16GROUP: Int = FETCH_WIDTH/16
  def ISSUE_WIDTH: Int = 3

  def IMEM_BASE: Int = "10100000"
  def IMEM_SIZE_KB: Int = 8
  def IMEM_BOUND: Int = IMEM_BASE + IMEM_SIZE_KB * 1024
  def DMEM_BASE: Int = "10000000"
  def DMEM_SIZE_KB: Int = 8
  def DMEM_BOUND: Int = DMEM_BASE + DMEM_SIZE_KB * 1024

}

