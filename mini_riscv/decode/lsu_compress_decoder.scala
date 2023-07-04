package EasonLib.mini_riscv.decode

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

class lsu_compress_decoder(WIDTH: Int = 32) extends Component {
  val io = new Bundle {
    val comp_dec = master(compress_dec_if(WIDTH))
  }
  noIoPrefix()
  val rvc_op_s = Bits(16 bits)
  val rvc_dec_s = Bits(WIDTH bits)
  io.comp_dec.instr_type := io.comp_dec.instr_in(1 downto 0).andR
  rvc_op_s := io.comp_dec.instr_in(15 downto 0)
  io.comp_dec.instr_out := io.comp_dec.instr_type ? io.comp_dec.instr_in | rvc_dec_s

  rvc_dec_s := 0
  switch(rvc_op_s(1 downto 0)) {
    is(B"2'b00") {
      switch(rvc_op_s(15 downto 13)) {
        is(B"3'b010") { //c.lw
          rvc_dec_s := B"5'd0" ## rvc_op_s(5) ## rvc_op_s(12 downto 10) ## rvc_op_s(6) ## B"2'd0" ##
            B"2'b01" ## rvc_op_s(9 downto 7)## B"3'b010" ## B"2'b01" ## rvc_op_s(4 downto 2) ## B"7'b0000011"
        }
        is(B"3'b110") { //c.sw
          rvc_dec_s := B"5'd0" ## rvc_op_s(5) ## rvc_op_s(12) ## B"2'b01" ## rvc_op_s(4 downto 2) ## B"2'b01" ##
            rvc_op_s(9 downto 7)## B"3'b010" ## rvc_op_s(11 downto 10) ## rvc_op_s(6) ## B"2'b00" ## B"7'b0100011"
        }
      }
    }
    is(B"2'b10"){
      switch(rvc_op_s(15 downto 13)) {
        is(B"3'b010") { //c.lwsp
          rvc_dec_s := B"4'd0" ## rvc_op_s(3 downto 2) ## rvc_op_s(12) ## rvc_op_s(6 downto 4) ## B"2'd0" ##
            B"5'd2" ## B"3'b010" ## rvc_op_s(11 downto 7) ## B"7'b0000011"
        }
        is(B"3'b110") { //c.sw
          rvc_dec_s := B"4'd0" ## rvc_op_s(8 downto 7) ## rvc_op_s(12) ## rvc_op_s(6 downto 2) ##
            B"5'd2" ## B"3'b010" ## rvc_op_s(11 downto 9) ## B"2'b00" ## B"7'b0100011"
        }
      }
    }
  }
}

object lsu_compress_decoder_inst {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode = Verilog,
      oneFilePerComponent = false,
      nameWhenByFile = false,
      inlineConditionalExpression = true,
      enumPrefixEnable = false,
      anonymSignalPrefix = "tmp",
      targetDirectory = "rtl")
      .addStandardMemBlackboxing(blackboxAll)
      .generate(new lsu_compress_decoder())
  }.printPruned()
}