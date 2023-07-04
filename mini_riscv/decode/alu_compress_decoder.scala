package EasonLib.mini_riscv.decode

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps


case class compress_dec_if(WIDTH: Int = 32) extends Bundle with IMasterSlave {
  val instr_in = Bits(WIDTH bits)
  val instr_type = Bool() // 0: 16bit, 1: 32bit
  val instr_out = Bits(WIDTH bits)

  override def asMaster(): Unit = {
    in(instr_in)
    out(instr_type)
    out(instr_out)
  }
}


class alu_compress_decoder(WIDTH: Int = 32) extends Component {
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
        is(B"3'b000") {
          when(rvc_op_s(4 downto 2) =/= 0 && rvc_op_s(12 downto 5) =/= 0) { //c.addi4spn
            rvc_dec_s := B"2'd0" ## rvc_op_s(10 downto 7) ## rvc_op_s(12 downto 11) ## rvc_op_s(5) ## rvc_op_s(6) ##
              B"2'd0" ## B"5'd2" ## B"3'd0" ## B"2'b01" ## rvc_op_s(4 downto 2) ## B"7'b0010011"
          }
        }
      }
    }
    is(B"2'b01") {
      switch(rvc_op_s(15 downto 13)) {
        is(B"3'b000") {
          when(rvc_op_s(12 downto 2) === B"11'd0") { //c.nop
            rvc_dec_s := B"25'd0" ## B"7'b0010011"
          }.elsewhen(rvc_op_s(12) | (rvc_op_s(6 downto 2) =/= 0)) { //c.addi
            rvc_dec_s := rvc_op_s(12) #* 7 ## rvc_op_s(6 downto 2) ## rvc_op_s(11 downto 7) ## B"3'd0" ## rvc_op_s(11 downto 7) ## B"7'b0010011"
          }
        }
        is(B"3'b001") { // c.jal
          rvc_dec_s := rvc_op_s(12) ## rvc_op_s(8) ## rvc_op_s(10 downto 9) ## rvc_op_s(6) ## rvc_op_s(7) ## rvc_op_s(2) ##
            rvc_op_s(11) ## rvc_op_s(5 downto 3) ## rvc_op_s(12) ## (rvc_op_s(12) #* 8) ## B"5'd1" ## B"7'b1101111"
        }
        is(B"3'b010") { // c.li
          when(rvc_op_s(11 downto 7) =/= 0) {
            rvc_dec_s := rvc_op_s(12) #* 7 ## rvc_op_s(6 downto 2) ## B"5'd0" ## B"3'd0" ## rvc_op_s(11 downto 7) ## B"7'b0010011"
          }
        }
        is(B"3'b011") {
          when(rvc_op_s(12) | (rvc_op_s(6 downto 2) =/= 0 && rvc_op_s(11 downto 7) =/= 0)) {
            when(rvc_op_s(11 downto 7) === B"5'd2") { // c.addi16sp
              rvc_dec_s := rvc_op_s(12) #* 3 ## rvc_op_s(4) ## rvc_op_s(3) ## rvc_op_s(5) ## rvc_op_s(2) ## rvc_op_s(6) ##
                B"4'd0" ## B"5'd2" ## B"3'd0" ## B"5'd2" ## B"7'b0010011"
            }.otherwise { // c.lui
              rvc_dec_s := rvc_op_s(12) #* 15 ## rvc_op_s(6 downto 2) ## rvc_op_s(11 downto 7) ## B"7'b0110111"
            }
          }
        }
        is(B"3'b100") {
          when(rvc_op_s(12 downto 10) === B"3'b011") {
            when(rvc_op_s(6 downto 5) === B"2'b00") { //c.sub
              rvc_dec_s := B"7'b0100000" ## B"2'b01" ## rvc_op_s(4 downto 2) ## B"2'b01" ## rvc_op_s(9 downto 7) ## B"3'b000" ##
                B"2'b01" ## rvc_op_s(9 downto 7) ## B"7'b0110011"
            }.elsewhen(rvc_op_s(6 downto 5) === B"2'b01") { //c.xor
              rvc_dec_s := B"7'b0000000" ## B"2'b01" ## rvc_op_s(4 downto 2) ## B"2'b01" ## rvc_op_s(9 downto 7) ## B"3'b100" ##
                B"2'b01" ## rvc_op_s(9 downto 7) ## B"7'b0110011"
            }.elsewhen(rvc_op_s(6 downto 5) === B"2'b10") { //c.or
              rvc_dec_s := B"7'b0000000" ## B"2'b01" ## rvc_op_s(4 downto 2) ## B"2'b01" ## rvc_op_s(9 downto 7) ## B"3'b110" ##
                B"2'b01" ## rvc_op_s(9 downto 7) ## B"7'b0110011"
            }.otherwise { //c.and
              rvc_dec_s := B"7'b0000000" ## B"2'b01" ## rvc_op_s(4 downto 2) ## B"2'b01" ## rvc_op_s(9 downto 7) ## B"3'b111" ##
                B"2'b01" ## rvc_op_s(9 downto 7) ## B"7'b0110011"
            }
          }.elsewhen(rvc_op_s(11 downto 10) === B"2'b10"){  //c.andi
            rvc_dec_s := rvc_op_s(12) #* 7 ## rvc_op_s(6 downto 2) ## B"2'b01" ## rvc_op_s(9 downto 7) ## B"3'b111" ## B"2'b01" ##
              rvc_op_s(9 downto 7) ## B"7'b0010011"
          }.elsewhen(rvc_op_s(11 downto 10) === B"2'b00"){  //c.srli
            rvc_dec_s := B"7'b0000000" ## rvc_op_s(6 downto 2) ## B"2'b01" ## rvc_op_s(9 downto 7) ## B"3'b101" ##
                B"2'b01" ## rvc_op_s(9 downto 7) ## B"7'b0010011"
          }.elsewhen(rvc_op_s(11 downto 10) === B"2'b01"){  //c.srai
            rvc_dec_s := B"7'b0100000" ## rvc_op_s(6 downto 2) ## B"2'b01" ## rvc_op_s(9 downto 7) ## B"3'b101" ##
                B"2'b01" ## rvc_op_s(9 downto 7) ## B"7'b0010011"
          }
        }
        is(B"3'b101"){  //c.j
          rvc_dec_s := rvc_op_s(12) ## rvc_op_s(8) ## rvc_op_s(10 downto 9) ## rvc_op_s(6) ## rvc_op_s(7) ## rvc_op_s(2) ##
            rvc_op_s(11) ## rvc_op_s(5 downto 3) ## rvc_op_s(12) ## (rvc_op_s(12) #* 8) ## B"5'd0" ## B"7'b1101111"
        }
        is(B"3'b110"){  //c.beqz
          rvc_dec_s := (rvc_op_s(12)#*4) ## rvc_op_s(6) ## rvc_op_s(5) ## rvc_op_s(2) ## B"5'd0" ## B"2'b01" ##
            rvc_op_s(9 downto 7) ## B"3'b000" ## rvc_op_s(11) ## rvc_op_s(10) ## rvc_op_s(4) ## rvc_op_s(3) ## rvc_op_s(12) ## B"7'b1100011"
        }
        is(B"3'b111"){  //c.bnez
          rvc_dec_s := rvc_op_s(12)#*4 ## rvc_op_s(6) ## rvc_op_s(5) ## rvc_op_s(2) ## B"5'd0" ## B"2'b01" ##
            rvc_op_s(9 downto 7) ## B"3'b001" ## rvc_op_s(11) ## rvc_op_s(10) ## rvc_op_s(4) ## rvc_op_s(3) ## rvc_op_s(12) ## B"7'b1100011"
        }
      }
    }
    is(B"2'b10") {
      switch(rvc_op_s(15 downto 13)){
        is(B"3'b000"){
          when(rvc_op_s(11 downto 7) =/= 0){ //c.slli
            rvc_dec_s := B"7'b0100000" ## rvc_op_s(6 downto 2) ## rvc_op_s(11 downto 7) ## B"3'b001" ##
              rvc_op_s(11 downto 7) ## B"7'b0010011"
          }
        }
        is(B"3'b100"){
          when(rvc_op_s(6 downto 2) === 0){
            when(rvc_op_s(11 downto 7) === 0){
              when(rvc_op_s(12)){ //c.break
                rvc_dec_s := B"11'd0" ## True ## B"13'd0" ## B"7'b1110011"
              }
            }.otherwise{
              when(rvc_op_s(12)){  //c.jalr
                rvc_dec_s := B"12'd0" ## rvc_op_s(11 downto 7) ## B"3'd0" ## B"5'd1" ## B"7'b1100111"
              }.otherwise{  //c.jr
                rvc_dec_s := B"12'd0" ## rvc_op_s(11 downto 7) ## B"3'd0" ## B"5'd0" ## B"7'b1100111"
              }
            }
          }.otherwise{
            when(rvc_op_s(11 downto 7) === 0){
              when(rvc_op_s(12)) { //c.add
                rvc_dec_s := B"7'b0000000" ## rvc_op_s(6 downto 2) ## rvc_op_s(11 downto 7) ## B"3'b000" ##
                  rvc_op_s(11 downto 7) ## B"7'b0110011"
              }.otherwise{  //c.mv
                rvc_dec_s := B"7'b0000000" ## rvc_op_s(6 downto 2) ## B"5'd0" ## B"3'b000" ##
                  rvc_op_s(11 downto 7) ## B"7'b0110011"
              }
            }
          }
        }
      }
    }
  }

}

object alu_compress_decoder_inst {
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
      .generate(new alu_compress_decoder())
  }.printPruned()
}