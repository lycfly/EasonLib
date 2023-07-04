package EasonLib.mini_riscv

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

object INST_FIELD {
  def opRange = 6 downto 0

  def funct7Range = 31 downto 25

  def rdRange = 11 downto 7

  def funct3Range = 14 downto 12

  def rs1Range = 19 downto 15

  def rs2Range = 24 downto 20

  def rs3Range = 31 downto 27

  def csrRange = 31 downto 20

}

object OP_TYPE {
  def branch = B"7'b1100011"

  def load = B"7'b0000011"

  def store = B"7'b0100011"

  def alu_imm = B"7'b0010011"

  def alu_reg = B"7'b0110011"

  def system = B"7'b1110011"

}
object RVIMCA{

  def ADD = M"0000000----------000-----0110011"

  def SUB = M"0100000----------000-----0110011"

  def SLL = M"0000000----------001-----0110011"

  def SLT = M"0000000----------010-----0110011"

  def SLTU = M"0000000----------011-----0110011"

  def XOR = M"0000000----------100-----0110011"

  def SRL = M"0000000----------101-----0110011"

  def SRA = M"0100000----------101-----0110011"

  def OR = M"0000000----------110-----0110011"

  def AND = M"0000000----------111-----0110011"

  def ADDI = M"-----------------000-----0010011"

  def SLLI = M"0000000----------001-----0010011"

  def SLTI = M"-----------------010-----0010011"

  def SLTIU = M"-----------------011-----0010011"

  def XORI = M"-----------------100-----0010011"

  def SRLI = M"0000000----------101-----0010011"

  def SRAI = M"0100000----------101-----0010011"

  def ORI = M"-----------------110-----0010011"

  def ANDI = M"-----------------111-----0010011"

  def LB = M"-----------------000-----0000011"

  def LH = M"-----------------001-----0000011"

  def LW = M"-----------------010-----0000011"

  def LBU = M"-----------------100-----0000011"

  def LHU = M"-----------------101-----0000011"

  def SB = M"-----------------000-----0100011"

  def SH = M"-----------------001-----0100011"

  def SW = M"-----------------010-----0100011"

  def LR = M"00010--00000-----010-----0101111"

  def SC = M"00011------------010-----0101111"

  def AMOSWAP = M"00001------------010-----0101111"

  def AMOADD = M"00000------------010-----0101111"

  def AMOXOR = M"00100------------010-----0101111"

  def AMOAND = M"01100------------010-----0101111"

  def AMOOR = M"01000------------010-----0101111"

  def AMOMIN = M"10000------------010-----0101111"

  def AMOMAX = M"10100------------010-----0101111"

  def AMOMINU = M"11000------------010-----0101111"

  def AMOMAXU = M"11100------------010-----0101111"

  def BEQ(rvc: Boolean) = if (rvc) M"-----------------000-----1100011" else M"-----------------000---0-1100011"

  def BNE(rvc: Boolean) = if (rvc) M"-----------------001-----1100011" else M"-----------------001---0-1100011"

  def BLT(rvc: Boolean) = if (rvc) M"-----------------100-----1100011" else M"-----------------100---0-1100011"

  def BGE(rvc: Boolean) = if (rvc) M"-----------------101-----1100011" else M"-----------------101---0-1100011"

  def BLTU(rvc: Boolean) = if (rvc) M"-----------------110-----1100011" else M"-----------------110---0-1100011"

  def BGEU(rvc: Boolean) = if (rvc) M"-----------------111-----1100011" else M"-----------------111---0-1100011"

  def JALR = M"-----------------000-----1100111"

  def JAL(rvc: Boolean) = if (rvc) M"-------------------------1101111" else M"----------0--------------1101111"

  def LUI = M"-------------------------0110111"

  def AUIPC = M"-------------------------0010111"

  def MULX = M"0000001----------0-------0110011"

  def DIVX = M"0000001----------1-------0110011"

  def MUL = M"0000001----------000-----0110011"

  def MULH = M"0000001----------001-----0110011"

  def MULHSU = M"0000001----------010-----0110011"

  def MULHU = M"0000001----------011-----0110011"


  def DIV = M"0000001----------100-----0110011"

  def DIVU = M"0000001----------101-----0110011"

  def REM = M"0000001----------110-----0110011"

  def REMU = M"0000001----------111-----0110011"


  def CSRRW = M"-----------------001-----1110011"

  def CSRRS = M"-----------------010-----1110011"

  def CSRRC = M"-----------------011-----1110011"

  def CSRRWI = M"-----------------101-----1110011"

  def CSRRSI = M"-----------------110-----1110011"

  def CSRRCI = M"-----------------111-----1110011"

  def ECALL = M"00000000000000000000000001110011"

  def EBREAK = M"00000000000100000000000001110011"

  def FENCEI = M"00000000000000000001000000001111"

  def MRET = M"00110000001000000000000001110011"

  def SRET = M"00010000001000000000000001110011"

  def WFI = M"00010000010100000000000001110011"
}
object BIT_MANIPULATION {
  //zba
  def ADD_UW               = M"0000100----------000-----0111011"
  def SH1ADD               = M"0010000----------010-----0110011"
  def SH1ADD_UW            = M"0010000----------010-----0111011"
  def SH2ADD               = M"0010000----------100-----0110011"
  def SH2ADD_UW            = M"0010000----------100-----0111011"
  def SH3ADD               = M"0010000----------110-----0110011"
  def SH3ADD_UW            = M"0010000----------110-----0111011"
  def SLLI_UW              = M"0000010----------001-----0011011"

  //zbb
  def ANDN                 = M"0100000----------111-----0110011"
  def CLZ                  = M"011000000000-----001-----0010011"
  def CLZW                 = M"011000000000-----001-----0011011"
  def CPOP                 = M"011000000010-----001-----0010011"
  def CPOPW                = M"011000000010-----001-----0011011"
  def CTZ                  = M"011000000001-----001-----0010011"
  def CTZW                 = M"011000000001-----001-----0011011"
  def MAX                  = M"0000101----------110-----0110011"
  def MAXU                 = M"0000101----------111-----0110011"
  def MIN                  = M"0000101----------100-----0110011"
  def MINU                 = M"0000101----------101-----0110011"
  def ORC_B                = M"001010000111-----101-----0010011"
  def ORN                  = M"0100000----------110-----0110011"
  def REV8                 = M"011010011000-----101-----0010011"
  def ROL                  = M"0110000----------001-----0110011"
  def ROLW                 = M"0110000----------001-----0111011"
  def ROR                  = M"0110000----------101-----0110011"
  def RORI                 = M"0110000----------101-----0010011"
  def RORIW                = M"0110000----------101-----0011011"
  def RORW                 = M"0110000----------101-----0111011"
  def SEXT_B               = M"011000000100-----001-----0010011"
  def SEXT_H               = M"011000000101-----001-----0010011"
  def XNOR                 = M"0100000----------100-----0110011"
  def ZEXT_H               = M"000010000000-----100-----0110011"


  //zbc
  def CLMUL                = M"0000101----------001-----0110011"
  def CLMULH               = M"0000101----------011-----0110011"
  def CLMULR               = M"0000101----------010-----0110011"

  //zbs
  def BCLR                 = M"0100100----------001-----0110011"
  def BCLRI                = M"0100100----------001-----0010011"
  def BEXT                 = M"0100100----------101-----0110011"
  def BEXTI                = M"0100100----------101-----0010011"
  def BINV                 = M"0110100----------001-----0110011"
  def BINVI                = M"0110100----------001-----0010011"
  def BSET                 = M"0010100----------001-----0110011"
  def BSETI                = M"0010100----------001-----0010011"


}
case class IMM(instruction: Bits) extends Area {
  def u = instruction(31 downto 12) ## U"x000"

  def z = instruction(19 downto 15)

  // sign-extend immediates
  def i_sext = B((19 downto 0) -> i(11)) ## i

  // immediates
  def i = instruction(31 downto 20)

  def h_sext = B((23 downto 0) -> h(7)) ## h

  def h = instruction(31 downto 24)

  def s_sext = B((19 downto 0) -> s(11)) ## s

  def s = instruction(31 downto 25) ## instruction(11 downto 7)

  def b_sext = B((18 downto 0) -> b(11)) ## b ## False

  def b = instruction(31) ## instruction(7) ## instruction(30 downto 25) ## instruction(11 downto 8)

  def j_sext = B((10 downto 0) -> j(19)) ## j ## False

  def j = instruction(31) ## instruction(19 downto 12) ## instruction(20) ## instruction(30 downto 21)
}

object CSR_ADDR {
  val MCOUNTEREN = 0x306
  val SSTATUS = 0x100
  val SIE = 0x104
  val STVEC = 0x105
  val SCOUNTEREN = 0x106
  val SSCRATCH = 0x140
  val SEPC = 0x141
  val SCAUSE = 0x142
  val SBADADDR = 0x143
  val SIP = 0x144
  val SATP = 0x180
  val FFLAGS = 0x1
  val FRM = 0x2
  val FCSR = 0x3
  val DCSR = 0x7B0
  val DPC = 0x7B1
  val TSELECT = 0x7A0
  val TDATA1 = 0x7A1
  val TDATA2 = 0x7A2
  val TINFO = 0x7a4
  val TCONTROL = 0x7A5

  def MVENDORID = 0xF11 // MRO Vendor ID.

  def MARCHID = 0xF12 // MRO Architecture ID.

  def MIMPID = 0xF13 // MRO Implementation ID.

  def MHARTID = 0xF14 // MRO Hardware thread ID.Machine Trap Setup

  def MSTATUS = 0x300 // MRW Machine status register.

  def MISA = 0x301 // MRW ISA and extensions

  def MEDELEG = 0x302 // MRW Machine exception delegation register.

  def MIDELEG = 0x303 // MRW Machine interrupt delegation register.

  def MIE = 0x304 // MRW Machine interrupt-enable register.

  def MTVEC = 0x305 // MRW Machine trap-handler base address. Machine Trap Handling

  def MSCRATCH = 0x340 // MRW Scratch register for machine trap handlers.

  def MEPC = 0x341 // MRW Machine exception program counter.

  def MCAUSE = 0x342 // MRW Machine trap cause.

  def MBADADDR = 0x343 // MRW Machine bad address.

  def MIP = 0x344 // MRW Machine interrupt pending.

  def MBASE = 0x380 // MRW Base register.

  def MBOUND = 0x381 // MRW Bound register.

  def MIBASE = 0x382 // MRW Instruction base register.

  def MIBOUND = 0x383 // MRW Instruction bound register.

  def MDBASE = 0x384 // MRW Data base register.

  def MDBOUND = 0x385 // MRW Data bound register.

  def MCYCLE = 0xB00 // MRW Machine cycle counter.

  def MINSTRET = 0xB02 // MRW Machine instructions-retired counter.

  def MCYCLEH = 0xB80 // MRW Upper 32 bits of mcycle, RV32I only.

  def MINSTRETH = 0xB82 // MRW Upper 32 bits of minstret, RV32I only.

  def UCYCLE = 0xC00 // UR Machine ucycle counter.

  def UCYCLEH = 0xC80

  def UTIME = 0xC01 // rdtime

  def UTIMEH = 0xC81

  def UINSTRET = 0xC02 // UR Machine instructions-retired counter.

  def UINSTRETH = 0xC82 // UR Upper 32 bits of minstret, RV32I only.
}