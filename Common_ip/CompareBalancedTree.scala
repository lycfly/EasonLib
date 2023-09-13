package EasonLib.Common_ip

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.language.postfixOps

trait CompareOpFunction {
  def comp(A:UInt, B:UInt) = Bool()
}
object MaxCompFunc extends CompareOpFunction{
  override def comp(A: UInt, B: UInt) = {
    A >= B
  }
}
object MinCompFunc extends CompareOpFunction{
  override def comp(A: UInt, B: UInt) = {
    A <= B
  }
}
object OlderCompFunc extends CompareOpFunction{ // for RobId compare
  override def comp(A: UInt, B: UInt) = {
    val old_out = Bool()
    when(A.msb === B.msb) {
      old_out := A <= B
    }.otherwise {
      old_out := A >= B
    }
    old_out
  }
}
object CompareBalancedTree {
  def apply(Name: String = "", op: CompareOpFunction, use_valid:Boolean, dataIn: Vec[UInt], validIn: Vec[Bool]): Vec[Data] = {
    val findMaxOrMin_unit = new CompareBalancedTree(op.comp,use_valid, WIDTH = dataIn(0).getWidth, NUM = dataIn.length)
    if (Name.isEmpty) {}
    else
      findMaxOrMin_unit.setName(Name)
    findMaxOrMin_unit.io.data_in := dataIn
    if(use_valid) {
      findMaxOrMin_unit.io.valid_in := validIn
      Vec(findMaxOrMin_unit.io.find_out, findMaxOrMin_unit.io.index_out, findMaxOrMin_unit.io.all_invalid)
    }
    else{
      Vec(findMaxOrMin_unit.io.find_out, findMaxOrMin_unit.io.index_out)
    }
  }
}

/**
  * Find max/min/custum_op value in data vec, return index and compare result.
  * this module will generate a binary tree to do the compare function.
  * @param op : the compare function between every couple of element in Input vec. Max/Min for example.
  * @param WIDTH : Input data Bit Width.
  * @param NUM : Input Vec element num.
  */
class CompareBalancedTree(op: (UInt,UInt) => Bool, use_valid:Boolean, WIDTH: Int, NUM: Int) extends Component {
  val io = new Bundle {
    val data_in = in(Vec(UInt(WIDTH bits), NUM))
    lazy val valid_in = in(Vec(Bool(), NUM))
    val find_out = out UInt(WIDTH bits)
    val index_out = out UInt(log2Up(NUM) bits)
    lazy val all_invalid = out Bool()
  }
  noIoPrefix()
  val STAGE_NUM = log2Up(NUM)
  val stages = ArrayBuffer[Vec[UInt]]()
  val index_stages = ArrayBuffer[Vec[UInt]]()
  lazy val valid_stages = ArrayBuffer[Vec[Bool]]()
  if(use_valid) {
    valid_stages += io.valid_in
  }


  stages += io.data_in
  index_stages += Vec(for(i <- 0 until NUM) yield U(i, log2Up(NUM) bits))
  for(i <- 0 until STAGE_NUM){
    val nextStageNum = stages(i).length / 2 + (stages(i).length % 2)
    val nextStageNode = Vec(UInt(WIDTH bits), nextStageNum)
    val nextIndexNodes = Vec(UInt(log2Up(NUM) bits), nextStageNum)
    lazy val nextValidNode = Vec(Bool(), nextStageNum)

    nextStageNode.setName("nextStageNode"+ i.toString)
    nextIndexNodes.setName("nextIndexNodes"+ i.toString)
    for (j <- 0 until nextStageNum) {
      if (j < nextStageNum - 1 || i == STAGE_NUM-1) {  //last stage should also use this logic
        val isOp = op(stages(i)(j * 2), stages(i)(j * 2 + 1))
        if(use_valid){
          nextValidNode(j) := valid_stages(i)(j * 2) | valid_stages(i)(j * 2 + 1)
          when(valid_stages(i)(j * 2) === True && valid_stages(i)(j * 2 + 1) === False){ // valid = 0,1
            nextStageNode(j) := stages(i)(j * 2)
            nextIndexNodes(j) := index_stages(i)(j * 2)
          }.elsewhen(valid_stages(i)(j * 2) === False && valid_stages(i)(j * 2 + 1) === True) { // valid = 1,0
            nextStageNode(j) := stages(i)(j * 2 + 1)
            nextIndexNodes(j) := index_stages(i)(j * 2 + 1)
          }.otherwise{
            nextStageNode(j) := isOp ? stages(i)(j * 2) | stages(i)(j * 2 + 1)
            nextIndexNodes(j) := isOp ? index_stages(i)(j * 2) | index_stages(i)(j * 2 + 1)
          }
        }
        else{
          nextStageNode(j) := isOp ? stages(i)(j * 2) | stages(i)(j * 2 + 1)
          nextIndexNodes(j) := isOp ? index_stages(i)(j * 2) | index_stages(i)(j * 2 + 1)
        }
      }
      else {
        if (stages(i).length % 2 != 0) { // last node odd case
          if(use_valid) {
            nextValidNode(j) := valid_stages(i)(j * 2)
          }
          nextStageNode(j) := stages(i)(j * 2)
          nextIndexNodes(j) := index_stages(i)(j * 2)
        }
        else{  // last node even case
          val isOp = op(stages(i)(j * 2), stages(i)(j * 2 + 1))
          if (use_valid) {
            nextValidNode(j) := valid_stages(i)(j * 2) | valid_stages(i)(j * 2 + 1)
            when(valid_stages(i)(j * 2) === True && valid_stages(i)(j * 2 + 1) === False) { // valid = 0,1
              nextStageNode(j) := stages(i)(j * 2)
              nextIndexNodes(j) := index_stages(i)(j * 2)
            }.elsewhen(valid_stages(i)(j * 2) === False && valid_stages(i)(j * 2 + 1) === True) { // valid = 1,0
              nextStageNode(j) := stages(i)(j * 2 + 1)
              nextIndexNodes(j) := index_stages(i)(j * 2 + 1)
            }.otherwise {
              nextStageNode(j) := isOp ? stages(i)(j * 2) | stages(i)(j * 2 + 1)
              nextIndexNodes(j) := isOp ? index_stages(i)(j * 2) | index_stages(i)(j * 2 + 1)
            }
          }
          else {
            nextStageNode(j) := isOp ? stages(i)(j * 2) | stages(i)(j * 2 + 1)
            nextIndexNodes(j) := isOp ? index_stages(i)(j * 2) | index_stages(i)(j * 2 + 1)
          }
        }
      }
    }
    if(use_valid){
      valid_stages += nextValidNode
    }
    index_stages += nextIndexNodes
    stages += nextStageNode
  }

  io.find_out := stages(STAGE_NUM)(0)
  io.index_out := index_stages(STAGE_NUM)(0)
  if(use_valid){
    io.all_invalid := ~valid_stages(STAGE_NUM)(0)
  }

}

object CompareBalancedTree_inst {
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
      targetDirectory = "rtl_gen")
      .addStandardMemBlackboxing(blackboxAll)
      .generate(new CompareBalancedTree(OlderCompFunc.comp,true, WIDTH = 8, NUM = 7))
  }.printPruned()
}