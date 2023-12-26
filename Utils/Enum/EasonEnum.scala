package EasonLib.Utils.Enum

import spinal.core._

class EasonEnumElement[T <: SpinalEnum](val myEnum: T, val myPosition: Int) extends SpinalEnumElement[T](myEnum,myPosition){
  def toM() = {
    val value = myEnum.defaultEncoding.getValue(this)
    val width = myEnum.defaultEncoding.getWidth(myEnum)
    new MaskedLiteral(value, BigInt(2).pow(width) - 1, width)
  }
  def toU() = {
    val value = myEnum.defaultEncoding.getValue(this)
    val width = myEnum.defaultEncoding.getWidth(myEnum)
    U(value, width bits)
  }

  def toInt() = {
    val value = myEnum.defaultEncoding.getValue(this)
    value
  }
}

class EasonEnum extends SpinalEnum{
  override def newElement(): EasonEnumElement[EasonEnum.this.type] = newElement(null)
  override def newElement(name: String): EasonEnumElement[this.type] = {
    val v = new EasonEnumElement(this, elements.size).asInstanceOf[EasonEnumElement[this.type]]
    if (name != null) v.setName(name)
    elements += v
    v
  }
  def get_element(name: String) = {
    this.elements.filter(_.getName() == name)(0)
  }

  //  def toM(enum: SpinalEnumElement[this.type]) = {
  //    val value = this.defaultEncoding.getValue(enum)
  //    val width = this.defaultEncoding.getWidth(this)
  //    new MaskedLiteral(value, BigInt(2).pow(width) - 1, width)
  //  }

}
