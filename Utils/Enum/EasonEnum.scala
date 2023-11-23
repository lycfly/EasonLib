package EasonLib.Utils.Enum

import spinal.core.SpinalEnum

class EasonEnum extends SpinalEnum{
  def get_element(name: String) = {
    this.elements.filter(_.getName() == name)(0)
  }
}
