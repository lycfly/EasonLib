import re
import numpy as np


def tobin(a):
    return bin(a)[2:]


def dec2bin_ext_clip(a, width):
    assert (width <= 32)
    return zero_extend(signed_dec2bin(a))[32 - width:32]


def signed_dec2bin(a):
    if a < 0:
        absa = abs(a)
        nega = np.array(~absa + 1).astype('uint32')
        return (bin(nega)[2:])
    else:
        return (bin(a)[2:])


def zero_extend(s, width=32):
    if len(s) < width:
        return (width - len(s)) * '0' + s
    else:
        return s


def Uint2Sint(num, bits):
    sign_mask = 1 << (bits - 1)
    inv_mask = (1 << (bits)) - 1
    if ((num & sign_mask) == 0):
        return num
    else:
        num = num ^ inv_mask
        num = num + 1
        return -num


def NpUint2Sint(x, bits):
    """
    Change a signed/unsigned np.array to unsigned/signed
    :param x: np.array
    :param bits: max bit width of data in x
    :return: signed/unsigned array of x
    """
    sign_mask = 1 << (bits - 1)
    inv_mask = (1 << (bits)) - 1
    x = x.astype(np.int32)
    pos_mask = (x & sign_mask) == 0
    return np.where(pos_mask, x, -((x ^ inv_mask) + 1))


def toggle_element2memline(ele_array, ele_width=8):
    ele_num = ele_array.shape[0]
    line_width = int(ele_width * ele_num)
    line = ''
    for e in ele_array:
        line += dec2bin_ext_clip(e, ele_width)
    return ('0b' + line, line_width)
