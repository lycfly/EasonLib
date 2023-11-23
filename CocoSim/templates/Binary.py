
import os
import random
import re
import warnings

def _clog2(val):
    if val < 0:
        raise ValueError("_clog2 can't take a negative")
    exp = 0
    while True:
        if (1 << exp) >= val:
            return exp
        exp += 1

class BinaryRepr:  # noqa
    UNSIGNED = 0  #: Unsigned format
    TWOS_COMPLEMENT = 1  #: Two's complement format



class Binary:
    def __init__(
        self,
        value=None,
        n_bits=None,
        binaryRepresentation=BinaryRepr.UNSIGNED
    ):
        """
        Args:
            value (str or int or long, optional): Value to assign to the bus.
            n_bits (int, optional): Number of bits to use for the underlying
                binary representation.
            binaryRepr (BinaryRepresentation): The representation
                of the binary value
                (one of :any:`UNSIGNED`, :any:`SIGNED_MAGNITUDE`, :any:`TWOS_COMPLEMENT`).
                Defaults to unsigned representation.
        """
        self._str = ""
        self.binaryRepr = BinaryRepr
        self._n_bits = n_bits

        # self._convert_to = self._convert_to_map[self.binaryRepresentation].__get__(
        #     self, self.__class__
        # )

        # self._convert_from = self._convert_from_map[self.binaryRepresentation].__get__(
        #     self, self.__class__
        # )

        if value is not None:
            self.assign(value)


    def assign(self, value):
        """Decides how best to assign the value to the vector.
        """
        if isinstance(value, int):
            self.integer = value
        elif isinstance(value, str):
            self.binstr = value
        else:
            raise TypeError(
                "value must be int, str, or bytes, not {!r}".format(
                    type(value).__qualname__
                )
            )
        
    def _convert_to_unsigned(self, x):
        if x == 0:
            return self._adjust_unsigned("")
        x = bin(x)
        if x[0] == "-":
            raise ValueError(
                "Attempt to assigned negative number to unsigned " "BinaryValue"
            )
        return self._adjust_unsigned(x[2:])
    
    def _convert_to_twos_comp(self, x):
        if x < 0:
            binstr = bin(2 ** (_clog2(abs(x)) + 1) + x)[2:]
            binstr = self._adjust_twos_comp(binstr)
        elif x == 0:
            binstr = '0'*self._n_bits
        else:
            binstr = self._adjust_twos_comp("0" + bin(x)[2:])
        return binstr

    def _convert_from_unsigned(self, x):
        if not len(x):
            return 0
        return int(x.translate(_resolve_table), 2)


    def _convert_from_twos_comp(self, x):
        if not len(x):
            return 0
        if x[0] == "1":
            binstr = x[1:]
            binstr = self._invert(binstr)
            rv = int(binstr, 2) + 1
            rv = rv * -1
        else:
            rv = int(x.translate(_resolve_table), 2)
        return rv
    
    _convert_to_map = {
        BinaryRepr.UNSIGNED: _convert_to_unsigned,
        BinaryRepr.TWOS_COMPLEMENT: _convert_to_twos_comp,
    }

    _convert_from_map = {
        BinaryRepr.UNSIGNED: _convert_from_unsigned,
        BinaryRepr.TWOS_COMPLEMENT: _convert_from_twos_comp,
    }

    _invert_table = str.maketrans({"0": "1", "1": "0"})

    def _invert(self, x):
        return x.translate(self._invert_table)

    def _adjust_unsigned(self, x):
        if self._n_bits is None:
            return x
        l = len(x)
        if l <= self._n_bits:
            rv = "0" * (self._n_bits - l) + x
        elif l > self._n_bits:
            rv = x[l - self._n_bits :]
            warnings.warn(
                "{}-bit value requested, truncating value {!r} ({} bits) to {!r}".format(
                    self._n_bits, x, l, rv
                ),
                category=RuntimeWarning,
                stacklevel=3,
            )
        return rv
    
    def _adjust_twos_comp(self, x):
        if self._n_bits is None:
            return x
        l = len(x)
        if l == 0:
            rv = x
        elif l < self._n_bits:
            rv = x[0] * (self._n_bits - l) + x
        elif l > self._n_bits:
            rv = x[l - self._n_bits :]
            warnings.warn(
                "{}-bit value requested, truncating value {!r} ({} bits) to {!r}".format(
                    self._n_bits, x, l, rv
                ),
                category=RuntimeWarning,
                stacklevel=3,
            )
        else:
            rv = x
        return rv
    


    @property
    def integer(self):
        """The integer representation of the underlying vector."""
        return self._convert_from(self._str)

    @integer.setter
    def integer(self, val):
        self._str = self._convert_to(val)

    @property
    def value(self):
        """Integer access to the value. **deprecated**"""
        return self.integer

    @value.setter
    def value(self, val):
        self.integer = val

    get_value = value.fget
    set_value = value.fset

    @property
    def signed_integer(self):
        """The signed integer representation of the underlying vector."""
        ival = int(self._str.translate(_resolve_table), 2)
        bits = len(self._str)
        signbit = 1 << (bits - 1)
        if (ival & signbit) == 0:
            return ival
        else:
            return -1 * (1 + (int(~ival) & (signbit - 1)))

    @signed_integer.setter
    def signed_integer(self, val):
        self.integer = val

    get_value_signed = signed_integer.fget

    def signed_trunc(self, x):

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")

            binstr = self._convert_to_twos_comp(x)
            if len(binstr) == 0:
                return 0
            sign = binstr[0] == '1'
            if sign:
                comp_int = -int('0b' + binstr ,2)
                comp_str = self._convert_to_twos_comp(comp_int)
                return -int('0b' + comp_str ,2)
            else:
                return int('0b' + binstr ,2)
            
    def signed_sat(self, x, out_bit):
        sat_bound = 2 ** (out_bit-1)
        if x >= sat_bound - 1:
            return sat_bound - 1
        elif x <= -sat_bound:
            return -sat_bound
        else:
            return x

