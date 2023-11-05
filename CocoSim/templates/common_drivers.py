from cocotb.triggers import FallingEdge, RisingEdge, Timer, Edge
from cocotb.binary import BinaryValue,BinaryRepresentation
import numpy as np
from Binary import Binary
"""
  single port sram driver
  support both binary string and integer assignment
"""
async def spram_driver(dut, mem_name, membank, io_rd, io_wr, io_addr, io_wdata, io_rdata, is_classic, use_str = False):
    while True:
        await RisingEdge(dut.clk)
        if is_classic:
            wr = io_wr.value.integer & io_rd.value.integer
            rd = ~io_wr.value.integer & io_rd.value.integer
        else:
            wr = io_wr.value.integer
            rd = io_rd.value.integer
        if wr:  # write
            membank[io_addr.value.integer] = BinaryValue(io_wdata.value.integer).binstr if use_str else io_wdata.value.integer
            dut._log.info('SPRAM {} write in to address: {}, data: {}'.format(mem_name, io_addr.value.integer,
                                                                            io_wdata.value.integer))
        elif rd:
            rddata = membank[io_addr.value.integer]
            io_rdata.value = BinaryValue(rddata, bigEndian=False) if type(rddata)==str else rddata
            dut._log.info('SPRAM {} read out from address: {}, data: {}'.format(
                mem_name, io_addr.value.integer, rddata))
            
async def sprom_driver(dut, mem_name, membank, io_rd, io_addr, io_rdata):
    while True:
        await RisingEdge(dut.clk)
        if io_rd.value.integer:
            rddata = membank[io_addr.value.integer]
            io_rdata.value = BinaryValue(rddata, bigEndian=False) if type(rddata)==str else rddata
            dut._log.info('ROM {} read out from address: {}, data: {}'.format(
                mem_name, io_addr.value.integer, rddata))

async def stream_spram_driver(dut, mem_name, membank, 
                              io_cmd_valid, io_cmd_wr, io_cmd_addr, io_cmd_ready, io_cmd_wdata,
                              io_rsp_valid, io_rsp_ready, io_rsp_rdata):
    while True:
        await RisingEdge(dut.clk)
        fire = io_cmd_valid.value.integer & io_cmd_ready.value.integer
        io_cmd_ready = 1
        io_rsp_valid = 0
        if  fire and io_cmd_wr.value.integer == 1:  # write
            membank[io_cmd_addr.value.integer] = io_cmd_wdata.binstr if type(membank[io_cmd_addr.value.integer])==str else io_cmd_wdata.value.integer
            dut._log.info('SPRAM {} write in to address: {}, data: {}'.format(mem_name, io_cmd_addr.value.integer,
                                                                            io_cmd_wdata.value.integer))
        elif fire and io_cmd_wr.value.integer == 0:
            io_rsp_valid = 1
            rddata = membank[io_cmd_addr.value.integer]
            io_rsp_rdata.value = BinaryValue(int('0b'+rddata,2)) if type(rddata)==str else rddata
            dut._log.info('SPRAM {} read out from address: {}, data: {}'.format(
                mem_name, io_cmd_addr.value.integer, rddata))
            
async def stream_sprom_driver(dut,mem_name, membank, 
                              io_cmd_valid, io_cmd_addr, io_cmd_ready,
                              io_rsp_valid, io_rsp_ready, io_rsp_rdata):
    while True:
        await RisingEdge(dut.clk)
        io_cmd_ready.value = 1
        io_rsp_valid.value = 0
        fire = io_cmd_valid.value.integer & io_cmd_ready.value.integer
        if fire and io_rsp_ready.value.integer:
            io_rsp_valid.value = 1
            rddata = membank[io_cmd_addr.value.integer]
            
            io_rsp_rdata.value = BinaryValue(int('0b'+rddata,2)) if type(rddata)==str else rddata
            dut._log.info('SPROM {} read out from address: {}, data: {}'.format(
                    mem_name, io_cmd_addr.value.integer, rddata))
            

def array2mem(array):
    flattened = np.array(array).flatten()
    return flattened.tolist()



def gather_array_element(array, gather_ratio, n_bits = 8, signed = True):
    binary_func = Binary(n_bits=n_bits)
    flattened = np.array(array).flatten()
    array_size = flattened.shape[0]
    assert(array_size % gather_ratio == 0, "gather_ratio is not suitable!")
    gathered_len = int(array_size / gather_ratio)
    gathered = np.zeros([gathered_len]).tolist()
    # binary_represent = BinaryRepresentation.TWOS_COMPLEMENT if signed else BinaryRepresentation.UNSIGNED
    for i in range(gathered_len):
        line = ''
        for j in range(gather_ratio):
            if signed:
                ele = binary_func._convert_to_twos_comp(int(flattened[i*gather_ratio+j]))
            else:
                ele = binary_func._convert_to_unsigned(int(flattened[i*gather_ratio+j]))

            # print("{},{},{},{}".format(i,j,ele.value,int(flattened[i*gather_ratio+j])))
            line =  ele + line
            # print(line)
        assert(len(line)==n_bits*gather_ratio, "Binary gather error! Width mismatch!")
        # gathered[i] = BinaryValue(int('0b'+line,2), n_bits=gather_ratio*n_bits, binaryRepresentation=binary_represent).value
        # gathered[i] = BinaryValue(line,n_bits=n_bits*gather_ratio, binaryRepresentation=binary_represent).value
        gathered[i] = int('0b'+line,2)
    return gathered

def invert(string):
    tmp = ''
    for i in string:
        if i == '0':
            tmp += '1'
        elif i == '1':
            tmp += '0'
        else:
            raise("Error invert")
    return tmp

def split_mem_element(mem, gather_ratio, n_bits = 8, signed = True):
    binary_func = Binary(n_bits=n_bits*gather_ratio)
    mem_len = len(mem)
    split_len = int(mem_len * gather_ratio)
    split = np.zeros([split_len], dtype=np.int32).tolist()
    # binary_represent = BinaryRepresentation.TWOS_COMPLEMENT if signed else BinaryRepresentation.UNSIGNED
    for i in range(mem_len):
        mem_ele = mem[i]
        for j in range(gather_ratio):
            array_ele = binary_func._adjust_unsigned(bin(mem_ele)[2:])
            array_ele = array_ele[(gather_ratio-j-1)*n_bits:((gather_ratio-j))*n_bits]
            if signed:
                if array_ele[0] == '1':
                    array_ele = int('0b'+invert(array_ele),2) + 1
                    array_ele = -array_ele
                    split[i*gather_ratio + j] = array_ele
                    continue
            split[i*gather_ratio + j] = int('0b'+array_ele,2)
            # print(line)
        # gathered[i] = BinaryValue(int('0b'+line,2), n_bits=gather_ratio*n_bits, binaryRepresentation=binary_represent).value
        

    return split


