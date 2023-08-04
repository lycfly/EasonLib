from cocotb.triggers import FallingEdge, RisingEdge, Timer, Edge
from cocotb.binary import BinaryValue

"""
  single port sram driver
  support both binary string and integer assignment
"""
async def spram_driver(dut, mem_name, membank, io_rd, io_wr, io_addr, io_wdata, io_rdata):
    while True:
        await RisingEdge(dut.clk)
        if io_wr.value.integer:  # write
            membank[io_addr.value.integer] = io_wdata.binstr if type(membank[io_addr.value.integer])==str else io_wdata.value.integer
            dut._log.info('SPRAM {} write in to address: {}, data: {}'.format(mem_name, io_addr.value.integer,
                                                                            io_wdata.value.integer))
        elif io_rd.value.integer:
            rddata = int(membank[io_addr.value.integer])
            io_rdata.value = BinaryValue(rddata) if type(rddata)==str else rddata
            dut._log.info('SPRAM {} read out from address: {}, data: {}'.format(
                mem_name, io_addr.value.integer, rddata))
            
async def sprom_driver(dut, mem_name, membank, io_rd, io_addr, io_rdata):
    while True:
        await RisingEdge(dut.clk)
        if io_rd.value.integer:
            rddata = int(membank[io_addr.value.integer])
            io_rdata.value = BinaryValue(rddata) if type(rddata)==str else rddata
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
            rddata = int(membank[io_cmd_addr.value.integer])
            io_rsp_rdata.value = BinaryValue(rddata) if type(rddata)==str else rddata
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
            
            io_rsp_rdata.value = BinaryValue(rddata) if type(rddata)==str else rddata
            dut._log.info('SPROM {} read out from address: {}, data: {}'.format(
                    mem_name, io_cmd_addr.value.integer, rddata))