from cocotb.triggers import FallingEdge, RisingEdge, Timer, Edge

async def spram_driver(dut, mem_name, membank, io_rd, io_wr, io_addr, io_wdata, io_rdata):
    while True:
        await RisingEdge(dut.clk)
        if io_wr.value.integer:  # write
            membank[io_addr.value.integer] = io_wdata.value.integer
            dut._log.info('SPRAM {} write in to address: {}, data: {}'.format(mem_name, io_addr.value.integer,
                                                                            io_wdata.value.integer))
        elif io_rd.value.integer:
            rddata = int(membank[io_addr.value.integer])
            io_rdata.value = rddata
            dut._log.info('SPRAM {} read out from address: {}, data: {}'.format(
                mem_name, io_addr.value.integer, rddata))
            
async def sprom_driver(dut, mem_name, membank, io_rd, io_addr, io_rdata):
    while True:
        await RisingEdge(dut.clk)
        if io_rd.value.integer:
            rddata = int(membank[io_addr.value.integer])
            io_rdata.value = rddata
            dut._log.info('ROM {} read out from address: {}, data: {}'.format(
                mem_name, io_addr.value.integer, rddata))
