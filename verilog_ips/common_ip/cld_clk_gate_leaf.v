
module cld_clk_gate_leaf (
    input  clk_i,
    input  test_en_i,
    input  en_i,
    output gclk_o
);

  reg latch_r;

  always_latch @(*) begin : latch
    if (clk_i == 1'b0) begin
        latch_r = en_i | test_en_i;
    end
  end

  assign gclk_o = clk_i & latch_r;

endmodule