
module cld_clk_gate(
    input clk_i,
    input dft_mode_test_mode_i,
    input dft_mode_scan_mode_i,
    input dft_mode_scan_shift_i,
    input dft_mode_mbist_mode_i,
    input en_i,
    output gclk_o
  );

  cld_clk_gate_leaf u_clk_gate_leaf (
                      .clk_i    (clk_i                ),
                      .test_en_i(dft_mode_scan_shift_i),
                      .en_i     (en_i                 ),
                      .gclk_o   (gclk_o               )
                    );

endmodule
