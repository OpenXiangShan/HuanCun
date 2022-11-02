module STD_CLKGT_func (
  input  wire TE,
  input  wire E,
  input  wire CK,
  output wire Q
);

`ifdef SYNTHESIS
  // Please include ICG cell here.
`else

  wire clk_en;
  reg  clk_en_reg;

  assign clk_en = E | TE;

`ifdef VERILATOR
  always @(posedge CK) begin
    clk_en_reg <= clk_en;
  end
`else
  always_latch begin
    if (!CK) begin
      clk_en_reg <= clk_en;
    end
  end
`endif // VERILATOR

  assign Q = CK & clk_en_reg;

`endif // SYNTHESIS

endmodule
