module STD_CLKGT_func (
  input  wire TE,
  input  wire E,
  input  wire CK,
  output wire Q,
  input  wire dft_l3dataram_clk,
  input  wire dft_l3dataramclk_bypass
);

  wire clk_en;
  reg  clk_en_reg;

  assign clk_en = E | TE;

  always @(posedge CK) 
    begin
      clk_en_reg = clk_en;
    end

  assign Q = CK & clk_en_reg;

endmodule // Copy from Xihu
