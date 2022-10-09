module STD_CLKGT_func (
  input  wire TE,
  input  wire E,
  input  wire CK,
  output wire Q
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
