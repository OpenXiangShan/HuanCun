module ClockMask2 (
    output clk_out,
    input clk_in,
    input reset
);

    reg clk_mask;
    always @(posedge clk_in) begin
        if (reset) begin
          clk_mask = 0;
        end
        else begin
          clk_mask = ~clk_mask; // Must use =, NOT <=
        end
    end
    assign clk_out = clk_in & clk_mask;

endmodule // ClockMask2
