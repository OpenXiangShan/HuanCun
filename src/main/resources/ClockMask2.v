module ClockMask2 (
    output reg clk_out,
    input clk_in
);

    reg clk_mask;
    initial clk_mask = 1'b0;
    always @(posedge clk_in) begin
        clk_mask = ~clk_mask; // Must use =, NOT <=
    end
    assign clk_out = clk_in & clk_mask;

endmodule // ClockMask2