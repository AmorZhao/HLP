module top_module;
  bit [0:0] si;
  bit [0:0] si_array [16:0];
  bit [0:0] so;
  bit [0:0] so_array [16:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(38) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"so\", \"Values\": [");
    for(i_=0;i_<16; i_=i_+1) begin $write("%d, ", so_array[i_]); end
    $display("%d]}", so_array[16]);
    $write("]");
    $finish(0);
  end
  initial begin
      si_array[0] = 1'd1;
      si_array[1] = 1'd1;
      si_array[2] = 1'd0;
      si_array[3] = 1'd0;
      si_array[4] = 1'd0;
      si_array[5] = 1'd1;
      si_array[6] = 1'd0;
      si_array[7] = 1'd1;
      si_array[8] = 1'd0;
      si_array[9] = 1'd1;
      si_array[10] = 1'd1;
      si_array[11] = 1'd0;
      si_array[12] = 1'd0;
      si_array[13] = 1'd1;
      si_array[14] = 1'd1;
      si_array[15] = 1'd1;
      si_array[16] = 1'd0;
    for(j_=0; j_<17; j_=j_+1) begin
        si=si_array[j_];
      #0.5;
        so_array[j_]=so;
      @(negedge clk);
end
  end
  shift dut (.si(si), .so(so), .clk(clk));
endmodule