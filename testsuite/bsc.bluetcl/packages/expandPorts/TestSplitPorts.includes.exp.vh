// autocreated by expandPorts.tcl 2.0 and portUtil.tcl 2.0

  wire   CLK;
  wire   RST_N;
  // ====================
  // Method = read
  //   ready  => RDY_read               1   Bit#(1)
  //   result => read_byte              8   Bit#(8)
  //   result => read_nibble            4   Bit#(4)
  wire   RDY_read;
  wire   [ 7 : 0 ] read_byte;
  wire   [ 3 : 0 ] read_nibble;

  // ====================
  // Method = exchange
  //   ready  => RDY_exchange           1   Bit#(1)
  //   enable => EN_exchange            1   Bit#(1)
  //   result => exchange_byte          8   ActionValue#(Bit#(8))
  //   result => exchange_nibble        4   ActionValue#(Bit#(4))
  //   input  => exchange_2_byte        8   Bit#(8)
  //   input  => exchange_2_nibble      4   Bit#(4)
  //   input  => exchange_3             3   Bit#(3)
  wire   RDY_exchange;
  wire   EN_exchange;
  wire   [ 7 : 0 ] exchange_byte;
  wire   [ 3 : 0 ] exchange_nibble;
  wire   [ 7 : 0 ] exchange_2_byte;
  wire   [ 3 : 0 ] exchange_2_nibble;
  wire   [ 2 : 0 ] exchange_3;

  // ====================
  // Method = zero
  //   ready  => RDY_zero               1   Bit#(1)
  wire   RDY_zero;


`ifndef __EXPANDPORTS_NO_RENAMES__
`endif

`ifndef __EXPANDPORTS_DONT_CREATE_DUT__
  mkTestSplitPorts _mkTestSplitPorts ( 
   .CLK( CLK ),
   .RST_N( RST_N ),
   .RDY_read( RDY_read ),
   .read_byte( read_byte ),
   .read_nibble( read_nibble ),
   .RDY_exchange( RDY_exchange ),
   .EN_exchange( EN_exchange ),
   .exchange_byte( exchange_byte ),
   .exchange_nibble( exchange_nibble ),
   .exchange_2_byte( exchange_2_byte ),
   .exchange_2_nibble( exchange_2_nibble ),
   .exchange_3( exchange_3 ),
   .RDY_zero( RDY_zero )
  );
`endif
