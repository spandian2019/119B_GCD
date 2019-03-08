-----------------------------------------------------------------------------
--
--  Control Unit constants package
--
--  This package defines control unit constants for supported AVR
--  instructions
--
--  Revision History
--      1/30/19   Sundar Pandian    initial revision
--      02/01/19  Sundar Pandian    Debugged with testbench
--      02/06/19  Sundar Pandian    Added the Mux entities
--
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package constants is

    ---------------------------
    -- USER FILLED CONSTANTS --
    ---------------------------
    constant N_BITS     : integer := 16;    -- number of bits on inputs
    constant CTRSIZE    : integer := 4;     -- bit length of zero counter 
                                            --  CTRSIZE = log_2(N_BITS)



end package constants;

----------------------------------------------------------------------------
--
--  1 Bit Full Adder
--
--  Implementation of a full adder. This entity takes the one bit
--  inputs A and B with a carry in input and outputs the sum and carry
--  out bits, using combinational logic.
--
-- Inputs:
--      A: std_logic - 1 bit adder input
--      B: std_logic - 1 bit adder input
--      Cin: std_logic - 1 bit carry in input
--
-- Outputs:
--      Sum: std_logic - 1 bit sum of A, B, and Cin
--      Cout: std_logic - 1 bit carry out value
--
--  Revision History:
--      03/07/19  Sundar Pandian    Initial revision.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fullAdder is
    port(
        A           :  in      std_logic;  -- adder input
        B           :  in      std_logic;  -- adder input
        Cin         :  in      std_logic;  -- carry in value
        Cout        :  out     std_logic;  -- carry out value
        Sum         :  out     std_logic   -- sum of A, B with carry in
      );
end fullAdder;

architecture fullAdder of fullAdder is
    begin
        -- combinational logic for calculating A+B with carry in and out bits
        Sum <= A xor B xor Cin;
        Cout <= (A and B) or (A and Cin) or (B and Cin);
end fullAdder;


----------------------------------------------------------------------------
--
--  n Bit Full Adder
--
--  Implementation of an n bit full adder. This entity takes the 2 n bit
--  inputs X and Y with a carry in input and outputs the sum and carry
--  out bits, using combinational logic.
--
-- Inputs:
--      Y: std_logic_vector - n bit adder input
--      X: std_logic_vector - n bit adder input
--      Cin: std_logic - 1 bit carry in input
--
-- Outputs:
--      Sum: std_logic_vector - n bit sum of X, Y, and Cin
--      Cout: std_logic - 1 bit carry out value
--
--  Revision History:
--      03/07/19  Sundar Pandian    Initial revision.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Adder is generic ( bitsize : integer := 8); -- default width is 8-bits
    port (
        X           : in       std_logic_vector((bitsize–1) downto 0); -- addend X
        Y           : in       std_logic_vector((bitsize–1) downto 0); -- addend Y
        Ci          : in       std_logic; -- carry in
        S           : out      std_logic_vector((bitsize–1) downto 0); -- sum out
        Co          : out      std_logic -- carry out
      );
end Adder;

architecture archAdder of Adder is

    component fullAdder is
        port(
            A           :  in      std_logic;  -- adder input
            B           :  in      std_logic;  -- adder input
            Cin         :  in      std_logic;  -- carry in value
            Cout        :  out     std_logic;  -- carry out value
            Sum         :  out     std_logic   -- sum of A, B with carry in
          );
    end component;

    signal carry : bit_vector(bitsize downto 0); -- intermediate carries

begin

    carry(0) <= Ci;                            -- put carry in into our carry vector

    Adders: for i in bitsize generate          -- generate bitsize full adders
    begin
        FAx: FullAdder port map (X(i), Y(i), carry(i), S(i), carry(i + 1));
    end generate;

    Co <= carry(bitsize);                      -- carry out is from carry vector

end archAdder;
