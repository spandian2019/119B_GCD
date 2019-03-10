-----------------------------------------------------------------------------
--
--  GCD systolic implementation constants package
--
--  This package defines constants for systolic array implementation of
--  GCD computer
--
--  Revision History
--      3/07/19   Sundar Pandian    initial revision
--      3/07/19   Sundar Pandian    Added constants for testing support
--
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package constants is

    ---------------------------
    -- USER FILLED CONSTANTS --
    ---------------------------
    constant N_BITS     : integer := 16;    -- number of bits on inputs
                                            -- USER INPUTTED
    constant CTRSIZE    : integer := 4;     -- bit length of zero counter 
                                            --  CTRSIZE = log_2(N_BITS)
                                            -- USER INPUTTED

    ---------------------
    -- ADDER CONSTANTS --
    ---------------------
    -- increments zero counter
    constant INC_Z_CTR  : std_logic_vector(CTRSIZE-1 downto 0) := (0      => '1',
                                                                   others => '0');
    -- decrements zero counter (performs addition of 2s complement of 1)
    -- 2s complement of 1 is all 1s
    constant DEC_Z_CTR  : std_logic_vector(CTRSIZE-1 downto 0) := (others => '1');
    -- adding in 1 with flipped B value is equivalent of performing 2s complement of B
    constant B_NEG      : std_logic_vector(N_BITS-1 downto 0) := (0      => '1',
                                                                  others => '0');
    -- adding in 1 with flipped B value is equivalent of performing 2s complement of T
    constant T_NEG      : std_logic_vector(N_BITS   downto 0) := (0      => '1',
                                                                  others => '0');
    -- carry in constants
    constant CARRY_RST  : std_logic := '0';
    constant CARRY_SET  : std_logic := '1';

    ---------------------------
    -- SUB_LOOP PE CONSTANTS --
    ---------------------------
    -- zero check value for subloop temp value
    constant T_ZERO     : std_logic_vector(N_BITS downto 0) := (others => '0');
    -- sign flag constants
    constant SF_NEG     : std_logic := '1';
    constant SF_POS     : std_logic := '0';

    ------------------------
    -- SHIFT PE CONSTANTS --
    ------------------------
    -- zero check value for zero counter
    constant CTR_ZERO   : std_logic_vector(CTRSIZE-1 downto 0) := (others => '0');
    -- zero check value for B operand
    constant B_ZERO     : std_logic_vector(N_BITS-1 downto 0) := (others => '0');

    ------------------------
    -- SYSARRAY CONSTANTS --
    ------------------------
    -- size constants
    -- only need to shift B-1 times
    constant SHIFTSIZE  : integer := N_BITS-1;
    -- steins algorithm has an efficiency of 3b for the inner subloop
    constant SUBSIZE    : integer := 3*N_BITS;
    -- total runtime with 2 shift blocks and an inner subloop
    constant RUNTIME    : integer := 2*SHIFTSIZE+SUBSIZE;

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
        X           : in       std_logic_vector((bitsize-1) downto 0); -- addend X
        Y           : in       std_logic_vector((bitsize-1) downto 0); -- addend Y
        Ci          : in       std_logic; -- carry in
        S           : out      std_logic_vector((bitsize-1) downto 0); -- sum out
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

    signal carry : std_logic_vector(bitsize downto 0); -- intermediate carries

begin

    carry(0) <= Ci;                            -- put carry in into our carry vector

    Adders: for i in 0 to bitsize-1 generate          -- generate bitsize full adders
    begin
        FAx: FullAdder port map (X(i), Y(i), carry(i), carry(i + 1), S(i));
    end generate;

    Co <= carry(bitsize);                      -- carry out is from carry vector

end archAdder;
