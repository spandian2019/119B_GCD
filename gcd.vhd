----------------------------------------------------------------------------
--
--  Parallel GCD computer
--
--  This file contains all the entities for synthesizing a systolic array
--  implementation for GCD computation. It utilizes Stein's algorithm to
--  to achieve GCD computation every clk (after some initial delay for the 
--  first 5b-2 GCD computations, where b is the number of bits). The system
--  computes the GCD of two non-negative inputs. The GCD systolic array
--  is generalized to take in inputs of b bits, declared in the
--  steins_const.vhd file.
--
--  Algorithm: Stein's Algorithm
--            k = 0
--            while (a is even AND b is even)
--                k ++ 
--                a /= 2
--                b /= 2
--            if (a is odd)
--                t = -b
--            else
--                t = a
--            do
--                while (t is even)
--                    t /= 2
--                if (t > 0)
--                    a = t
--                else
--                    b = -t
--                t = a-b
--            until (t = 0)
--            return a * 2^k
--
--  Edge Cases:
--      When one of the inputs is 0, the other is the GCD
--      When both are 0, the GCD is 0
--
--  Limitations:
--
--  Entities contained:
--      sys_array   : generates linear systolic array of the 3 diff 
--                      PEs to compute GCD
--      in_shift    : shifts out lowest shared 0s from each operand
--                      saves num of 0s to shift back to GCD
--      sub_loop    : loops stein's algorithm
--                      nested while loop implementation
--      out_shift   : shifts back saved num of 0s into GCD
--
--  Revision History:
--     03/7/2019  Sundar Pandian       Initial version
--
----------------------------------------------------------------------------

----------------------------------------------------------------------------
--
--  sys_array
--
--  Implements the systolic array utilizing the PEs declared later in the
--  file. Interconnect is linear (with each PE passing data to the following
--  PE upon each clock rising edge). It utilizes Stein's algorithm to
--  to achieve GCD computation every clk (after some initial delay for the 
--  first 4b GCD computations, where b is the number of bits). The system
--  computes the GCD of two non-negative inputs. The GCD systolic array
--  is generalized to take in inputs of b bits, declared in the
--  steins_const.vhd file. For more specific information on each PE
--  action, read PE headers.
--  
--  in_shift PE is generated b-1 times (for cases where there are b-1 0s in 
--  A and B input operands). In the case where A = B = 0, there is no need
--  to shift again the bth time since the operands (and t) are already
--  ready for the next set of PEs.
-- 
--  sub_loop PE is generated b+2 times (as determined by running through SW
--  implementation). SW implementation showed that there is a max of b+2
--  combined iterations through each of the while loops (sum of execution
--  through both loops).
-- 
--  out_shift PE is generated b-1 times for same reasons as in_shift PE
--  generations. Merely multiplies back factors of 2 that were divided out
--  by in_shift PEs. 
--
--  Inputs:
--      CLK                     - clock signal
--      op_A                    - IN first operand (b bit)
--      op_B                    - IN second operand (b bit)
--
--  Outputs:
--      gcd                     - OUT first operand (b bit)
--
--  Algorithm: Stein's Algorithm
--            k = 0
--            while (a is even AND b is even)
--                k ++ 
--                a /= 2
--                b /= 2
--            if (a is odd)
--                t = -b
--            else
--                t = a
--            do
--                while (t is even)
--                    t /= 2
--                if (t > 0)
--                    a = t
--                else
--                    b = -t
--                t = a-b
--            until (t = 0)
--            return a * 2^k
--
--  Edge Cases:
--      When one of the inputs is 0, the other is the GCD
--      When both are 0, the GCD is 0
--
--  Limitations:
--
--  Entities contained:
--      in_shift    : shifts out lowest shared 0s from each operand
--                      saves num of 0s to shift back to GCD
--      sub_loop    : loops stein's algorithm
--                      nested while loop implementation
--      out_shift   : shifts back saved num of 0s into GCD
--
--  Revision History:
--     03/8/2019  Sundar Pandian       Initial version
--
----------------------------------------------------------------------------

-- libraries
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.numeric_std.all;
use work.constants.all;

entity  sys_array  is

    port (
            CLK             : in    std_logic;                              -- clk signal
            op_A            : in    std_logic_vector(N_BITS-1 downto 0);    -- operand A
            op_B            : in    std_logic_vector(N_BITS-1 downto 0);    -- operand B

            gcd             : out   std_logic_vector(N_BITS-1 downto 0)     -- output
    );

end  sys_array;

--
--  sys_array linear_interconnect architecture
--

architecture  linear_interconnect  of  sys_array  is

component  in_shift  is
    port (
            CLK             : in    std_logic;
            in_A            : in    std_logic_vector(N_BITS-1 downto 0);
            in_B            : in    std_logic_vector(N_BITS-1 downto 0);
            in_zero_ctr     : in    std_logic_vector(CTRSIZE-1 downto 0);

            out_A           : out   std_logic_vector(N_BITS-1 downto 0);
            out_B           : out   std_logic_vector(N_BITS-1 downto 0);
            out_zero_ctr    : out   std_logic_vector(CTRSIZE-1 downto 0);
            t               : out   std_logic_vector(N_BITS downto 0)
    );
end  component;

component  sub_loop  is
    port (
            CLK             : in    std_logic;
            in_A            : in    std_logic_vector(N_BITS-1 downto 0);
            in_B            : in    std_logic_vector(N_BITS-1 downto 0);
            in_zero_ctr     : in    std_logic_vector(CTRSIZE-1 downto 0);
            in_t            : in    std_logic_vector(N_BITS downto 0);

            out_A           : out   std_logic_vector(N_BITS-1 downto 0);
            out_B           : out   std_logic_vector(N_BITS-1 downto 0);
            out_zero_ctr    : out   std_logic_vector(CTRSIZE-1 downto 0);
            out_t           : out   std_logic_vector(N_BITS downto 0)
    );
end  component;

component  out_shift  is
    port (
            CLK             : in    std_logic;
            in_gcd          : in    std_logic_vector(N_BITS-1 downto 0);
            in_zero_ctr     : in    std_logic_vector(CTRSIZE-1 downto 0);

            out_gcd         : out   std_logic_vector(N_BITS-1 downto 0);
            out_zero_ctr    : out   std_logic_vector(CTRSIZE-1 downto 0)
    );
end component;

-- array signal names for synthesizing chained PEs
type OPARR is array (natural range <>) of std_logic_vector(N_BITS-1 downto 0);
signal AInShift     : OPARR(SHIFTSIZE-1 downto 0);
signal BInShift     : OPARR(SHIFTSIZE-1 downto 0);
signal ASub         : OPARR(SUBSIZE-1 downto 0);
signal BSub         : OPARR(SUBSIZE-1 downto 0);
signal GCDShift     : OPARR(SHIFTSIZE-1 downto 0);

type CTRARR is array (natural range <>) of std_logic_vector(CTRSIZE-1 downto 0);
signal CtrInShift   : CTRARR(SHIFTSIZE-1 downto 0);
signal CtrSub       : CTRARR(SUBSIZE-1 downto 0);
signal CtrOutShift  : CTRARR(SHIFTSIZE-1 downto 0);

type TARR is array (natural range <>) of std_logic_vector(N_BITS downto 0);
signal TInShift     : TARR(SHIFTSIZE-1 downto 0);
signal TSub         : TARR(SUBSIZE-1 downto 0);


begin

    -- port mappings
    -- each block of PEs connects to previous PE
    -- except for first PE in each block with either (in in_shift case) connects to 
    --  starting operands, or the last PE of the previous block
    -- final out_shift block outputs result

    in_shift0: in_shift
        port map (
            CLK             => CLK,
            in_A            => op_A,
            in_B            => op_B,
            in_zero_ctr     => CTR_ZERO,
            out_A           => AInShift(0),
            out_B           => BInShift(0),
            out_zero_ctr    => CtrInShift(0),
            t               => TInShift(0)
        );

    in_shift_while: for i in 1 to SHIFTSIZE-1 generate
        in_shifti: in_shift
            port map (
                CLK             => CLK,
                in_A            => AInShift(i-1),
                in_B            => BInShift(i-1),
                in_zero_ctr     => CtrInShift(i-1),
                out_A           => AInShift(i),
                out_B           => BInShift(i),
                out_zero_ctr    => CtrInShift(i),
                t               => TInShift(i)
            );
    end generate in_shift_while;

    sub_loop0: sub_loop
        port map (
            CLK             => CLK,
            in_A            => AInShift(SHIFTSIZE-1),
            in_B            => BInShift(SHIFTSIZE-1),
            in_zero_ctr     => CtrInShift(SHIFTSIZE-1),
            in_t            => TInShift(SHIFTSIZE-1),
            out_A           => ASub(0),
            out_B           => BSub(0),
            out_zero_ctr    => CtrSub(0),
            out_t           => TSub(0)
        );

    sub_loop_while: for i in 1 to SUBSIZE-1 generate
        sub_loopi: sub_loop
            port map (
                CLK             => CLK,
                in_A            => ASub(i-1),
                in_B            => BSub(i-1),
                in_zero_ctr     => CtrSub(i-1),
                in_t            => TSub(i-1),
                out_A           => ASub(i),
                out_B           => BSub(i),
                out_zero_ctr    => CtrSub(i),
                out_t           => TSub(i)
            );
    end generate sub_loop_while;

    out_shift0: out_shift
        port map (
            CLK             => CLK,
            in_gcd          => ASub(SUBSIZE-1),
            in_zero_ctr     => CtrSub(SUBSIZE-1),
            out_gcd         => GCDShift(0),
            out_zero_ctr    => CtrOutShift(0)
        );

    out_shift_while: for i in 1 to SHIFTSIZE-1 generate
        out_shifti: out_shift
            port map (
                CLK             => CLK,
                in_gcd          => GCDShift(i-1),
                in_zero_ctr     => CtrOutShift(i-1),
                out_gcd         => GCDShift(i),
                out_zero_ctr    => CtrOutShift(i)
            );
    end generate out_shift_while;

    -- final out_shift block outputs result
    gcd <= GCDShift(SHIFTSIZE-1);

end  linear_interconnect;



----------------------------------------------------------------------------
--
--  in_shift PE
--
--  Implements first step of Stein's algorithm. Until both of the first
--  operands are no longer even, shifts out the 0s from the LSB of each. 
--  Increments counter each time operands are shifted. This counter is
--  passed through each PE so the output from the Stein's loop can get
--  the same amount of 0s shifted left in to the LSB.
--  Acts to essentially divide shared factors of 2 from each operand and
--  multiply them back into the output.
--  PE does not need to take in t because t is only output (always depends
--  on current values of a or b).
--
--  Inputs:
--      CLK                     - clock signal
--      in_A                    - IN first operand (b bit)
--      in_B                    - IN second operand (b bit)
--      in_zero_ctr             - IN ctr for zeros shifted from operands
--                              -   (log_2 b bits)
--
--  Outputs:
--      out_A                   - OUT first operand (b bit)
--      out_B                   - OUT second operand (b bit)
--      out_zero_ctr            - OUT ctr for zeros shifted from operands
--                              -   (log_2 b bits)
--      t                       - OUT temporary algorithm register
--                              -   signed value (b+1 bits)
--      
--
--  Algorithm: Stein's Algorithm implemented in these PEs
--            if (a is even AND b is even)
--                k ++ 
--                a /= 2
--                b /= 2
--            if (a is odd)
--                t = -b
--            else
--                t = a
--
--  Revision History:
--     03/7/2019  Sundar Pandian       Initial version
--
----------------------------------------------------------------------------

-- libraries
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.numeric_std.all;
use work.constants.all;

entity  in_shift  is

    port (
            CLK             : in    std_logic;
            in_A            : in    std_logic_vector(N_BITS-1 downto 0);
            in_B            : in    std_logic_vector(N_BITS-1 downto 0);
            in_zero_ctr     : in    std_logic_vector(CTRSIZE-1 downto 0);

            out_A           : out   std_logic_vector(N_BITS-1 downto 0);
            out_B           : out   std_logic_vector(N_BITS-1 downto 0);
            out_zero_ctr    : out   std_logic_vector(CTRSIZE-1 downto 0);
            t               : out   std_logic_vector(N_BITS downto 0)
    );

end  in_shift;

--
--  in_shift PE architecture
--

architecture  PE  of  in_shift  is

component Adder is generic ( bitsize : integer); -- default width is 8-bits
    port (
        X           : in       std_logic_vector((bitsize-1) downto 0); -- addend X
        Y           : in       std_logic_vector((bitsize-1) downto 0); -- addend Y
        Ci          : in       std_logic; -- carry in
        S           : out      std_logic_vector((bitsize-1) downto 0); -- sum out
        Co          : out      std_logic -- carry out
      );
end component;

-- carry out from adders
signal carryOutT        : std_logic;

-- output buffer from div_zero_ctr
signal out_zero_buffer  : std_logic_vector(CTRSIZE-1 downto 0);
-- output buffer from t_neg_b
signal negB_buffer      : std_logic_vector(N_BITS-1 downto 0);

-- registers for outputs
signal out_A_reg           : std_logic_vector(N_BITS-1 downto 0);
signal out_B_reg           : std_logic_vector(N_BITS-1 downto 0);
signal neg_out_B_reg       : std_logic_vector(N_BITS-1 downto 0);
signal out_zero_ctr_reg    : std_logic_vector(CTRSIZE-1 downto 0);
signal t_reg               : std_logic_vector(N_BITS downto 0);

begin

    out_zero_buffer <= in_zero_ctr(CTRSIZE-2 downto 0) & '1'; -- shift in 1 to LSB
                                                              -- acts as simple incrementer
                                                              -- push 1 when shifting A and B
                                                              -- else do nothing
    
    out_A_reg <= '0' & in_A(N_BITS-1 downto 1) when (in_A(0) = '0') and (in_B(0) = '0') else
                 in_A;
    out_B_reg <= '0' & in_B(N_BITS-1 downto 1) when (in_A(0) = '0') and (in_B(0) = '0') else
                 in_B;
    out_zero_ctr_reg <= out_zero_buffer when (in_A(0) = '0') and (in_B(0) = '0') else
                        in_zero_ctr;

    -- flip bits in in_B to form 2s complement
    NotOutB: for i in N_BITS-1 downto 0 generate
        neg_out_B_reg(i) <= not out_B_reg(i);
    end generate NotOutB;

    -- adds 1 to not in_B for 2s complement
    t_neg_b: Adder
        generic map (bitsize => N_BITS)
        port map (
            X  => neg_out_B_reg,
            Y  => B_NEG,
            Ci => CARRY_RST,
            S  => negB_buffer,
            Co => carryOutT
        );

    t_reg <= SF_POS & negB_buffer when out_A_reg(0) = '1' and in_B = B_ZERO else -- if in_B = 0
             SF_NEG & negB_buffer when out_A_reg(0) = '1' else                   -- if a is odd
             SF_POS & out_A_reg;

    -- synchronize outputs
    synch: process(CLK)
    begin
        if rising_edge(CLK) then
            out_A           <= out_A_reg;
            out_B           <= out_B_reg;
            out_zero_ctr    <= out_zero_ctr_reg;
            t               <= t_reg;
        end if;
    end process;

end  PE;

----------------------------------------------------------------------------
--
--  sub_loop PE
--
--  Implements second step of Stein's algorithm. Converts pseudocode of 
--  Stein's algorithm from containing two nested while loops to containing
--  the action of both while loops, only performing the inner loop if valid.
--  Action 1: 
--      PE shifts t right if t is even 
--      if t is negative, saves -t to B
--      else,             saves  t to A 
--  Action 2:
--      PE saves a-b to t
--
--  Inputs:
--      CLK                     - clock signal
--      in_A                    - IN first operand (b bit)
--      in_B                    - IN second operand (b bit)
--      in_zero_ctr             - IN ctr for zeros shifted from operands
--                              -   (log_2 b bits)
--      in_t                    - IN temporary algorithm register
--                              -   signed value (b+1 bits)
--
--  Outputs:
--      out_A                   - OUT first operand (b bit)
--      out_B                   - OUT second operand (b bit)
--      out_zero_ctr            - OUT ctr for zeros shifted from operands
--                              -   (log_2 b bits)
--      out_t                   - OUT temporary algorithm register
--                              -   signed value (b+1 bits)
--
--  Algorithm: Stein's Algorithm implemented in these PEs
--            if (t != 0)
--                if (t is even)
--                    t /= 2
--                    if (t > 0)
--                        a = t
--                    else
--                        b = -t
--                else
--                    t = a-b
--
--  Revision History:
--     03/7/2019  Sundar Pandian       Initial version
--
----------------------------------------------------------------------------

-- libraries
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.numeric_std.all;
use work.constants.all;

entity  sub_loop  is

    port (
            CLK             : in    std_logic;
            in_A            : in    std_logic_vector(N_BITS-1 downto 0);
            in_B            : in    std_logic_vector(N_BITS-1 downto 0);
            in_zero_ctr     : in    std_logic_vector(CTRSIZE-1 downto 0);
            in_t            : in    std_logic_vector(N_BITS downto 0);

            out_A           : out   std_logic_vector(N_BITS-1 downto 0);
            out_B           : out   std_logic_vector(N_BITS-1 downto 0);
            out_zero_ctr    : out   std_logic_vector(CTRSIZE-1 downto 0);
            out_t           : out   std_logic_vector(N_BITS downto 0)
    );

end  sub_loop;

--
--  sub_loop PE architecture
--

architecture  PE  of  sub_loop  is

component Adder is generic ( bitsize : integer); -- default width is 8-bits
    port (
        X           : in       std_logic_vector((bitsize-1) downto 0); -- addend X
        Y           : in       std_logic_vector((bitsize-1) downto 0); -- addend Y
        Ci          : in       std_logic; -- carry in
        S           : out      std_logic_vector((bitsize-1) downto 0); -- sum out
        Co          : out      std_logic -- carry out
      );
end component;

-- carry out from adders
signal carryOutT         : std_logic;
signal carryOutNT        : std_logic;

-- output buffer from SubXOR, flipping bits in in_B
signal not_in_B         : std_logic_vector(N_BITS-1 downto 0);
-- output buffer from t_adder
signal t_adder_buffer   : std_logic_vector(N_BITS-1 downto 0);
-- output buffer from neg_t
signal neg_t_buffer     : std_logic_vector(N_BITS downto 0);
-- output buffer from t shifter
signal t_shift_buffer   : std_logic_vector(N_BITS downto 0);
-- neg output buffer from t shifter
signal neg_t_shift_buffer : std_logic_vector(N_BITS downto 0);

-- registers for outputs
signal out_A_reg           : std_logic_vector(N_BITS-1 downto 0);
signal out_B_reg           : std_logic_vector(N_BITS-1 downto 0);
signal out_zero_ctr_reg    : std_logic_vector(CTRSIZE-1 downto 0);
signal t_reg               : std_logic_vector(N_BITS downto 0);


begin

    -- divide by 2 t, maintain sign bit in MSB
    t_shift_buffer <= in_t(N_BITS) & in_t(N_BITS downto 1);

    -- flip bits in in_B since subtracting
    SubNegB: for i in N_BITS-1 downto 0 generate
        not_in_B(i) <= not in_B(i);
    end generate SubNegB;

    -- perform subtraction
    t_adder: Adder
        generic map (bitsize => N_BITS)
        port map (
            X  => in_A,
            Y  => not_in_B,
            Ci => CARRY_SET,
            S  => t_adder_buffer,
            Co => carryOutT
        );

    -- flip bits in in_B since subtracting
    NotTShift: for i in N_BITS downto 0 generate
        neg_t_shift_buffer(i) <= not t_shift_buffer(i);
    end generate NotTShift;

    -- adding 1 to not t_shift_buffer to perform 2s complement
    neg_t: Adder
        generic map (bitsize => N_BITS+1)
        port map (
            X  => neg_t_shift_buffer,
            Y  => T_NEG,
            Ci => CARRY_RST,
            S  => neg_t_buffer,
            Co => carryOutNT
        );

    -- cases found in header 
    out_A_reg <= t_shift_buffer(N_BITS-1 downto 0) when (in_t /= T_ZERO) and
                                                        (in_t(0) = '0')  and
                                                        (in_t(N_BITS) = SF_POS) else
                 in_B                              when (in_t = T_ZERO) and     -- edge case when op_B = 0
                                                        (in_B = B_ZERO) else
                 in_A;
    out_B_reg <= neg_t_buffer(N_BITS-1 downto 0)   when (in_t /= T_ZERO) and
                                                        (in_t(0) = '0')  and
                                                        (in_t(N_BITS) = SF_NEG) else
                 in_B;

    t_reg <= t_shift_buffer                 when (in_t /= T_ZERO) and 
                                                 (in_t(0) = '0') else
             not carryOutT & t_adder_buffer when (in_t /= T_ZERO) and   -- flip carryOut into SF
                                                 (in_t(0) = '1') else
             in_t;
 
    out_zero_ctr_reg <= in_zero_ctr;    -- pass through


    -- synchronize outputs
    synch: process(CLK)
    begin
        if rising_edge(CLK) then
            out_A           <= out_A_reg;
            out_B           <= out_B_reg;
            out_zero_ctr    <= out_zero_ctr_reg;
            out_t           <= t_reg;
        end if;
    end process;

end  PE;

----------------------------------------------------------------------------
--
--  out_shift PE
--
--  Implements last step of Stein's algorithm. Using the data from the inc
--  counter, shifts in_gcd left. This acts to multiply back all the common
--  factors of 2 we divided out in the first step. Once we are done with
--  this, the GCD is finished
--
--  Inputs:
--      CLK                     - clock signal
--      in_gcd                  - IN operand (b bit)
--      in_zero_ctr             - IN ctr for zeros shifted from operands
--                              -   (log_2 b bits)
--
--  Outputs:
--      out_gcd                 - OUT operand (b bit)
--      out_zero_ctr            - OUT ctr for zeros shifted from operands
--                              -   (log_2 b bits)
--
--  Algorithm: Stein's Algorithm implemented in these PEs
--            a * 2^k
--
--  Revision History:
--     03/7/2019  Sundar Pandian       Initial version
--
----------------------------------------------------------------------------

-- libraries
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.numeric_std.all;
use work.constants.all;

entity  out_shift  is

    port (
            CLK             : in    std_logic;
            in_gcd          : in    std_logic_vector(N_BITS-1 downto 0);
            in_zero_ctr     : in    std_logic_vector(CTRSIZE-1 downto 0);

            out_gcd         : out   std_logic_vector(N_BITS-1 downto 0);
            out_zero_ctr    : out   std_logic_vector(CTRSIZE-1 downto 0)
    );

end  out_shift;

--
--  out_shift PE architecture
--

architecture  PE  of  out_shift  is

component Adder is generic ( bitsize : integer); -- default width is 8-bits
    port (
        X           : in       std_logic_vector((bitsize-1) downto 0); -- addend X
        Y           : in       std_logic_vector((bitsize-1) downto 0); -- addend Y
        Ci          : in       std_logic; -- carry in
        S           : out      std_logic_vector((bitsize-1) downto 0); -- sum out
        Co          : out      std_logic -- carry out
      );
end component;

-- output buffer from mul_zero_ctr
signal out_zero_buffer  : std_logic_vector(CTRSIZE-1 downto 0);

-- registers for outputs
signal gcd_reg             : std_logic_vector(N_BITS-1 downto 0);
signal out_zero_ctr_reg    : std_logic_vector(CTRSIZE-1 downto 0);


begin

    out_zero_buffer <= '0' & in_zero_ctr(CTRSIZE-1 downto 1); -- shift out LSB each operation
    
    -- can always shift out without checking
    -- since finished when zero_ctr gets to 0 and
    -- can keep shifting 0 into 0
    
    gcd_reg <= in_gcd(N_BITS-2 downto 0) & '0' when (in_zero_ctr /= CTR_ZERO) else
               in_gcd;
    out_zero_ctr_reg <= out_zero_buffer;

    -- synchronize outputs
    synch: process(CLK)
    begin
        if rising_edge(CLK) then
            out_gcd         <= gcd_reg;
            out_zero_ctr    <= out_zero_ctr_reg;
         end if;
    end process;

end  PE;