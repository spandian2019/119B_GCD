----------------------------------------------------------------------------
--
--  Parallel GCD computer
--
--  This file contains all the entities for synthesizing a systolic array
--  implementation for GCD computation. It utilizes Stein's algorithm to
--  to achieve GCD computation every clk (after some initial delay for the 
--  first 4b GCD computations, where b is the number of bits). The system
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

entity  in_shift  is

    port (
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

component fullAdder is
    port(
        A           :  in      std_logic;  -- adder input
        B           :  in      std_logic;  -- adder input
        Cin         :  in      std_logic;  -- carry in value
        Cout        :  out     std_logic;  -- carry out value
        Sum         :  out     std_logic   -- sum of A, B with carry in
      );
end component;

begin



    -- synchronize outputs
    synch: process(CLK)
    begin
        if rising_edge(CLK) then
            -- detect any user inputs on each clock
            if nCalculate = '0' then
                nCalcSynch <= '0';
            -- reset any user inputs on the FSM clock
            elsif state_clk = '1' then
                nCalcSynch <= '1';
            -- latch value until FSM clock
            else
                nCalcSynch <= nCalcSynch;
            end if;
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

entity  sub_loop  is

    port (
            in_A            : in    std_logic_vector(N_BITS-1 downto 0);
            in_B            : in    std_logic_vector(N_BITS-1 downto 0);
            in_zero_ctr     : in    std_logic_vector(CTRSIZE-1 downto 0);

            out_A           : out   std_logic_vector(N_BITS-1 downto 0);
            out_B           : out   std_logic_vector(N_BITS-1 downto 0);
            out_zero_ctr    : out   std_logic_vector(CTRSIZE-1 downto 0)
    );

end  sub_loop;

--
--  sub_loop PE architecture
--

architecture  PE  of  sub_loop  is

    signal CurrentState :  states := idle;      -- current state, initialized to idle

    signal outVal           :  std_logic;       -- combinational output from add/sub
    signal carryVal         :  std_logic;       -- combinational carry from add/sub

    signal nCalcSynch       :  std_logic;       -- DFF'd value from nCalculate
                                                -- eliminates delay issues in CL

    signal state_clk        :  std_logic;       -- clock to change FSM states
    signal counter          :  unsigned(3 downto 0) := "0000";  -- counter for operations

    signal carryFlag        :  std_logic;        -- stored carry flag
    signal subFlag          :  std_logic;        -- flag to determine adding or subbing

    -- input registers
    signal a_reg            :  std_logic_vector(15 downto 0);
    signal b_reg            :  std_logic_vector(15 downto 0);


begin
    -- synchronize outputs
    synch: process(CLK)
    begin
        if rising_edge(CLK) then
            -- detect any user inputs on each clock
            if nCalculate = '0' then
                nCalcSynch <= '0';
            -- reset any user inputs on the FSM clock
            elsif state_clk = '1' then
                nCalcSynch <= '1';
            -- latch value until FSM clock
            else
                nCalcSynch <= nCalcSynch;
            end if;
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

entity  out_shift  is

    port (
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

    signal CurrentState :  states := idle;      -- current state, initialized to idle

    signal outVal           :  std_logic;       -- combinational output from add/sub
    signal carryVal         :  std_logic;       -- combinational carry from add/sub

    signal nCalcSynch       :  std_logic;       -- DFF'd value from nCalculate
                                                -- eliminates delay issues in CL

    signal state_clk        :  std_logic;       -- clock to change FSM states
    signal counter          :  unsigned(3 downto 0) := "0000";  -- counter for operations

    signal carryFlag        :  std_logic;        -- stored carry flag
    signal subFlag          :  std_logic;        -- flag to determine adding or subbing

    -- input registers
    signal a_reg            :  std_logic_vector(15 downto 0);
    signal b_reg            :  std_logic_vector(15 downto 0);


begin

    -- synchronize outputs
    synch: process(CLK)
    begin
        if rising_edge(CLK) then
            -- detect any user inputs on each clock
            if nCalculate = '0' then
                nCalcSynch <= '0';
            -- reset any user inputs on the FSM clock
            elsif state_clk = '1' then
                nCalcSynch <= '1';
            -- latch value until FSM clock
            else
                nCalcSynch <= nCalcSynch;
            end if;
        end if;
    end process;

end  PE;