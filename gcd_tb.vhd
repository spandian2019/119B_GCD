----------------------------------------------------------------------------
--
--  Test Bench for GCD
--
--  This is a test bench for the GCD entity.  The test bench
--  thoroughly tests the entity by exercising it and checking the outputs.
--  The testbench first tests some preset edge cases, defined in test vectors
--  Then it goes into 32 randomized test cases, using a for loop to compute
--  the GCD for these random test cases.
--  Upon each error, the two operands are outputted as well as their
--  expected GCD and the received GCD.
--
--  Revision History:
--      03/8/19   Sundar Pandian    Initial revision.
--
----------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.constants.all;

entity GCD_tb is
end GCD_tb;

architecture TB_ARCHITECTURE of GCD_tb is

    -- Component declaration of the tested unit
    component sys_array is
        port (
                CLK             : in    std_logic;
                op_A            : in    std_logic_vector(N_BITS-1 downto 0);
                op_B            : in    std_logic_vector(N_BITS-1 downto 0);

                gcd             : out   std_logic_vector(N_BITS-1 downto 0)
        );
    end  component;

    -- signals for porting inputs
    signal reg_A       :  std_logic_vector(N_BITS-1 downto 0);
    signal reg_B       :  std_logic_vector(N_BITS-1 downto 0);
    signal CLK         :  std_logic;

    -- signals for porting outputs
    signal result      :  std_logic_vector(N_BITS-1 downto 0);

    -- clock period
    constant clk_time : time := 20 ns;
    -- SIM process flag
    signal END_SIM : BOOLEAN := FALSE;

    -- array of integers for producing test vectors
    constant TESTSIZE : integer := 14;
    type test_vectors is array (0 to TESTSIZE-1) of integer;

    -- random testing constants
    constant RANDSIZE : integer := 32;

begin

    -- Unit Under Test port map
    UUT : sys_array
        port map  (
            CLK => CLK,
            op_A => reg_A,
            op_B => reg_B,
            gcd => result
        );

    -- now generate the stimulus and test it
    process

        -- loop indices
        variable i : integer;
        variable j : integer;

        -- two input operand vectors
        variable A_vectors : test_vectors   := (3, 15, 7, 81, 10000, 65535, 0, 65535, 65535,     0,
                                              255, 60, 25, 64);
        variable B_vectors : test_vectors   := (2,  5, 6,  9,  1000,     1, 0,     0, 65535, 65535,
                                              110, 84, 11, 88);

        -- expected output vector
        variable GCD_vectors : test_vectors := (1,  5, 1,  9,  1000,     1, 0,     0, 65535,     0,
                                                5, 12,  1,  8);

        ---- uniform randum number generator function variables
        variable seed1, seed2 : positive;
        variable randA, randB : real;
        variable range_of_rand : real := 65535.0;

        -- variables for comparing GCD outputs of randomly generated inputs
        variable rand_A : integer;
        variable rand_B : integer;
        variable rd_GCD : integer;
        variable sum    : integer;

    begin  -- of stimulus process
        -- sets initial state

        -- tests edge cases
        for i in 0 to TESTSIZE+RUNTIME-2 loop
            -- input to the system
            if i < TESTSIZE then
                reg_A <= std_logic_vector(to_unsigned(A_vectors(i), 16));
                reg_B <= std_logic_vector(to_unsigned(B_vectors(i), 16));
            end if;
            wait for clk_time*0.8;
            -- checks output
            if i >= RUNTIME-1 then
                assert (std_match(result, std_logic_vector(to_unsigned(GCD_vectors(i-RUNTIME+1), 16))))
                    report  " Computed GCD of: " & integer'image(A_vectors(i-RUNTIME+1)) &
                            " and "              & integer'image(B_vectors(i-RUNTIME+1)) &
                            ". Result was "      & integer'image(to_integer(unsigned(result))) &
                            ". Expected "        & integer'image(GCD_vectors(i-RUNTIME+1))
                    severity  ERROR;
            end if;
            wait for clk_time*0.2;
        end loop;

        --wait for 32*clk_time;

        -- test 32 random input combinations
        -- does not check inputting every clock since TB has already checked this
        -- for ease of randomized testing, inputs 2 random operands and waits RUNTIME*CLK to check
        -- checks random output before inputting next randomized inputs
        for i in RANDSIZE-1 downto 0 loop
            if i < RANDSIZE then
                -- generates two random inputs
                uniform(seed1, seed2, randA);
                uniform(seed1, seed2, randB);
                rand_A := integer(randA*range_of_rand);
                rand_B := integer(randB*range_of_rand);
                -- simple for loop computes expected GCD of random inputs
                --      highest number with no remainder is the GCD, by defn
                sum := rand_A+rand_B;
                    for j in 1 to sum loop
                        if (rand_A mod j = 0) and (rand_B mod j = 0) then
                            rd_GCD := j;
                        end if;
                    end loop;
                -- input to the system
                reg_A <= std_logic_vector(to_unsigned(rand_A, 16));
                reg_B <= std_logic_vector(to_unsigned(rand_B, 16));
            end if;
            wait for clk_time*RUNTIME;
            wait for clk_time*0.8;
            -- checks output
            assert (std_match(result, std_logic_vector(to_unsigned(rd_GCD, 16))))
                report  " Computed GCD of: " & integer'image(rand_A) &
                        " and "              & integer'image(rand_B) &
                        ". Result was "      & integer'image(to_integer(unsigned(result))) &
                        ". Expected "        & integer'image(rd_GCD)
                severity  ERROR;
            wait for clk_time*0.2;
        end loop;

        wait for 100*clk_time;
        END_SIM <= TRUE;
        wait;
        -- and just keep looping in the process
    end process; -- end of stimulus process

    CLOCK_CLK : process
    begin

        -- this process generates a clock with a defined period and 50% duty cycle
        -- stop the clock when end of simulation is reached
        if END_SIM = FALSE then
            CLK <= '0';
            wait for clk_time/2;
        else
            wait;
        end if;

        if END_SIM = FALSE then
            CLK <= '1';
            wait for clk_time/2;
        else
            wait;
        end if;

    end process;

end TB_ARCHITECTURE;


--configuration TB_MULT of GCD_tb is
--    for TB_ARCHITECTURE
--        for UUT : sys_array
--            use entity work.sys_array(linear_interconnect);
--        end for;
--    end for;
--end TB_MULT;
