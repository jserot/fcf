library ieee ;
use ieee.std_logic_1164.all;

entity top is
  port (MAX10_CLK1_50: in std_logic;
	    HEX0: out std_logic_vector(7 downto 0);
	    HEX1: out std_logic_vector(7 downto 0);
	    HEX2: out std_logic_vector(7 downto 0);
	    HEX3: out std_logic_vector(7 downto 0);
	    HEX4: out std_logic_vector(7 downto 0);
	    HEX5: out std_logic_vector(7 downto 0);
	    KEY: in std_logic_vector(1 downto 0);
	    LEDR: out std_logic_vector(9 downto 0);
	    SW: in std_logic_vector(9 downto 0)
       ); 
end entity;

architecture rtl of top is

	component main is   
		port (
			clk_clk: in  std_logic := 'X';
			reset_reset_n: in  std_logic := 'X'
            );
    end component;

begin

	u0 : component main
		port map (
			clk_clk                           => MAX10_CLK1_50,
			reset_reset_n                     => '1'
		);

	LEDR(9 downto 0) <= "000000000" & not KEY(1);
    HEX0 <= "11000000"; -- "0"
    
end architecture;
