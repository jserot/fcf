library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;
use work.globals.all;

entity tb is
end entity;

architecture struct of tb is

component cordic 
  port(
        niter: in signed(31 downto 0);
        phi: in signed(31 downto 0);
        start: in std_logic;
        res1: out signed(31 downto 0);
        res2: out signed(31 downto 0);
        rdy: out std_logic;
        clk: in std_logic;
        rst: in std_logic
);
end component;

signal cordic_niter: signed(31 downto 0);
signal cordic_phi: signed(31 downto 0);
signal cordic_start: std_logic;
signal cordic_res1: signed(31 downto 0);
signal cordic_res2: signed(31 downto 0);
signal cordic_rdy: std_logic;
signal clk: std_logic;
signal rst: std_logic;

begin

RESET: process
begin
  rst <= '1';
  wait for 2 ns;
  rst <= '0';
  wait;
end process;


CLOCK: process
begin
  clk <= '1';
  wait for 10 ns;
  clk <= '0';
  wait for 10 ns;
end process;

U1: cordic port map(cordic_niter,cordic_phi,cordic_start,cordic_res1,cordic_res2,cordic_rdy,clk,rst);


process
begin
  cordic_start <= '0';
  wait for 10 ns;
  --  cordic(to_signed(31,32),to_signed(1200000000,32))
  cordic_niter <= to_signed(31,32);
  cordic_phi <= to_signed(1200000000,32);
  cordic_start <= '1';
  wait for 15 ns;
  cordic_start <= '0';
  wait until cordic_rdy = '1';
  wait for 50 ns;
  assert false report "end of simulation" severity note;
  wait;
end process;

end architecture;
