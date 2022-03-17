library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.utils.all;
use fcf.values.all;

use work.consts.all;
use work.types.all;

entity tb is
end entity;

architecture struct of tb is

component cordic 
  port(
        niter: in integer;
        phi: in signed(31 downto 0);
        start: in std_logic;
        res1: out signed(31 downto 0);
        res2: out signed(31 downto 0);
        rdy: out std_logic;
        clk: in std_logic;
        rst: in std_logic
);
end component;

signal cordic_niter: integer;
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
  wait for 5 ns;
  clk <= '0';
  wait for 5 ns;
end process;

U1: cordic port map(cordic_niter,cordic_phi,cordic_start,cordic_res1,cordic_res2,cordic_rdy,clk,rst);

process -- cordic
begin
  cordic_start <= '0';
  wait for 10 ns;
  -- Start computation
  cordic_niter <= val_int(31);
  cordic_phi <= val_int(1200000000);
  cordic_start <= '1';
  wait for 10 ns;
  cordic_start <= '0';
  wait until cordic_rdy = '1';
  assert false report "cordic_res1=" & signed_to_string(cordic_res1) severity note;
  assert false report "cordic_res2=" & signed_to_string(cordic_res2) severity note;
  wait for 50 ns;
  assert false report "end of simulation" severity note;
  wait;
end process;

end architecture;
