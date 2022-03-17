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
        phi: in real;
        start: in std_logic;
        res1: out real;
        res2: out real;
        rdy: out std_logic;
        clk: in std_logic;
        rst: in std_logic
);
end component;

signal cordic_niter: integer;
signal cordic_phi: real;
signal cordic_start: std_logic;
signal cordic_res1: real;
signal cordic_res2: real;
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
