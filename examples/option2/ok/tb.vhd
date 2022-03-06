library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;

use work.int_option.all;

entity tb is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;

use work.values.all;
use work.int_option.all;

architecture struct of tb is

signal silly_a: value;
signal silly_start: std_logic;
signal silly_res: integer;
signal silly_rdy: std_logic;
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

U1: entity work.silly port map(silly_a,silly_start,silly_res,silly_rdy,clk,rst);

process
begin
  silly_start <= '0';
  wait for 15 ns;
  -- silly_a <= val_int(0);  -- None OK
  -- silly_a <= val_ptr(0);  -- heap(0)=Some 0
  silly_a <= val_ptr(0);  -- heap(0)=Some 1
  silly_start <= '1';
  wait for 10 ns;
  silly_start <= '0';
  wait until silly_rdy = '1';
  wait for 50 ns;
  assert false report "end of simulation: res=" & integer'image(silly_res) severity note;
  wait;
end process;

end architecture;
