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
signal silly_lc: std_logic;
signal silly_hi_cnt: natural range 0 to heap_size;
signal silly_hi_val: block_t;
signal clk: std_logic;
signal rst: std_logic;

begin

RESET: process
begin
  rst <= '1'; wait for 2 ns;
  rst <= '0';
  wait;
end process;


CLOCK: process
begin
  clk <= '1'; wait for 10 ns;
  clk <= '0'; wait for 10 ns;
end process;

U1: entity work.silly
     generic map(true)
     port map(silly_a, silly_start, silly_lc, silly_res, silly_rdy, silly_hi_cnt, silly_hi_val, clk, rst);

process

type heap_init_t is array (natural range <>) of block_t;

constant heap_init: heap_init_t := (
  mk_header(1, 1), -- Some 2
  val_int(2) 
  );

begin
  silly_start <= '0';
  silly_lc <= '0';
  wait for 15 ns;
  -- Heap init sequence
  silly_start <= '1';
  silly_hi_cnt <= 2;
  wait for 10 ns;
  silly_start <= '0';
  for i in heap_init'range loop
    wait for 10 ns;
    silly_hi_val <= heap_init(i);
  end loop;
  wait until silly_rdy = '1';
  wait for 15 ns;
  -- Start computation
  silly_lc <= '1';
  silly_a <= val_ptr(0);
  silly_start <= '1';
  wait for 10 ns;
  silly_start <= '0';
  wait until silly_rdy = '1';
  wait for 10 ns;
  assert false report "end of simulation: res=" & integer'image(silly_res) severity note;
  wait;
end process;

end architecture;
