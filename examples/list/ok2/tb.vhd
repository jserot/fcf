library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.utils.all;
use fcf.values.all;

use work.int_list.all;

entity tb is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.utils.all;
use fcf.values.all;

use work.int_list.all;

architecture struct of tb is

signal sum_list_a: value;
signal sum_list_start: std_logic;
signal sum_list_res: integer;
signal sum_list_rdy: std_logic;
signal sum_list_h_init: std_logic;
signal sum_list_hi_cnt: natural range 0 to heap_size;
signal sum_list_hi_val: block_t;
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
  clk <= '1'; wait for 5 ns;
  clk <= '0'; wait for 5 ns;
end process;

U1: entity work.sum_list
     port map(sum_list_a, sum_list_start, sum_list_h_init, sum_list_hi_cnt, sum_list_hi_val, sum_list_res, sum_list_rdy, clk, rst);

process

type heap_init_t is array (natural range <>) of std_logic_vector(31 downto 0);

constant heap_init: heap_init_t := (
  mk_header(1, 2), -- 0: Cons
  val_int(1),      -- 1:   1
  val_ptr(3),      -- 2:   *-----+
  mk_header(1, 2), -- 3: Cons <--+
  val_int(2),      -- 4:   2
  val_ptr(6),      -- 5:   *-----+
  mk_header(1, 2), -- 6: Cons <--+
  val_int(3),      -- 7:   3
  val_int(0)       -- 8:   Nil
  );

begin
  sum_list_start <= '0';
  sum_list_h_init <= '0';
  wait for 5 ns;
  -- Heap init sequence
  sum_list_h_init <= '1';
  sum_list_hi_cnt <= heap_init'length;
  wait for 10 ns;
  sum_list_h_init <= '0';
  for i in heap_init'range loop
    sum_list_hi_val <= heap_init(i);
    wait for 10 ns;
  end loop;
  wait until sum_list_rdy = '1';
  wait for 15 ns;
  -- Start computation
  sum_list_h_init <= '0';
  sum_list_a <= val_ptr(0);
  sum_list_start <= '1';
  wait for 10 ns;
  sum_list_start <= '0';
  wait until sum_list_rdy = '1';
  wait for 10 ns;
  assert false report "end of simulation: res=" & integer'image(sum_list_res) severity note;
  wait;
end process;

end architecture;
