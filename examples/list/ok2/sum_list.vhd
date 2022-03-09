library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.utils.all;
use fcf.values.all;

use work.int_list.all;

entity sum_list is
  generic (heap_size: natural := 16);
  port(
        a: in int_list;
        start: in std_logic;
        h_init: in std_logic;
        hi_cnt: in integer range 0 to heap_size;
        hi_val: in block_t;
        res: out integer;
        rdy: out std_logic;
        clk: in std_logic;
        rst: in std_logic
);
end entity;

architecture RTL of sum_list is
  type t_state is ( Idle, F, InitH );
  signal state: t_state;
  signal acc: integer;
  signal l: int_list;
  signal heap: heap_t;
  signal h_ptr: heap_ptr;
begin
  process(rst, clk)
    variable ll: int_list;
    variable v: integer;
  begin
    if rst='1' then
      state <= Idle;
      rdy <= '1';
      h_ptr <= 0;
    elsif rising_edge(clk) then 
      case state is
      when InitH =>
        if ( h_ptr<hi_cnt ) then
          heap(h_ptr) <= hi_val;
          h_ptr <= h_ptr+1;
          dump_heap(heap,h_ptr);
        elsif  ( h_ptr=hi_cnt ) then
          rdy <= '1';
          state <= Idle;
          dump_heap(heap,h_ptr);
        end if;
      when F =>
        if ( int_list_match_Nil(heap, l) ) then
          res <= acc;
          rdy <= '1';
          state <= Idle;
          dump_heap(heap,h_ptr);
        elsif  ( int_list_match_Cons(heap,l,"00",0,val_int(0)) ) then
          v := int_list_get_Cons_1(heap, l);
          ll := int_list_get_Cons_2(heap, l);
          l <= ll;
          acc <= acc+v;
          dump_heap(heap,h_ptr);
        end if;
      when Idle =>
        if ( start='1' ) then
          l <= a;
          acc <= 0;
          rdy <= '0';
          state <= F;
          dump_heap(heap,h_ptr);
        elsif  ( h_init='1' ) then
          rdy <= '0';
          state <= InitH;
          dump_heap(heap,h_ptr);
        end if;
    end case;
    end if;
  end process;
end architecture;
