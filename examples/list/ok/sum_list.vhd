library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.utils.all;
use fcf.values.all;

use work.int_list.all;

entity sum_list is
  generic (trace_heap: boolean := false);
  port( a: in value;
        start: in std_logic;
        lc: in std_logic; -- 0: init heap, 1: compute
        res: out integer;
        rdy: out std_logic;
        -- Inputs dedicated to heap initilisation
        hi_cnt: in integer range 0 to heap_size;     -- number of words 
        hi_val: in block_t;                          -- word value
        --
        clk: in std_logic;
        rst: in std_logic
        );
end entity;

architecture RTL of sum_list is
  type t_state is ( Idle, Init, F );
  signal state: t_state;
  signal l: value;
  signal acc: integer;
  signal heap: heap_t;
  signal h_ptr: heap_ptr;
begin
  process(rst, clk)
    -- Patterns
    variable v: integer;
    variable ll: value;
  begin
    if rst='1' then
      state <= Idle;
      rdy <= '1';
      h_ptr <= 0;
    elsif rising_edge(clk) then 
      case state is
      when Idle =>
        if ( start='1' ) then
          if lc = '0' then -- Init heap
            if hi_cnt > 0 then -- Start heap init sequence
              rdy <= '0';
              state <= Init;
            else               -- Empty init sequence
              rdy <= '1';
              state <= Idle;
            end if;
          else             -- Start computation
            l <= a;
            acc <= 0;
            rdy <= '0';
            state <= F;
          end if;
        end if;
      when Init =>
        if h_ptr = hi_cnt then -- End of heap init sequence
          rdy <= '1';
          state <= Idle;
          assert false report "sum_list: end of heap init" severity note;
          if trace_heap then dump_heap(heap, h_ptr); end if;
        else         
          heap(h_ptr) <= hi_val;
          h_ptr <= h_ptr+1;
          -- assert false report "sum_list: heap init: " & integer'image(h_ptr) & " " & string_of_value(hi_val) severity note;
          state <= Init;
        end if;
      when F =>
        -- if trace_heap then dump_heap(heap, h_ptr); end if;
        assert false report "l=" & string_of_value(l) & " acc=" & integer'image(acc) severity note;
        if ( int_list_match_Nil(heap, l) ) then
          assert false report "sum_list: matched Nil" severity note;
          res <= acc;
          rdy <= '1';
          state <= Idle;
        elsif  ( int_list_match_Cons(heap,l,"00",0,val_int(0)) ) then
          v := int_list_get_Cons_1(heap,l);
          ll := int_list_get_Cons_2(heap,l);
          assert false report "sum_list: matched Cons(v=" & integer'image(v) & ",ll=" &  string_of_value(ll) & ")" severity note;
          l <= ll;
          acc <= acc+v;
        end if;
    end case;
    end if;
  end process;
end architecture;
