library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.utils.all;
use fcf.values.all;

use work.int_option.all;

entity silly is
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

architecture RTL of silly is
  type t_state is ( Idle, Init, F );
  signal state: t_state;
  signal x: value;
  signal heap: heap_t;
  signal h_ptr: heap_ptr;
begin
  process(rst, clk)
    variable n: integer;
  begin
    if rst='1' then
      state <= Idle;
      rdy <= '1';
      h_ptr <= 0;
      -- Static heap initialisation. Easy but not synthetizer friendly ...
      -- heap <= ( 0 => mk_header(1, 1), -- we_size=1, tag=1=Some 
      --           1 => val_int(1), -- Some 1
      --           others => (others => '0'));
      -- h_ptr <= 2;
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
            x <= a;
            rdy <= '0';
            state <= F;
          end if;
        end if;
      when Init =>
        if h_ptr = hi_cnt then -- End of heap init sequence
          rdy <= '1';
          state <= Idle;
          assert false report "silly: end of heap init" severity note;
          if trace_heap then dump_heap(heap, h_ptr); end if;
        else         
          heap(h_ptr) <= hi_val;
          h_ptr <= h_ptr+1;
          assert false report "silly: heap init: " & integer'image(h_ptr) & " " & string_of_value(hi_val) severity note;
          state <= Init;
        end if;
      when F =>
        if trace_heap then dump_heap(heap, h_ptr); end if;
        assert false report "x=" & string_of_value(x) severity note;
        if ( int_option_match_None(heap, x) ) then
          assert false report "silly: matched None" severity note;
          res <= 0;
          rdy <= '1';
          state <= Idle;
        elsif  ( int_option_match_Some(heap,x,"1",0) ) then
          assert false report "silly: matched Some(0)" severity note;
          int_option_mk_None(heap,h_ptr,x);
        elsif  ( int_option_match_Some(heap,x,"0",0) ) then
          assert false report "silly: matched Some _" severity note;
          n := int_option_get_Some_1(heap,x);
          int_option_mk_Some(heap,h_ptr,n-1,x);
        end if;
    end case;
    end if;
  end process;
end architecture;
