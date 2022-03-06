library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;

use work.values.all;
use work.int_option.all;

entity silly is
  generic (trace_heap: boolean := false);
  port( a: in value;
        start: in std_logic;
        res: out integer;
        rdy: out std_logic;
        clk: in std_logic;
        rst: in std_logic
        );
end entity;

architecture RTL of silly is
  type t_state is ( Idle, F );
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
      heap <= ( 0 => mk_header(1, 1), -- we_size=1, tag=1=Some 
                1 => val_int(1), -- Some 1
                others => (others => '0'));
      h_ptr <= 2;
    elsif rising_edge(clk) then 
      case state is
      when Idle =>
        if ( start='1' ) then
          x <= a;
          rdy <= '0';
          state <= F;
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
