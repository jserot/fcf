library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;
use work.globals.all;

entity cordic is
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
end entity;

architecture RTL of cordic is
  type t_state is ( Idle, Compute );
  signal state: t_state;
  signal d: real;
  signal y: real;
  signal x: real;
  signal a: real;
  signal i: integer;
begin
  process(rst, clk)
  begin
    if rst='1' then
      state <= Idle;
      rdy <= '1';
    elsif rising_edge(clk) then 
      case state is
      when Compute =>
        if ( i>=niter ) then
          res1 <= x; res2 <= y;
          rdy <= '1';
          state <= Idle;
        elsif  ( a<phi ) then
          i <= i+1;
          a <= a+arctan(i);
          x <= x-(y*d);
          y <= y+(x*d);
          d <= d/2.0;
        elsif  ( a>=phi ) then
          i <= i+1;
          a <= a-arctan(i);
          x <= x+(y*d);
          y <= y-(x*d);
          d <= d/2.0;
        end if;
      when Idle =>
        if ( start='1' ) then
          i <= 0;
          a <= 0.0;
          x <= x0(niter-1);
          y <= 0.0;
          d <= 1.0;
          rdy <= '0';
          state <= Compute;
        end if;
    end case;
    end if;
  end process;
end architecture;
