library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;
use work.globals.all;

entity cordic is
  port(
        niter: in signed(31 downto 0);
        phi: in signed(31 downto 0);
        start: in std_logic;
        res1: out signed(31 downto 0);
        res2: out signed(31 downto 0);
        rdy: out std_logic;
        clk: in std_logic;
        rst: in std_logic
);
end entity;

architecture RTL of cordic is
  type t_state is ( Idle, Compute );
  signal state: t_state;
  signal y: signed(31 downto 0);
  signal x: signed(31 downto 0);
  signal a: signed(31 downto 0);
  signal i: signed(31 downto 0);
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
          i <= i+to_signed(1,32);
          a <= a+arctan(to_integer(i));
          x <= x-shift_left(y,to_integer(i));
          y <= y+shift_left(x,to_integer(i));
        elsif  ( a>=phi ) then
          i <= i+to_signed(1,32);
          a <= a-arctan(to_integer(i));
          x <= x+shift_left(y,to_integer(i));
          y <= y-shift_left(x,to_integer(i));
        end if;
      when Idle =>
        if ( start='1' ) then
          i <= to_signed(0,32);
          a <= to_signed(0,32);
          x <= x0(to_integer(niter)-1);
          y <= to_signed(0,32);
          rdy <= '0';
          state <= Compute;
        end if;
    end case;
    end if;
  end process;
end architecture;
