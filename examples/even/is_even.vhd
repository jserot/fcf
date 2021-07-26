library ieee;
use ieee.std_logic_1164.all;
library fcf;
use fcf.fcf.all;

entity is_even is
  port(
        start: in std_logic;
        n: in integer;
        rdy: out std_logic;
        res: out std_logic;
        clk: in std_logic;
        rst: in std_logic
);
end entity;

architecture RTL of is_even is
  type t_state is ( Idle, Even, Odd );
  signal state: t_state;
  signal m: integer;
begin
  process(rst, clk)
  begin
    if rst='1' then
      state <= Idle;
    elsif rising_edge(clk) then 
      case state is
      when Even =>
        if ( m>0 ) then
          m <= m-1;
          state <= Odd;
        elsif  ( m<=0 ) then
          res <= '1';
          rdy <= '1';
          state <= Idle;
        end if;
      when Idle =>
        if ( start='1' ) then
          m <= n;
          rdy <= '0';
          state <= Even;
        end if;
      when Odd =>
        if ( m>0 ) then
          m <= m-1;
          state <= Even;
        elsif  ( m<=0 ) then
          res <= '0';
          rdy <= '1';
          state <= Idle;
        end if;
    end case;
    end if;
  end process;
end architecture;
