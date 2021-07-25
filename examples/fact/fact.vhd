library ieee;
use ieee.std_logic_1164.all;

entity fact is
  port(
        start: in integer;
        n: in integer;
        rdy: out integer;
        res: out integer;
        clk: in std_logic;
        rst: in std_logic
);
end entity;

architecture RTL of fact is
  type t_state is ( Idle, Compute );
  signal state: t_state;
  signal k: integer;
  signal acc: integer;
begin
  process(rst, clk)
  begin
    if rst='1' then
      state <= Idle;
    elsif rising_edge(clk) then 
      case state is
      when Compute =>
        if ( k<=n ) then
          acc <= acc*k;
          k <= k+1;
        elsif  ( k>n ) then
          res <= acc;
          rdy <= 1;
          state <= Idle;
        end if;
      when Idle =>
        if ( start=1 ) then
          acc <= 1;
          k <= 1;
          rdy <= 0;
          state <= Compute;
        end if;
    end case;
    end if;
  end process;
end architecture;
