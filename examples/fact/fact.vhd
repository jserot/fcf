library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;

entity fact is
  port(
        start: in std_logic;
        n: in unsigned(7 downto 0);
        rdy: out std_logic;
        res: out unsigned(7 downto 0);
        clk: in std_logic;
        rst: in std_logic
);
end entity;

architecture RTL of fact is
  type t_state is ( Idle, Compute );
  signal state: t_state;
  signal k: unsigned(7 downto 0);
  signal acc: unsigned(7 downto 0);
begin
  process(rst, clk)
  begin
    if rst='1' then
      state <= Idle;
    elsif rising_edge(clk) then 
      case state is
      when Compute =>
        if ( k<=n ) then
          acc <= mul(acc,k);
          k <= k+to_unsigned(1,8);
        elsif  ( k>n ) then
          res <= acc;
          rdy <= '1';
          state <= Idle;
        end if;
      when Idle =>
        if ( start='1' ) then
          acc <= to_unsigned(1,8);
          k <= to_unsigned(1,8);
          rdy <= '0';
          state <= Compute;
        end if;
    end case;
    end if;
  end process;
end architecture;
