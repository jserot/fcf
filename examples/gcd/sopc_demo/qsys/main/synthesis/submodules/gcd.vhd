library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--library fcf;
use work.fcf.all;

entity gcd is
  port(
        start: in std_logic;
        a: in unsigned(15 downto 0);
        b: in unsigned(15 downto 0);
        rdy: out std_logic;
        res: out unsigned(15 downto 0);
        clk: in std_logic;
        rst: in std_logic
);
end entity;

architecture RTL of gcd is
  type t_state is ( Idle, Compute );
  signal state: t_state;
  signal n: unsigned(15 downto 0);
  signal m: unsigned(15 downto 0);
begin
  process(rst, clk)
  begin
    if rst='1' then
      state <= Idle;
    elsif rising_edge(clk) then 
      case state is
      when Compute =>
        if ( m>n ) then
          m <= m-n;
          n <= n;
        elsif  ( m<n ) then
          m <= m;
          n <= n-m;
        elsif  ( m=n ) then
          res <= m;
          rdy <= '1';
          state <= Idle;
        end if;
      when Idle =>
        if ( start='1' ) then
          m <= a;
          n <= b;
          rdy <= '0';
          state <= Compute;
        end if;
    end case;
    end if;
  end process;
end architecture;
