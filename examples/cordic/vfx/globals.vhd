library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;

package globals is
  type s32_arr32 is array(0 to 31) of signed(31 downto 0);
  constant x0: s32_arr32;
  constant arctan: s32_arr32;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;

package body globals is
  constant x0: s32_arr32 := (to_signed(1414213562,32),to_signed(1264911064,32),to_signed(1227143982,32),to_signed(1217667825,32),to_signed(1215296512,32),to_signed(1214703540,32),to_signed(1214555288,32),to_signed(1214518224,32),to_signed(1214508958,32),to_signed(1214506642,32),to_signed(1214506063,32),to_signed(1214505918,32),to_signed(1214505882,32),to_signed(1214505873,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32),to_signed(1214505870,32));
  constant arctan: s32_arr32 := (to_signed(900000000,32),to_signed(531301023,32),to_signed(280724869,32),to_signed(142500326,32),to_signed(71526687,32),to_signed(35798212,32),to_signed(17903474,32),to_signed(8952283,32),to_signed(4476210,32),to_signed(2238113,32),to_signed(1119057,32),to_signed(559529,32),to_signed(279764,32),to_signed(139882,32),to_signed(69941,32),to_signed(34970,32),to_signed(17485,32),to_signed(8742,32),to_signed(4371,32),to_signed(2185,32),to_signed(1092,32),to_signed(546,32),to_signed(273,32),to_signed(136,32),to_signed(68,32),to_signed(34,32),to_signed(17,32),to_signed(8,32),to_signed(4,32),to_signed(2,32),to_signed(1,32),to_signed(0,32));
end package body;
