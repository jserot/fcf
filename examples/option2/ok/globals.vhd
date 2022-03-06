library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;

use work.values.all;

package int_option is

  procedure int_option_mk_None(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; signal result: out value);
  procedure int_option_mk_Some(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; arg1: in integer; signal result: out value);
  function int_option_get_Some_1(signal heap: heap_t; v: value) return integer;
  function int_option_match_None(signal heap: heap_t; v: value) return boolean;
  function int_option_match_Some(signal heap: heap_t; v: value; args: std_logic_vector(1 to 1); arg1: integer) return boolean;
  
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.fcf.all;

package body int_option is

  procedure int_option_mk_None(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; signal result: out value) is
  begin
    result <= val_int(0); -- 0=ctor_id(None)
  end procedure;

  procedure int_option_mk_Some(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; arg1: in integer; signal result: out value) is
  begin
    heap(h_ptr) <= mk_header(1, 1); -- tag=ctor_id(Some)=1, wo_size=1 
    heap(h_ptr+1) <= val_int(arg1);
    h_ptr <= h_ptr+2;
    result <= val_ptr(h_ptr);
  end procedure;

  function int_option_get_Some_1(signal heap: heap_t; v: value) return integer is
  begin
    return int_val(field(heap,v,0));
  end function;

  function int_option_match_None(signal heap: heap_t; v: value) return boolean is
  begin
    return is_imm(v) and int_val(v) = 0; -- 0=ctor_id(None)
  end function;

  function int_option_match_Some(signal heap: heap_t; v: value; args: std_logic_vector(1 to 1); arg1: integer) return boolean is
    variable b: block_t;
    variable r: boolean;
  begin
    r := tag_val(heap,v) = 1; -- 1=ctor_id(Some)
    r := r and (args(1) = '0' or int_option_get_Some_1(heap,v) = arg1);
    return r;
  end function;

end package body;

