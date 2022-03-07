library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.utils.all;
use fcf.values.all;

package int_list is

  procedure int_list_mk_Nil(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; signal result: out value);
  procedure int_list_mk_Cons(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; arg1: in integer; arg2: in value; signal result: out value);
  function int_list_get_Cons_1(signal heap: heap_t; v: value) return integer;
  function int_list_get_Cons_2(signal heap: heap_t; v: value) return value;
  function int_list_match_Nil(signal heap: heap_t; v: value) return boolean;
  function int_list_match_Cons( signal heap: heap_t; v: value; args: std_logic_vector(1 to 2); arg1: integer; arg2: value) return boolean;
  
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library fcf;
use fcf.utils.all;
use fcf.values.all;

package body int_list is

  procedure int_list_mk_Nil(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; signal result: out value) is
  begin
    result <= val_int(0); -- 0=ctor_id(Nil)
  end procedure;

  procedure int_list_mk_Cons(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; arg1: in integer; arg2: in value; signal result: out value) is
  begin
    heap(h_ptr) <= mk_header(1, 2); -- tag=ctor_id(Cons)=1, wo_size=2 
    heap(h_ptr+1) <= val_int(arg1);
    heap(h_ptr+2) <= arg2;
    h_ptr <= h_ptr+3;
    result <= val_ptr(h_ptr);
  end procedure;

  function int_list_get_Cons_1(signal heap: heap_t; v: value) return integer is
  begin
    return int_val(field(heap,v,0));
  end function;

  function int_list_get_Cons_2(signal heap: heap_t; v: value) return value is
  begin
    return field(heap,v,1);
  end function;

  function int_list_match_Nil(signal heap: heap_t; v: value) return boolean is
  begin
    return is_imm(v) and int_val(v) = 0; -- 0=ctor_id(Nil)
  end function;

  function int_list_match_Cons(signal heap: heap_t; v: value; args: std_logic_vector(1 to 2); arg1: integer; arg2: value) return boolean is
    variable b: block_t;
    variable r: boolean;
  begin
    r := tag_val(heap,v) = 1; -- 1=ctor_id(Cons)
    r := r and (args(1) = '0' or int_list_get_Cons_1(heap,v) = arg1);
    r := r and (args(2) = '0' or int_list_get_Cons_2(heap,v) = arg2);
    return r;
  end function;

end package body;

