library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package values is

  -- Value representation :
  --
  -- + 31               0
  -- +---+----------------+
  -- |tag|   ...          |  tag=0 for immediate values, 1 for block ptrs (representing structured values)
  -- +---+----------------+
  --
  -- Blocks in heap :
  -- +----------------+---+
  -- |        sz      |tag|  tag=0 for arrays values, ctor id for variants
  -- +--------------------+
  -- |        v_1         |  
  -- +--------------------+
  -- |        ...         |  for arrays: elem i, for non-const ctors: arg_i
  -- +--------------------+
  -- |        v_sz        | 
  -- +--------------------+

  -- constant heap_size: natural := 16;  -- in words

  constant word_size: natural := 32;  -- blocks and values

  subtype block_t is std_logic_vector(word_size-1 downto 0);
  type heap_t is array (natural range <>) of block_t;
  -- type heap_t is array (0 to heap_size-1) of block_t;

  subtype heap_ptr is integer;
  -- subtype heap_ptr is integer range 0 to heap_size;

  constant bk_tag_sz: natural := 4;
  constant max_ctors: natural := 2**bk_tag_sz;  -- to be checked by all variant type definitions 

  subtype value is std_logic_vector(word_size-1 downto 0);
  
  function is_imm(v: value) return boolean;
  function is_ptr(v: value) return boolean;

  function int_val(v: value) return integer;
  function bool_val(v: value) return boolean;
  function bool_val(v: value) return std_logic;
  function ptr_val(v: value) return heap_ptr;

  function val_int(v: integer) return value;
  function val_int(v: integer) return integer;
  function val_int(v: unsigned) return value;
  function val_int(v: signed) return value;
  function val_bool(v: boolean) return value;
  function val_bool(v: std_logic) return value;
  function val_ptr(v: heap_ptr) return value;

  function wo_size(heap: heap_t; v: value) return natural;
  function tag_val(heap: heap_t; v: value) return natural;
  function field(heap: heap_t; v: value; n: natural) return value;

  function mk_header(tag: natural; wo_size: natural) return block_t;
  -- procedure alloc_header(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; b_tag: in natural; wo_size: in natural);
  -- procedure alloc_block(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; v: value);

  -- For debug only
  function string_of_value(v : value) return string;
  procedure dump_heap(signal heap: heap_t; signal h_ptr: heap_ptr);

end package;

package body values is
  
  function is_imm(v: value) return boolean is
  begin
    return v(word_size-1) = '0';  
  end;

  function is_ptr(v: value) return boolean is
  begin
    return v(word_size-1) = '1';  
  end;

  function int_val(v: value) return integer is
  begin
    return to_integer(signed(v(word_size-2 downto 0)));
  end;

  function bool_val(v: value) return boolean is
  begin
    return int_val(v) /= 0; 
  end;

  function bool_val(v: value) return std_logic is
  begin
    if int_val(v) /= 0 then return '1'; else return '0'; end if;
  end;

  function ptr_val(v: value) return heap_ptr is
  begin
    return to_integer(unsigned(v(word_size-2 downto 0)));
  end;

  function val_int(v: integer) return value is
  begin
    return '0' & std_logic_vector(to_signed(v,31));
  end;

  function val_int(v: integer) return integer is
  begin
    return v;
  end;
  
  function val_int(v: unsigned) return value is
  begin
    return '0' & std_logic_vector(resize(v,31));
  end;
  
  function val_int(v: signed) return value is
  begin
    return '0' & std_logic_vector(resize(v,31));
  end;
  
  function val_bool(v: boolean) return value is
  begin
    if v then return val_int(1); else return val_int(0); end if;
  end;

  function val_bool(v: std_logic) return value is
  begin
    if v='1' then return val_int(1); else return val_int(0); end if;
  end;

  function val_ptr(v: heap_ptr) return value is
  begin
    return '1' & std_logic_vector(to_unsigned(v,31));
  end;

  function b_size(blk: block_t) return natural is
  begin
    return to_integer(unsigned(blk(word_size-1 downto bk_tag_sz)));
  end;
    
  function wo_size(heap: heap_t; v: value) return natural is
    variable header: block_t;
  begin
    return b_size(heap(ptr_val(v)));
  end;
    
  function b_tag(blk: block_t) return natural is
  begin
    return to_integer(unsigned(blk(bk_tag_sz-1 downto 0)));
  end;

  function tag_val(heap: heap_t; v: value) return natural is
  begin
    return b_tag(heap(ptr_val(v)));
  end;

  function field(heap: heap_t; v: value; n: natural) return value is
  begin
    return heap(ptr_val(v)+n+1);
  end;

  function mk_header(tag: natural; wo_size: natural) return block_t is
    variable b: block_t;
  begin
    b(word_size-1 downto bk_tag_sz) := std_logic_vector(to_unsigned(wo_size,word_size-bk_tag_sz));
    b(bk_tag_sz-1 downto 0) := std_logic_vector(to_unsigned(tag,bk_tag_sz));
    return b;
  end;
  
  -- procedure alloc_header(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; b_tag: in natural; wo_size: in natural) is
  -- begin
  --   assert(h_ptr+wo_size+1<heap_size);
  --   heap(h_ptr) <= mk_header(b_tag, wo_size);
  --   -- h_ptr <= h_ptr+1;
  -- end;

  -- procedure alloc_block(signal heap: inout heap_t; signal h_ptr: inout heap_ptr; v: value) is
  -- begin
  --   assert(h_ptr+1<heap_size);
  --   heap(h_ptr) <= v;
  --   -- h_ptr <= h_ptr+1;
  -- end;

  function slv_int_image(v : std_logic_vector) return string is
  begin
    return integer'image(to_integer(unsigned(v)));
  end;

  function string_of_value(v : value) return string is
  begin
    if is_imm(v) then
      return "imm<" &  integer'image(int_val(v)) & ">";
    elsif is_ptr(v) then
      return "ptr<" &  integer'image(ptr_val(v)) & ">";
    else
      return "???";
    end if;
  end function;
  
  function string_of_block(b : block_t) return string is
  begin
   return "tag=" &  slv_int_image(b(bk_tag_sz-1 downto 0)) & " sz=" &  slv_int_image(b(bk_tag_sz-1 downto 0));
  end function;
    
  procedure dump_heap(signal heap: heap_t; signal h_ptr: heap_ptr) is
    variable p, q: heap_ptr;
    variable b: block_t;
    variable v: value;
    variable sz: natural;
  begin
    assert false report "heap [sz=" & integer'image(h_ptr) & "] ------- " severity note;
    p := 0;
    while p < h_ptr loop
      b := heap(p);
      assert false report "heap(" &  integer'image(p) & ")=" & string_of_block(b) severity note;
      sz := b_size(b);
      for q in p+1 to p+sz loop
        v := heap(q);
        assert false report "  heap(" &  integer'image(q) & ")=" & string_of_value(v) severity note;
      end loop;
      p := p+sz+1;
    end loop;
    assert false report "-------------- " severity note;
  end;

end package body;
