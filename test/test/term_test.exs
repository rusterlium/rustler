defmodule RustlerTest.TermTest do
  use ExUnit.Case, async: true

  test "term_debug" do
    assert RustlerTest.term_debug(33) == "33"
    assert RustlerTest.term_debug("hello world") == "<<\"hello world\">>"
    assert RustlerTest.term_debug("饂") == "<<233,165,130>>"
    assert RustlerTest.term_debug({:atom, :pair}) == "{atom,pair}"
    assert RustlerTest.term_debug(0..1000) == "\#{'__struct__'=>'Elixir.Range',first=>0,last=>1000}"
    assert RustlerTest.term_debug(Enum.to_list(0..5)) == "[0,1,2,3,4,5]"
    assert RustlerTest.term_debug(Enum.to_list(0..1000)) == "[#{Enum.join(0..1000, ",")}]"
    #assert RustlerTest.term_debug([[[[]],[]],[[]],[]]) == "[[[[]],[]],[[]],[]]"
    
    sues = Enum.map(1..500, fn i -> %{name: "Aunt Sue", id: i} end)
    sue_strs = Enum.map(1..500, fn i -> "\#{id=>#{i},name=><<\"Aunt Sue\">>}" end)
    assert RustlerTest.term_debug(sues) == "[#{Enum.join(sue_strs, ",")}]"
  end
end
