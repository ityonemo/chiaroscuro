defmodule Disasm do
  defstruct ~w(exports imports locals attribs cinfo code literals atoms)a

  def bin(binary) do
    parsed =
      binary
      |> parse
      |> post_process

    exports = parsed.exports
    attribs = parsed.attribs
    cinfo = parsed.cinfo
    code = parsed.code

    {:beam_file, Basic, Enum.reverse(exports), attribs, cinfo, code}
  end

  defp align(size) do
    size +
      case rem(size, 4) do
        0 -> 0
        other -> 4 - other
      end
  end

  def parse(_, so_far \\ nil)

  def parse(<<"FOR1", size::integer-size(32), rest::binary-size(size)>>, nil) do
    parse(rest, %__MODULE__{})
  end

  def parse("BEAM" <> rest, s = %__MODULE__{}) do
    parse(rest, s)
  end

  def parse(<<"AtU8", size::integer-size(32), rest!::binary>>, s) do
    # skip the atoms table
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, %{s | atoms: parse_atoms(table)})
  end

  def parse(<<"Code", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, %{s | code: parse_code(table)})
  end

  def parse(<<"StrT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, s)
  end

  def parse(<<"ImpT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, %{s | imports: parse_imports(table)})
  end

  def parse(<<"ExpT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, %{s | exports: parse_exports(table)})
  end

  def parse(<<"LitT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, %{s | literals: parse_literals(table)})
  end

  def parse(<<"LocT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, %{s | locals: parse_locals(table)})
  end

  def parse(<<"Attr", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, %{s | attribs: parse_attribs(table)})
  end

  def parse(<<"CInf", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, %{s | cinfo: parse_info(table)})
  end

  def parse(<<"Dbgi", size::integer-size(32), rest!::binary>>, s) do
    # not used for our purposes
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, s)
  end

  def parse(<<"Docs", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, s)
  end

  def parse(<<"ExCk", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, s)
  end

  def parse(<<"Line", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse(rest!, s)
  end

  def parse(<<>>, module), do: module

  defmodule Atoms do
    defstruct [:index, :atom]
  end

  def parse_atoms(<<count::integer-size(32), atoms::binary>>) do
    {atoms, _} =
      Enum.flat_map_reduce(1..count, atoms, fn
        _, <<>> ->
          {:halt, nil}

        _, <<0>> <> _ ->
          {:halt, nil}

        _, <<size, atom::binary-size(size), rest::binary>> ->
          {[String.to_atom(atom)], rest}
      end)

    atoms
  end

  defmodule Export do
    defstruct ~w(function arity label)a

    def post_process(export, source) do
      function = Enum.at(source.atoms, export.function - 1)
      {function, export.arity, export.label}
    end
  end

  def parse_exports(<<count::integer-size(32), exports::binary>>) do
    {exports, ""} =
      Enum.map_reduce(1..count, exports, fn
        _, <<f::integer-size(32), a::integer-size(32), l::integer-size(32), rest::binary>> ->
          {%Export{function: f, arity: a, label: l}, rest}
      end)
    exports
  end

  def parse_imports(v), do: parse_exports(v)
  def parse_locals(v), do: parse_exports(v)

  defdelegate parse_attribs(table), to: :erlang, as: :binary_to_term

  defdelegate parse_info(table), to: :erlang, as: :binary_to_term

  def parse_literals(<<_size::integer-size(32), zipped::binary>>) do
    <<count::integer-size(32), terms::binary>> = :zlib.uncompress(zipped)

    {terms, _} =
      Enum.map_reduce(1..count, terms, fn
        _, <<size::integer-size(32), etf::binary-size(size), rest::binary>> ->
          {:erlang.binary_to_term(etf), rest}
      end)

    terms
  end

  defp post_process(module = %__MODULE__{}) do
    updated_module = %{module |
      exports: Enum.map(module.exports, &Export.post_process(&1, module)),
      imports: Enum.map(module.imports, &Export.post_process(&1, module)),
      locals: Enum.map(module.locals, &Export.post_process(&1, module)),
    }

    new_code = Enum.map(module.code, &interpolate_element(&1, updated_module))

    updated_module
    |> Map.put(:code, new_code)
    |> intersperse_functions
  end

  defp interpolate_element({:atom, 0}, _) do
    nil
  end

  defp interpolate_element({:atom, index}, module) do
    {:atom, Enum.at(module.atoms, index - 1)}
  end

  defp interpolate_element({:literal, index}, module) do
    {:literal, Enum.at(module.literals, index)}
  end

  defp interpolate_element({:call_only, arity, {:f, index}}, module = %{atoms: [m | _]}) do
    {fun, ^arity, _} = Enum.find(module.locals ++ module.exports, &(elem(&1, 2) == index))
    {:call_only, arity, {m, fun, arity}}
  end

  defp interpolate_element({:call_ext_only, arity, index}, module) do
    {m, atom_index, ^arity} = Enum.at(module.imports, index)
    fun = Enum.at(module.atoms, atom_index - 1)

    {:call_ext_only, arity, {:extfunc, m, fun, arity}}
  end

  defp interpolate_element(list, module) when is_list(list) do
    Enum.map(list, &interpolate_element(&1, module))
  end

  defp interpolate_element(tuple, module) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> interpolate_element(module)
    |> List.to_tuple()
  end

  defp interpolate_element(any, _), do: any

  defp intersperse_functions(module) do
    all_functions = Enum.sort_by(module.exports ++ module.locals, &elem(&1, 2))

    chunked_code =
      module.code
      |> chunk(all_functions, [])
      |> Enum.map(&Enum.reverse/1)
      |> Enum.reverse()
      |> Enum.zip(all_functions)
      |> Enum.map(fn {code, {fun, arity, label}} ->
        {:function, fun, arity, label, code}
      end)

    %{module | code: chunked_code}
  end

  defp chunk([{:label, label}, {:line, line} | rest_code], [{_, _, m} | rest_fn], so_far)
       when m == label + 1 and label != 1 do
    chunk(rest_code, rest_fn, [[{:label, label}, {:line, line}] | so_far])
  end

  defp chunk([{:label, label} | rest_code], [{_, _, m} | rest_fn], so_far)
       when m == label + 1 do
    chunk(rest_code, rest_fn, [[{:label, label}] | so_far])
  end

  defp chunk([code | rest_code], funs, [head | rest]) do
    chunk(rest_code, funs, [[code | head] | rest])
  end

  defp chunk([], _funs, so_far), do: so_far

  def parse_code(
        <<subheader_size::integer-size(32), _subheader::binary-size(subheader_size),
          rest::binary>>
      ) do
    parse_opcode(rest, [])
  end

  @opcodes %{
    1 => {:label, 1},
    2 => {:func_info, 3},
    3 => {:int_code_end, 0},
    4 => {:call, 2},
    5 => {:call_last, 3},
    6 => {:call_only, 2},
    7 => {:call_ext, 2},
    8 => {:call_ext_last, 3},
    9 => {:bif0, 3},
    10 => {:bif1, 4},
    11 => {:bif2, 5},
    12 => {:allocate, 2},
    13 => {:allocate_heap, 3},
    14 => {:allocate_zero, 2},
    15 => {:allocate_heap_zero, 3},
    16 => {:test_heap, 2},
    17 => {:init, 1},
    18 => {:deallocate, 1},
    19 => {:return, 0},
    20 => {:send, 0},
    21 => {:remove_message, 0},
    22 => {:timeout, 0},
    23 => {:loop_rec, 2},
    24 => {:loop_rec_end, 1},
    25 => {:wait, 1},
    26 => {:wait_timeout, 2},
    27 => {:m_plus, 4},
    28 => {:m_minus, 4},
    29 => {:m_times, 4},
    30 => {:m_div, 4},
    31 => {:int_div, 4},
    32 => {:int_rem, 4},
    33 => {:int_band, 4},
    34 => {:int_bor, 4},
    35 => {:int_bxor, 4},
    36 => {:int_bsl, 4},
    37 => {:int_bsr, 4},
    38 => {:int_bnot, 3},
    39 => {:is_lt, 3},
    40 => {:is_ge, 3},
    41 => {:is_eq, 3},
    42 => {:is_ne, 3},
    43 => {:is_eq_exact, 3},
    44 => {:is_ne_exact, 3},
    45 => {:is_integer, 2},
    46 => {:is_float, 2},
    47 => {:is_number, 2},
    48 => {:is_atom, 2},
    49 => {:is_pid, 2},
    50 => {:is_reference, 2},
    51 => {:is_port, 2},
    52 => {:is_nil, 2},
    53 => {:is_binary, 2},
    54 => {:is_constant, 2},
    55 => {:is_list, 2},
    56 => {:is_nonempty_list, 2},
    57 => {:is_tuple, 2},
    58 => {:test_arity, 3},
    59 => {:select_val, 3},
    60 => {:select_tuple_arity, 3},
    61 => {:jump, 1},
    62 => {:catch, 1},
    63 => {:catch_end, 1},
    64 => {:move, 2},
    65 => {:get_list, 3},
    66 => {:get_tuple_element, 3},
    67 => {:set_tuple_element, 3},
    68 => {:put_string, 3},
    69 => {:put_list, 3},
    70 => {:put_tuple, 2},
    71 => {:put, 1},
    72 => {:badmatch, 1},
    73 => {:if_end, 0},
    74 => {:case_end, 1},
    75 => {:call_fun, 1},
    76 => {:make_fun, 3},
    77 => {:is_function, 2},
    78 => {:call_ext_only, 2},
    79 => {:bs_start_match, 2},
    80 => {:bs_get_integer, 2},
    81 => {:bs_get_float, 2},
    82 => {:bs_get_binary, 2},
    83 => {:bs_skip_bits, 2},
    84 => {:bs_test_tail, 2},
    85 => {:bs_save, 1},
    86 => {:bs_restore, 1},
    87 => {:bs_init, 2},
    88 => {:bs_final, 2},
    89 => {:bs_put_integer, 5},
    90 => {:bs_put_binary, 5},
    91 => {:bs_put_float, 5},
    92 => {:bs_put_string, 2},
    93 => {:bs_need_buf, 1},
    94 => {:fclearerror, 0},
    95 => {:fcheckerror, 1},
    96 => {:fmove, 4},
    97 => {:fconv, 4},
    98 => {:fadd, 4},
    99 => {:fsub, 4},
    100 => {:fmul, 4},
    101 => {:fdiv, 4},
    102 => {:fnegate, 3},
    103 => {:make_fun2, 0},
    104 => {:try, 1},
    105 => {:try_end, 1},
    106 => {:try_case, 1},
    107 => {:try_case_end, 1},
    108 => {:raise, 2},
    109 => {:bs_init2, 6},
    110 => {:bs_bits_to_bytes, 3},
    111 => {:bs_add, 3},
    112 => {:apply, 1},
    113 => {:apply_last, 2},
    114 => {:is_boolean, 2},
    115 => {:is_function2, 3},
    116 => {:bs_start_match2, 5},
    117 => {:bs_get_integer2, 7},
    118 => {:bs_get_float2, 7},
    119 => {:bs_get_binary2, 7},
    120 => {:bs_skip_bits2, 5},
    121 => {:bs_test_tail2, 3},
    122 => {:bs_save2, 2},
    123 => {:bs_restore2, 2},
    124 => {:gc_bif1, 5},
    125 => {:gc_bif2, 6},
    126 => {:bs_final2, 2},
    127 => {:bs_bits_to_bytes2, 2},
    128 => {:put_literal, 2},
    129 => {:is_bitstr, 2},
    130 => {:bs_context_to_binary, 1},
    131 => {:bs_test_unit, 3},
    132 => {:bs_match_string, 4},
    133 => {:bs_init_writable, 0},
    134 => {:bs_append, 8},
    135 => {:bs_private_append, 6},
    136 => {:trim, 2},
    137 => {:bs_init_bits, 6},
    138 => {:bs_get_utf8, 5},
    139 => {:bs_skip_utf8, 4},
    140 => {:bs_get_utf16, 5},
    141 => {:bs_skip_utf16, 4},
    142 => {:bs_get_utf32, 5},
    143 => {:bs_skip_utf32, 4},
    144 => {:bs_utf8_size, 3},
    145 => {:bs_put_utf8, 3},
    146 => {:bs_utf16_size, 3},
    147 => {:bs_put_utf16, 3},
    148 => {:bs_put_utf32, 3},
    149 => {:on_load, 0},
    150 => {:recv_mark, 1},
    151 => {:recv_set, 1},
    152 => {:gc_bif3, 7},
    153 => {:line, 1},
    154 => {:put_map_assoc, 5},
    155 => {:put_map_exact, 5},
    156 => {:is_map, 2},
    157 => {:has_map_field, 3},
    158 => {:get_map_element, 4}
  }

  def parse_opcode(<<>>, so_far), do: Enum.reverse(so_far)
  def parse_opcode(<<0>> <> _, so_far), do: Enum.reverse(so_far)

  def parse_opcode(<<opcode, rest::binary>>, so_far) do
      case Map.fetch!(@opcodes, opcode) do
        {:int_code_end, 0} ->
          Enum.reverse(so_far)
        {opcode, 0} ->
          parse_opcode(rest, [opcode | so_far])

        {opcode, arity} ->
          {terms, rest} = fetch_terms(rest, arity, [])
          op = List.to_tuple([opcode | Enum.reverse(terms)])
          parse_opcode(rest, [op | so_far])
      end
  end

  def fetch_terms(rest, arity, so_far, opt \\ [])

  def fetch_terms(rest, 0, so_far, _), do: {so_far, rest}

  def fetch_terms(<<x::5, 0b000::3, rest::binary>>, arity, so_far, opts) do
    {v, rest} = resegmented(<<x::5>>, rest)
    fetch_terms(rest, arity - 1, [v | so_far], opts)
  end

  def fetch_terms(<<x::5, 0b001::3, rest::binary>>, arity, so_far, opts) do
    {v, rest} = resegmented(<<x::5>>, rest)
    fetch_terms(rest, arity - 1, [{:integer, v} | so_far], opts)
  end

  def fetch_terms(<<x::5, 0b010::3, rest::binary>>, arity, so_far, opts) do
    {v, rest} = resegmented(<<x::5>>, rest)
    fetch_terms(rest, arity - 1, [{:atom, v} | so_far], opts)
  end

  def fetch_terms(<<x::5, 0b011::3, rest::binary>>, arity, so_far, opts) do
    {v, rest} = resegmented(<<x::5>>, rest)
    fetch_terms(rest, arity - 1, [{:x, v} | so_far], opts)
  end

  def fetch_terms(<<x::5, 0b100::3, rest::binary>>, arity, so_far, opts) do
    {v, rest} = resegmented(<<x::5>>, rest)
    fetch_terms(rest, arity - 1, [{:y, v} | so_far], opts)
  end

  def fetch_terms(<<x::5, 0b101::3, rest::binary>>, arity, so_far, opts) do
    {v, rest} = resegmented(<<x::5>>, rest)

    fetch_terms(rest, arity - 1, [{:f, v} | so_far], opts)
  end

  def fetch_terms(<<x::5, 0b110::3, rest::binary>>, arity, so_far, opts) do
    {v, rest} = resegmented(<<x::5>>, rest)
    fetch_terms(rest, arity - 1, [{:char, v} | so_far], opts)
  end

  def fetch_terms(<<0b0010_111, rest::binary>>, arity, so_far, opts) do
    # list
    {size, rest} = fetch_term(rest)

    {terms, rest} =
      Enum.flat_map_reduce(1..size, rest, fn
        _, rest -> fetch_terms(rest, 1, [], opts)
      end)

    fetch_terms(rest, arity - 1, [{:list, terms} | so_far], opts)
  end

  def fetch_terms(<<0b1000_111, rest::binary>>, arity, so_far, opts) do
    # literal lookup
    {term, rest} = fetch_term(rest)
    fetch_terms(rest, arity - 1, [{:literal, term} | so_far], opts)
  end

  def fetch_terms(<<x::8, _::binary>>, _arity, _so_far, opts) do
    raise Integer.to_string(x, 2)
  end

  defp fetch_term(binary) do
    {[term], rest} = fetch_terms(binary, 1, [])
    {term, rest}
  end

  def resegmented(<<x::4, 0::1>>, rest), do: {x, rest}

  def resegmented(<<x::3, 0b01::2>>, <<y>> <> rest) do
    {Bitwise.<<<(x, 8) + y, rest}
  end

  def resegmented(<<size::3, 0b11::2>>, rest) do
    bits = (size + 2) * 8
    <<value::size(bits), rest::binary>> = rest
    {value, rest}
  end
end
