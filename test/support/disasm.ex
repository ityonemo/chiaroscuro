defmodule Disasm do
  defstruct ~w(attribs cinfo code literals atoms strings)a ++
              [exports: [], imports: [], locals: [], lambdas: []]

  require Logger

  @doc """
  disassembles a .beam file and outputs the code segment
  """
  def bin(binary, opts \\ []) do
    as_disasm = Keyword.get(opts, :as_disasm, true)

    parsed =
      binary
      |> parse_chunks
      |> post_process(opts)

    if as_disasm do
      exports = parsed.exports
      attribs = parsed.attribs
      cinfo = parsed.cinfo
      code = parsed.code

      {:beam_file, List.first(parsed.atoms), Enum.sort_by(exports, &(&1)), attribs, cinfo, code}
    else
      parsed
    end
  catch
    {:error, error} ->
      opts |> IO.inspect(label: "30")
      if module = opts[:module] do
        raise "error #{error} disassembling #{inspect module}"
      else
        raise "error #{error}"
      end
  end

  @doc """
  Performs a tail-call binary parse over IFF "chunks" in the BEAM file.

  The erlang BEAM file follows the "interchange file format" which is defined
  by EA (https://en.wikipedia.org/wiki/Interchange_File_Format).  Each chunk
  has a 4-byte header followed by a 4-byte size entry, followed by the contents
  of the chunk.

  Note that overall the IFF is a 4-byte aligned standard, but the size entry
  does not have to be a multiple fo 4.  Splitting into chunks must therefore
  be aligned.
  """
  def parse_chunks(_, so_far \\ nil)

  def parse_chunks(<<"FOR1", size::integer-size(32), rest::binary-size(size)>>, nil) do
    parse_chunks(rest, %__MODULE__{})
  end

  def parse_chunks("BEAM" <> rest, s = %__MODULE__{}) do
    parse_chunks(rest, s)
  end

  def parse_chunks(<<"AtU8", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing AtU8")
    # skip the atoms table
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | atoms: parse_atoms(table)})
  end

  def parse_chunks(<<"Code", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing Code")
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | code: parse_code(table)})
  end

  def parse_chunks(<<"StrT", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing StrT")
    aligned = align(size)
    <<strings::binary-size(aligned), rest!::binary>> = rest!

    parse_chunks(rest!, %{s | strings: strings})
  end

  def parse_chunks(<<"ImpT", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing ImpT")
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | imports: parse_functions(table)})
  end

  def parse_chunks(<<"ExpT", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing ExpT")
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | exports: parse_functions(table)})
  end

  def parse_chunks(<<"LocT", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing LocT")
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | locals: parse_functions(table)})
  end

  def parse_chunks(<<"FunT", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing FunT")
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | lambdas: parse_lambdas(table)})
  end

  def parse_chunks(<<"LitT", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing LitT")
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | literals: parse_literals(table)})
  end

  def parse_chunks(<<"Attr", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing Attr")
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | attribs: parse_attribs(table)})
  end

  def parse_chunks(<<"CInf", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing Cinf")
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | cinfo: parse_info(table)})
  end

  def parse_chunks(<<"Dbgi", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing Dbgi")
    # not used for our purposes
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, s)
  end

  def parse_chunks(<<"Docs", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing Docs")
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, s)
  end

  def parse_chunks(<<"ExCk", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing ExCk")
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, s)
  end

  def parse_chunks(<<"Line", size::integer-size(32), rest!::binary>>, s) do
    Logger.debug("parsing Line")
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, s)
  end

  def parse_chunks(<<>>, module), do: module

  defp align(size) when rem(size, 4) == 0, do: size
  defp align(size), do: size + 4 - rem(size, 4)

  #############################################################################
  ## ATOM TABLE PARSING

  @doc """
  parses the Atoms table

  see:
  http://beam-wisdoms.clau.se/en/latest/indepth-beam-file.html#atom-and-atu8-atoms-table

  The atoms table is 32-bit count field followed by a flat list of unterminated
  strings.  Each string is preceded by a one-byte "size" byte, which specifies
  how many bytes are in the atom string representation.  Note that this means
  that atoms cannot back a string that is more than 255 bytes long, which is
  indeed a restriction in the beam VM.
  """
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

  #############################################################################
  ## FUNCTION TABLE PARSING

  @doc """
  parses the Functions table (import, export, local)

  These tables are lists of triples:
  - `ExpT`: exported functions
  - `LocT`: local (private) functions
  - `ImpT`: imported functions

  First 4-bytes is the number of triples declared.
  followed by triples of:
  - function atom (index in atoms table)
  - function arity
  - function entry point label

  For ImpT, the triple is:
  - module atom (index in atoms table)
  - function atom (index in atoms table)
  - function arity
  """

  defmodule Function do
    defstruct ~w(i0 i1 i2)a

    def post_process(export, source, :imports) do
      module = Enum.at(source.atoms, export.i0 - 1)
      function = Enum.at(source.atoms, export.i1 - 1)

      {module, function, export.i2}
    end

    def post_process(export, source, _) do
      function = Enum.at(source.atoms, export.i0 - 1)
      {function, export.i1, export.i2}
    end
  end

  def parse_functions(<<count::integer-size(32), exports::binary>>) do
    1..count
    |> Enum.map_reduce(exports, fn
      _, <<f::integer-size(32), a::integer-size(32), l::integer-size(32), rest::binary>> ->
        {%Function{i0: f, i1: a, i2: l}, rest}
      _, <<>> ->
        throw {:error, "incomplete function list"}
    end)
    |> elem(0)
  end

  #############################################################################
  ## LAMBDA TABLE PARSING

  defmodule Lambda do
    defstruct ~w(fun arity label offset nfree ouniq)a
  end


  @doc """
  parses the Functions table (import, export, local, lambdas)
  """

  def parse_lambdas(<<count::integer-size(32), exports::binary>>) do
    1..count
    |> Enum.map_reduce(exports, fn
      _, <<f::integer-size(32), a::integer-size(32), offset::integer-size(32),
           l::integer-size(32), nfree::integer-size(32), ouniq::integer-size(32),
           rest::binary>> ->
        {%Lambda{fun: f, arity: a, label: l, offset: offset, nfree: nfree, ouniq: ouniq}, rest}
    end)
    |> elem(0)
  end

  #############################################################################
  ## ETF TABLES

  @doc """
  parses the attributes table.  This is just erlang term format encoding.
  """
  defdelegate parse_attribs(table), to: :erlang, as: :binary_to_term

  @doc """
  parses the info table.  This is just erlang term format encoding.
  """
  defdelegate parse_info(table), to: :erlang, as: :binary_to_term

  #############################################################################
  ## Literals table

  @doc """
  parses the literals table.  This is a zlib-compressed list of erlang terms;
  with each term prepended by its size.
  """
  def parse_literals(<<_size::integer-size(32), zipped::binary>>) do
    <<count::integer-size(32), terms::binary>> = :zlib.uncompress(zipped)

    1..count
    |> Enum.map_reduce(terms, fn
      _, <<size::integer-size(32), etf::binary-size(size), rest::binary>> ->
        {:erlang.binary_to_term(etf), rest}
    end)
    |> elem(0)
  end

  #############################################################################
  ## Code Chunk

  @doc """
  parses the code chunk

  The code chunk is encoded using the erlang compact term encoding system:

  http://beam-wisdoms.clau.se/en/latest/indepth-beam-file.html#beam-compact-term-encoding

  This function walks the code chunk section and uses a tail-call to emit a list
  of opcode tuples.
  """

  def parse_code(
        <<subheader_size::integer-size(32), _subheader::binary-size(subheader_size),
          rest::binary>>
      ) do
    parse_opcode(rest, [])
  end

  # use typespecs to get the highest opcode number.
  {:ok, specs} = Code.Typespec.fetch_specs(:beam_opcodes)
  {{:opname, 1}, [{:type, _, :fun, [product, _]}]} = Enum.find(specs, &(elem(&1, 0) == {:opname, 1}))
  {:type, _, :product, [range]} = product
  {:type, _, :range, [_, {:integer, _, limit}]} = range
  # assemble a dict of all beam opcodes, by number
  @opcodes Map.new(1..limit, &{&1, :beam_opcodes.opname(&1)})
  def opcodes, do: @opcodes

  @spec parse_opcode(binary, any) :: list
  def parse_opcode(<<>>, so_far), do: Enum.reverse(so_far)
  def parse_opcode(<<0>> <> _, so_far), do: Enum.reverse(so_far)

  def parse_opcode(<<opcode, rest::binary>>, so_far) do
    case Map.fetch!(@opcodes, opcode) do
      {:int_code_end, 0} ->
        Enum.reverse(so_far)

      {opcode, 0} ->
        parse_opcode(rest, [opcode | so_far])

      {opcode, arity} ->
        {terms, rest} = parse_compact_terms(rest, arity, [])
        op = List.to_tuple([opcode | Enum.reverse(terms)])
        parse_opcode(rest, [op | so_far])
    end
  end

  defp parse_compact_terms(rest, arity, so_far)

  defp parse_compact_terms(rest, 0, so_far), do: {so_far, rest}

  defp parse_compact_terms(<<x::5, 0b000::3, rest::binary>>, arity, so_far) do
    {v, rest} = resegmented(<<x::5>>, rest)
    parse_compact_terms(rest, arity - 1, [v | so_far])
  end

  defp parse_compact_terms(<<x::5, 0b001::3, rest::binary>>, arity, so_far) do
    {v, rest} = resegmented_integer(<<x::5>>, rest)
    parse_compact_terms(rest, arity - 1, [{:integer, v} | so_far])
  end

  defp parse_compact_terms(<<x::5, 0b010::3, rest::binary>>, arity, so_far) do
    {v, rest} = resegmented(<<x::5>>, rest)
    parse_compact_terms(rest, arity - 1, [{:atom, v} | so_far])
  end

  defp parse_compact_terms(<<x::5, 0b011::3, rest::binary>>, arity, so_far) do
    {v, rest} = resegmented(<<x::5>>, rest)
    parse_compact_terms(rest, arity - 1, [{:x, v} | so_far])
  end

  defp parse_compact_terms(<<x::5, 0b100::3, rest::binary>>, arity, so_far) do
    {v, rest} = resegmented(<<x::5>>, rest)
    parse_compact_terms(rest, arity - 1, [{:y, v} | so_far])
  end

  defp parse_compact_terms(<<x::5, 0b101::3, rest::binary>>, arity, so_far) do
    {v, rest} = resegmented(<<x::5>>, rest)

    parse_compact_terms(rest, arity - 1, [{:f, v} | so_far])
  end

  defp parse_compact_terms(<<x::5, 0b110::3, rest::binary>>, arity, so_far) do
    {v, rest} = resegmented(<<x::5>>, rest)
    parse_compact_terms(rest, arity - 1, [{:char, v} | so_far])
  end

  defp parse_compact_terms(<<0b0010_111, rest::binary>>, arity, so_far) do
    # list
    {size, rest} = parse_compact_term(rest)

    {terms, rest} =
      Enum.flat_map_reduce(1..size, rest, fn
        _, rest -> parse_compact_terms(rest, 1, [])
      end)

    parse_compact_terms(rest, arity - 1, [{:list, terms} | so_far])
  end

  defp parse_compact_terms(<<0b0100_111, rest!::binary>>, arity, so_far) do
    # decodes an fp register
    {reg, rest!} = parse_compact_term(rest!)
    parse_compact_terms(rest!, arity - 1, [{:fr, reg} | so_far])
  end

  defp parse_compact_terms(<<0b0110_111, rest!::binary>>, arity, so_far) do
    # decodes an allocation list
    {length, rest!} = parse_compact_term(rest!)
    {alloc_list, rest!} = parse_alloc_list(rest!, length, [])
    parse_compact_terms(rest!, arity - 1, [{:alloc, alloc_list} | so_far])
  end

  defp parse_compact_terms(<<0b1000_111, rest::binary>>, arity, so_far) do
    # literal lookup
    {term, rest} = parse_compact_term(rest)
    parse_compact_terms(rest, arity - 1, [{:literal, term} | so_far])
  end

  defp parse_compact_terms(<<x::8, _::binary>>, _arity, _so_far) do
    throw {:error, "unknown compact term: #{Integer.to_string(x, 2)}"}
  end

  # special case for a single compact term
  defp parse_compact_term(binary) do
    {[term], rest} = parse_compact_terms(binary, 1, [])
    {term, rest}
  end

  defp resegmented(<<x::4, 0::1>>, rest), do: {x, rest}

  defp resegmented(<<x::3, 0b01::2>>, <<y>> <> rest) do
    {Bitwise.<<<(x, 8) + y, rest}
  end

  defp resegmented(<<size::3, 0b11::2>>, rest!) do
    bits = (size + 2) * 8
    <<value::size(bits), rest!::binary>> = rest!
    {value, rest!}
  end

  defp resegmented_integer(<<0b11111::5>>, rest!) do
    {size, rest!} = parse_compact_term(rest!)
    integer_bits = (size + 9) * 8
    <<value::integer-size(integer_bits), rest!::binary>> = rest!
    {value, rest!}
  end

  defp resegmented_integer(<<size::3, 0b11::2>>, rest!) do
    bits = (size + 2) * 8
    <<value::signed-integer-size(bits), rest!::binary>> = rest!
    {value, rest!}
  end

  defp resegmented_integer(other, rest), do: resegmented(other, rest)

  @alloc_list_types %{0 => :words, 1 => :floats, 2 => :funs}

  defp parse_alloc_list(rest, 0, so_far), do: {Enum.reverse(so_far), rest}
  defp parse_alloc_list(rest!, length, so_far) do
    {type, rest!} = parse_compact_term(rest!)
    {value, rest!} = parse_compact_term(rest!)
    parse_alloc_list(rest!, length - 1, [{@alloc_list_types[type], value} | so_far])
  end

  #############################################################################
  ## postprocessing.

  defp post_process(module = %__MODULE__{}, _opts) do
    updated_module = %{
      module
      | exports: Enum.map(module.exports, &Function.post_process(&1, module, :exports)),
        imports: Enum.map(module.imports, &Function.post_process(&1, module, :imports)),
        locals: Enum.map(module.locals, &Function.post_process(&1, module, :locals))
    }

    new_code = Enum.map(module.code, &reinterpret(&1, updated_module))

    updated_module
    |> Map.put(:code, new_code)
    |> group_by_function
  end

  # some opcodes need to be reinterpreted to be match the format of
  # :beam_disasm, which includes looking up and substituting indicia for
  # certain other tables.
  defp reinterpret({:atom, 0}, _) do
    nil
  end

  defp reinterpret({:atom, index}, module) do
    {:atom, Enum.at(module.atoms, index - 1)}
  end

  defp reinterpret({:literal, index}, module) do
    case Enum.at(module.literals, index) do
      float when is_float(float) -> {:float, float}
      other -> {:literal, other}
    end
  end

  @local_calls ~w(call call_only call_last)a

  defp reinterpret(call, module = %{atoms: [m | _]}) when elem(call, 0) in @local_calls do
    [local, a, {:f, index} | rest!] = Tuple.to_list(call)
    {f, ^a, ^index} = Enum.find(module.locals ++ module.exports, &(elem(&1, 2) == index))
    rest = Enum.map(rest!, &reinterpret(&1, module))
    List.to_tuple([local, a, {m, f, a} | rest])
  end

  @remote_calls ~w(call_ext call_ext_only call_ext_last)a

  defp reinterpret(call, module) when elem(call, 0) in @remote_calls do
    [remote, a, index | rest!] = Tuple.to_list(call)
    {m, f, ^a} = Enum.at(module.imports, index)
    rest! = Enum.map(rest!, &reinterpret(&1, module))
    List.to_tuple([remote, a, {:extfunc, m, f, a} | rest!])
  end

  defp reinterpret({:make_fun2, index}, module = %{atoms: [m | _]}) do
    lambda = %{fun: fun, arity: a} = Enum.at(module.lambdas, index)
    f = Enum.at(module.atoms, fun - 1)

    {:make_fun2, {m, f, a}, index, lambda.ouniq, lambda.nfree}
  end

  @tests ~w(is_tuple is_nonempty_list is_tagged_tuple is_integer is_lt is_ge is_eq_exact test_arity
    is_pid is_atom is_nil is_binary is_list is_map bs_test_unit bs_test_tail2 bs_start_match2 is_eq
    is_ne_exact is_function is_ne is_function2 is_boolean is_float is_reference is_number is_bitstr
    is_port)a
  defp reinterpret(test, module) when elem(test, 0) in @tests do
    [name, jump | args] = Tuple.to_list(test)
    {:test, name, jump, reinterpret(args, module)}
  end

  defp reinterpret({:has_map_fields, fail, src, lst}, module) do
    {:test, :has_map_fields, fail, src, reinterpret(lst, module)}
  end

  defp reinterpret({:bs_match_string, fail, src, len, pos}, module) do
    content = :erlang.binary_part(module.strings, {pos, aligned_bytes(len)})
    {:test, :bs_match_string, fail, [src, len, content]}
  end

  @float_ops ~w(fdiv fmul fsub fadd fnegate)a

  defp reinterpret(op, module) when elem(op, 0) in @float_ops do
    [operand, fail | rest] = Tuple.to_list(op)
    {args, [dest]} = Enum.split(rest, -1)
    {:arithfbif, operand, fail, reinterpret(args, module), dest}
  end

  @builtins ~w(bs_add)a
  defp reinterpret(builtin, _module) when elem(builtin, 0) in @builtins do
    [fun, fail | rest] = Tuple.to_list(builtin)
    {args, [dest]} = Enum.split(rest, -1)
    {fun, fail, args, dest}
  end

  defp reinterpret({:bif0, index, dest}, module) do
    name = module.imports
    |> Enum.at(index)
    |> elem(1)

    {:bif, name, :nofail, [], dest}
  end

  defp reinterpret({:bif1, fail, index, arg, dest}, module) do
    name = module.imports
    |> Enum.at(index)
    |> elem(1)

    a2 = reinterpret(arg, module)
    {:bif, name, fail, [a2], dest}
  end

  defp reinterpret({:bif2, fail, index, a, b, dest}, module) do
    name = module.imports
    |> Enum.at(index)
    |> elem(1)

    args = Enum.map([a, b], &reinterpret(&1, module))
    {:bif, name, fail, args, dest}
  end

  defp reinterpret({:gc_bif1, fail, dealloc, index, arg, dest}, module) do
    name = module.imports
    |> Enum.at(index)
    |> elem(1)

    {:gc_bif, name, fail, dealloc, reinterpret([arg], module), dest}
  end

  defp reinterpret({:gc_bif2, fail, dealloc, index, a1, a2, dest}, module) do
    name = module.imports
    |> Enum.at(index)
    |> elem(1)

    {:gc_bif, name, fail, dealloc, reinterpret([a1, a2], module), dest}
  end

  defp reinterpret({:gc_bif3, fail, dealloc, index, a1, a2, a3, dest}, module) do
    name = module.imports
    |> Enum.at(index)
    |> elem(1)

    {:gc_bif, name, fail, dealloc, reinterpret([a1, a2, a3], module), dest}
  end

  defp reinterpret({:bs_put_string, len, index}, module) do
    str = :binary.part(module.strings, {index, len})
    {:bs_put_string, len, {:string, :binary.bin_to_list(str)}}
  end

  defp reinterpret({:bs_init2, fail, src, a, b, flag, dest}, _module) do
    {:bs_init2, fail, src, a, b, {:field_flags, flag}, dest}
  end

  @bs_field_flags ~w(bs_append bs_put_binary bs_put_float bs_put_utf16 bs_put_integer
  bs_put_utf32 bs_put_utf8 bs_init_bits bs_private_append)a
  @bs_field_tests ~w(bs_get_binary2 bs_get_integer2 bs_get_utf8 bs_get_utf16 bs_get_utf32
  bs_get_float2)a

  defp reinterpret(op, module) when elem(op, 0) in @bs_field_flags do
    {parts, [flag, dest]} = op
    |> Tuple.to_list
    |> Enum.split(-2)

    List.to_tuple(reinterpret(parts ++ [{:field_flags, flag}, dest], module))
  end

  defp reinterpret(op, module) when elem(op, 0) in @bs_field_tests do
    {[operand, fail | rest], [flag, dest]} = op
    |> Tuple.to_list
    |> Enum.split(-2)

    {:test, operand, fail, reinterpret(rest ++ [{:field_flags, flag}, dest], module)}
  end

  defp reinterpret({:bs_skip_bits2, fail, src, a, b, flag}, _module) do
    {:test, :bs_skip_bits2, fail, [src, a, b, {:field_flags, flag}]}
  end

  defp reinterpret({:bs_skip_utf8, fail, src, a, flag}, _module) do
    {:test, :bs_skip_utf8, fail, [src, a, {:field_flags, flag}]}
  end

  defp reinterpret({:bs_skip_utf16, fail, src, a, flag}, _module) do
    {:test, :bs_skip_utf16, fail, [src, a, {:field_flags, flag}]}
  end

  defp reinterpret({:bs_skip_utf32, fail, src, a, flag}, _module) do
    {:test, :bs_skip_utf32, fail, [src, a, {:field_flags, flag}]}
  end

  defp reinterpret({:bs_start_match3, fail, src, len, dst}, _module) do
    {:bs_start_match3, fail, src, {:u, len}, dst}
  end

  defp reinterpret({:bs_start_match4, fail, a, b, dst}, module) do
    {:bs_start_match4, reinterpret(fail, module), {:u, a}, b, dst}
  end

  defp reinterpret({:bs_get_tail, a, b, c}, _module) do
    {:bs_get_tail, a, b, {:u, c}}
  end

  defp reinterpret({:bs_get_position, a, b, c}, _module) do
    {:bs_get_position, a, b, {:u, c}}
  end

  defp reinterpret({:raise, a, b}, module) do
    {:raise, {:f, 0}, reinterpret([a, b], module), {:x, 0}}
  end

  defp reinterpret(list, module) when is_list(list) do
    Enum.map(list, &reinterpret(&1, module))
  end

  defp reinterpret(tuple, module) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> reinterpret(module)
    |> List.to_tuple()
  end

  defp reinterpret(any, _), do: any

  defp aligned_bytes(bit_length) when rem(bit_length, 8) == 0, do: div(bit_length, 8)
  defp aligned_bytes(bit_length), do: div(bit_length, 8) + 1

  defp group_by_function(module) do
    all_functions = Enum.sort_by(module.exports ++ module.locals, &elem(&1, 2))

    chunked_code =
      module.code
      |> group_by(all_functions, [])
      |> Enum.map(&Enum.reverse/1)
      |> Enum.reverse()
      |> Enum.zip(all_functions)
      |> Enum.map(fn {code, {fun, arity, label}} ->
        {:function, fun, arity, label, code}
      end)

    %{module | code: chunked_code}
  end

  defp group_by([{:label, label}, {:line, line} | rest_code], [{_, _, m} | rest_fn], so_far)
       when m == label + 1 and label != 1 do
    group_by(rest_code, rest_fn, [[{:label, label}, {:line, line}] | so_far])
  end

  defp group_by([{:label, label} | rest_code], [{_, _, m} | rest_fn], so_far)
       when m == label + 1 do
    group_by(rest_code, rest_fn, [[{:label, label}] | so_far])
  end

  defp group_by([code | rest_code], funs, [head | rest]) do
    group_by(rest_code, funs, [[code | head] | rest])
  end

  defp group_by([], _funs, so_far), do: so_far
end
