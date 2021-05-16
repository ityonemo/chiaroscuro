defmodule Disasm do
  defstruct ~w(attribs cinfo code literals atoms)a ++
  [exports: [],
   imports: [],
   locals: [],
   lambdas: []]

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

      {:beam_file, Basic, Enum.reverse(exports), attribs, cinfo, code}
    else
      parsed
    end
  end

  @doc """
  Performs a tail-call binary parse over IFF chunks in the BEAM file.

  The erlang BEAM file follows the "interchange file format" which is defined
  by EA (https://en.wikipedia.org/wiki/Interchange_File_Format).  Each chunk
  has a 4-byte header followed by a 4-byte size field, followed by the contents
  of the chunk.

  Note that overall the IFF is a 4-byte aligned standard, but the size field
  does not have to be a multiple fo 4.
  """
  def parse_chunks(_, so_far \\ nil)

  def parse_chunks(<<"FOR1", size::integer-size(32), rest::binary-size(size)>>, nil) do
    parse_chunks(rest, %__MODULE__{})
  end

  def parse_chunks("BEAM" <> rest, s = %__MODULE__{}) do
    parse_chunks(rest, s)
  end

  def parse_chunks(<<"AtU8", size::integer-size(32), rest!::binary>>, s) do
    # skip the atoms table
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | atoms: parse_atoms(table)})
  end

  def parse_chunks(<<"Code", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | code: parse_code(table)})
  end

  def parse_chunks(<<"StrT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, s)
  end

  def parse_chunks(<<"ImpT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | imports: parse_functions(table)})
  end

  def parse_chunks(<<"ExpT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | exports: parse_functions(table)})
  end

  def parse_chunks(<<"LocT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | locals: parse_functions(table)})
  end

  def parse_chunks(<<"FunT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | lambdas: parse_functions(table)})
  end

  def parse_chunks(<<"LitT", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | literals: parse_literals(table)})
  end

  def parse_chunks(<<"Attr", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | attribs: parse_attribs(table)})
  end

  def parse_chunks(<<"CInf", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, %{s | cinfo: parse_info(table)})
  end

  def parse_chunks(<<"Dbgi", size::integer-size(32), rest!::binary>>, s) do
    # not used for our purposes
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, s)
  end

  def parse_chunks(<<"Docs", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, s)
  end

  def parse_chunks(<<"ExCk", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, s)
  end

  def parse_chunks(<<"Line", size::integer-size(32), rest!::binary>>, s) do
    aligned = align(size)
    <<_table::binary-size(aligned), rest!::binary>> = rest!
    parse_chunks(rest!, s)
  end

  def parse_chunks(<<>>, module), do: module

  defp align(size) when rem(size, 4) == 0, do: size
  defp align(size), do: size + rem(size, 4)

  #############################################################################
  ## ATOM TABLE PARSING

  @doc """
  parses the Atoms table

  see:
  http://beam-wisdoms.clau.se/en/latest/indepth-beam-file.html#atom-and-atu8-atoms-table

  The atoms table is 32-bit count field followed by a flat list of unterminated
  strings.  Each string is prepended by a one-byte "size" byte, which specifies
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
  parses the Functions table (import, export, local, lambdas)

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
    {exports, ""} =
      Enum.map_reduce(1..count, exports, fn
        _, <<f::integer-size(32), a::integer-size(32), l::integer-size(32), rest::binary>> ->
          {%Function{i0: f, i1: a, i2: l}, rest}
      end)
    exports
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
  parses the literals table.  This is a zlib-compressed etf.
  """
  def parse_literals(<<_size::integer-size(32), zipped::binary>>) do
    <<count::integer-size(32), terms::binary>> = :zlib.uncompress(zipped)

    {terms, _} =
      Enum.map_reduce(1..count, terms, fn
        _, <<size::integer-size(32), etf::binary-size(size), rest::binary>> ->
          {:erlang.binary_to_term(etf), rest}
      end)

    terms
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

  @opcodes __DIR__ |> Path.join("opcode_table.exs") |> Code.eval_file() |> elem(0)

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
    {v, rest} = resegmented(<<x::5>>, rest)
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

  defp parse_compact_terms(<<0b1000_111, rest::binary>>, arity, so_far) do
    # literal lookup
    {term, rest} = parse_compact_term(rest)
    parse_compact_terms(rest, arity - 1, [{:literal, term} | so_far])
  end

  defp parse_compact_terms(<<x::8, _::binary>>, _arity, _so_far) do
    raise Integer.to_string(x, 2)
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

  defp resegmented(<<size::3, 0b11::2>>, rest) do
    bits = (size + 2) * 8
    <<value::size(bits), rest::binary>> = rest
    {value, rest}
  end

  #############################################################################
  ## postprocessing.

  defp post_process(module = %__MODULE__{}, _opts) do
    updated_module = %{module |
      exports: Enum.map(module.exports, &Function.post_process(&1, module, :exports)),
      imports: Enum.map(module.imports, &Function.post_process(&1, module, :imports)),
      locals: Enum.map(module.locals, &Function.post_process(&1, module, :locals)),
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
    {:literal, Enum.at(module.literals, index)}
  end

  defp reinterpret({:call_only, a, {:f, index}}, module = %{atoms: [m | _]}) do
    {f, ^a, ^index} = Enum.find(module.locals ++ module.exports, &(elem(&1, 2) == index))
    {:call_only, a, {m, f, a}}
  end

  defp reinterpret({:call_ext_only, a, index}, module) do
    {m, f, ^a} = Enum.at(module.imports, index)
    {:call_ext_only, a, {:extfunc, m, f, a}}
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
