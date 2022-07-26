defmodule Dmp.Diff do
  @moduledoc """
  ## DIFF FUNCTIONS

  A "difflist" is an Elixir list of "diff" tuples.
  Each tuple is an operation atom and a data string.
  The operations are:

  `:delete`, `:insert`, `:equal`

  Here is an example difflist:

  ```
  [{:delete, "Hello"}, {:insert, "Goodbye"}, {:equal, " world."}]
  ```

  which means: delete "Hello", add "Goodbye" and keep " world."
  """

  import Dmp.StringUtils

  alias Dmp.{Cursor, Options}

  @typedoc "Operations for a diff."
  @type op() :: :delete | :insert | :equal

  @typedoc "The diff tuple."
  @type t() :: {op(), String.t()}

  @typedoc "A list of diff operations."
  @type difflist() :: list(t())

  @type options() :: Options.t()

  @type expiry() :: :never | non_neg_integer()

  @typedoc """
  The result of a successful `half_match` call.
  A tuple of five strings:

    1. prefix of `text1`
    2. suffix of `text1`
    3. prefix of `text2`
    4. suffix of `text2`
    5. common middle
  """
  @type half_match_result() :: {String.t(), String.t(), String.t(), String.t(), String.t()}

  @doc """
  Find the differences between two texts.
  Most of the time `check_lines` is wanted, so default to true.

  `text1` - Old string to be diffed.
  `text2` - New string to be diffed.
  `check_lines` - Speedup flag.  If `false`, then don't run a
    line-level diff first to identify the changed areas.
    If `true`, then run a faster slightly less optimal diff.
  `opts` - A `DiffMatchPatch.Options` struct, or `nil` to use the
    defaults.

  Returns a difflist.
  """
  @spec main(String.t(), String.t(), boolean(), nil | options()) :: difflist()
  def main(text1, text2, check_lines \\ true, opts \\ nil) do
    opts = opts || Options.default()

    deadline =
      if opts.diff_timeout <= 0 do
        :never
      else
        deadline = :os.system_time(:millisecond) + round(opts.diff_timeout * 1000)
      end

    main_impl(text1, text2, check_lines, deadline)
  end

  @spec main_impl(String.t(), String.t(), boolean(), expiry()) :: difflist()
  defp main_impl(text1, text2, check_lines, deadline)
       when is_binary(text1) and is_binary(text2) do
    # Check for equality (speedup).
    if text1 == text2 do
      if text1 == "" do
        []
      else
        [{:equal, text1}]
      end
    else
      # Trim off common prefix (speedup).
      {prefix, text1, text2} = common_prefix(text1, text2)

      #  Trim off common suffix (speedup).
      {suffix, text1, text2} = common_suffix(text1, text2)

      # Compute the diff on the middle block.
      diffs = compute(text1, text2, check_lines, deadline)

      # Restore the prefix and suffix.
      diffs =
        if prefix != "" do
          [{:equal, prefix} | diffs]
        else
          diffs
        end

      diffs =
        if suffix != "" do
          diffs ++ [{:equal, suffix}]
        else
          diffs
        end

      diffs = cleanup_merge(diffs)

      diffs
    end
  end

  @doc """
  Find the differences between two texts.  Assumes that the texts do not
  have any common prefix or suffix.

  `text1` - Old string to be diffed.
  `text2` -  New string to be diffed.
  `check_lines` - Speedup flag.  If false, then don't run a
    line-level diff first to identify the changed areas.
    If true, then run a faster slightly less optimal diff.
  `deadline` -  Unix timestamp (in milliseconds) when the diff should be complete by.

  Returns a difflist.
  """
  @spec compute(String.t(), String.t(), boolean(), expiry()) :: difflist()
  def compute(text1, text2, check_lines, deadline) do
    text1_length = String.length(text1)
    text2_length = String.length(text2)

    cond do
      text1 == "" ->
        # Just add some text (speedup).

        [{:insert, text2}]

      text2 == "" ->
        # Just delete some text (speedup).

        [{:delete, text1}]

      true ->
        {longtext, shorttext, shorttext_length, op} =
          if text1_length > text2_length do
            {text1, text2, text2_length, :delete}
          else
            {text2, text1, text1_length, :insert}
          end

        case String.split(longtext, shorttext, parts: 2) do
          [left, right] ->
            # Shorter text is inside the longer text (speedup).
            [{op, left}, {:equal, shorttext}, {op, right}]

          _notfound ->
            if shorttext_length == 1 do
              # Single character string.
              # After the previous speedup, the character can't be an equality.

              [{:delete, text1}, {:insert, text2}]
            else
              # Check to see if the problem can be split in two.
              case half_match(text1, text2, deadline) do
                {text1_a, text1_b, text2_a, text2_b, mid_common} ->
                  # Send both pairs off for separate processing.

                  diffs_a = main_impl(text1_a, text2_a, check_lines, deadline)
                  diffs_b = main_impl(text1_b, text2_b, check_lines, deadline)
                  # Merge the results.
                  diffs_a ++ [{:equal, mid_common} | diffs_b]

                nil ->
                  if check_lines && text1_length > 100 && text2_length > 100 do
                    line_mode(text1, text2, deadline)
                  else
                    bisect(text1, text2, deadline)
                  end
              end
            end
        end
    end
  end

  @doc """
  Do a quick line-level diff on both strings, then rediff the parts for
  greater accuracy. This speedup can produce non-minimal diffs.

  `text1` - Old string to be diffed.
  `text2` - New string to be diffed.
  `deadline` -  Unix timestamp (in milliseconds) when the diff should be complete by.

  Returns a difflist.
  """
  @spec line_mode(String.t(), String.t(), expiry()) :: difflist()
  def line_mode(text1, text2, deadline) do
    # Scan the text on a line-by-line basis first.
    {text1, text2, line_array} = lines_to_chars(text1, text2)

    diffs =
      main_impl(text1, text2, false, deadline)
      # Convert the diff back to original text.
      |> chars_to_lines(line_array)
      # Eliminate freak matches (e.g. blank lines)
      |> cleanup_semantic()

    if diffs == [] do
      diffs
    else
      # Add a dummy entry at the end.
      # Rediff any replacement blocks, this time character-by-character.
      # Remove the dummy entry at the end.
      (diffs ++ [{:equal, ""}])
      |> Cursor.from_list(position: 0)
      |> line_mode_loop({0, 0, "", ""}, deadline)
      |> remove_dummy()
    end
  end

  @type line_mode_acc :: {integer(), integer(), String.t(), String.t()}

  # Verified tail-recursive
  @spec line_mode_loop(difflist(), line_mode_acc(), expiry()) :: difflist()
  defp line_mode_loop(
         %Cursor{current: nil} = diffs,
         _acc,
         _deadline
       ),
       do: Cursor.to_list(diffs)

  defp line_mode_loop(
         %Cursor{current: this_diff} = diffs,
         {count_delete, count_insert, text_delete, text_insert},
         deadline
       ) do
    {op, text} = this_diff

    {cursor, count_delete, count_insert, text_delete, text_insert} =
      case op do
        :insert ->
          {diffs, count_delete, count_insert + 1, text_delete, text_insert <> text}

        :delete ->
          {diffs, count_delete + 1, count_insert, text_delete <> text, text_insert}

        :equal ->
          # Upon reaching an equality, check for prior redundancies.
          diffs2 =
            if count_delete > 0 && count_insert > 0 do
              # Delete the offending records and add the merged ones.
              diffs1 = Cursor.delete_before(diffs, count_delete + count_insert)
              sub_diff = main_impl(text_delete, text_insert, false, deadline)
              Cursor.insert_before(diffs1, sub_diff)
            else
              diffs
            end

          {diffs2, 0, 0, "", ""}
      end

    line_mode_loop(
      Cursor.move_forward(cursor),
      {count_delete, count_insert, text_delete, text_insert},
      deadline
    )
  end

  @doc """
  Find the 'middle snake' of a diff, split the problem in two
  and return the recursively constructed diff.
  See Myers 1986 paper: An O(ND) Difference Algorithm and Its Variations.

  `text1` - Old string to be diffed.
  `text2` - New string to be diffed.
  `deadline` - Unix timestamp (in milliseconds) at which to bail if not yet complete.

  Returns a difflist.
  """
  @spec bisect(String.t(), String.t(), expiry()) :: difflist()
  def bisect(text1, text2, deadline) do
    # Cache the text lengths to prevent multiple calls.
    text1_length = String.length(text1)
    text2_length = String.length(text2)
    max_d = div(text1_length + text2_length + 1, 2)
    v_offset = max_d
    v_length = 2 * max_d

    v1init =
      Enum.reduce(0..(v_length - 1), [], fn i, acc ->
        val =
          if i == v_offset + 1 do
            0
          else
            -1
          end

        [{i, val} | acc]
      end)

    v2init = Map.new(v1init)
    v1init = Map.new(v1init)

    # Offsets for start and end of k loop.
    # Prevents mapping of space beyond the grid.
    # k1start = 0
    # k1end = 0
    # k2start = 0
    # k2end = 0
    diffs =
      Enum.reduce_while(0..max_d, {v1init, v2init, 0, 0, 0, 0}, fn d,
                                                                   {v1, v2, k1start, k1end,
                                                                    k2start, k2end} ->
        if is_integer(deadline) && :os.system_time(:millisecond) > deadline do
          # Bail out if deadline is reached.

          {:halt, nil}
        else
          # Walk the front path one step.
          {v1, diffs1} =
            advance_front(
              d,
              v_offset,
              v_length,
              -d + k1start,
              k1start,
              k1end,
              v1,
              v2,
              text1,
              text1_length,
              text2,
              text2_length,
              deadline
            )

          if !is_nil(diffs1) do
            {:halt, diffs1}
          else
            # Walk the reverse path one step.

            {v2, diffs2} =
              advance_reverse(
                d,
                v_offset,
                v_length,
                -d + k2start,
                k2start,
                k2end,
                v1,
                v2,
                text1,
                text1_length,
                text2,
                text2_length,
                deadline
              )

            if !is_nil(diffs2) do
              {:halt, diffs2}
            else
              {:cont, {v1, v2, k1start, k1end, k2start, k2end}}
            end
          end
        end
      end)

    if !is_nil(diffs) do
      diffs
    else
      # Diff took too long and hit the deadline or
      # number of diffs equals number of characters, no commonality at all.
      [{:delete, text1}, {:insert, text2}]
    end
  end

  defp advance_front(
         d,
         v_offset,
         v_length,
         k1,
         k1start,
         k1end,
         v1,
         v2,
         text1,
         text1_length,
         text2,
         text2_length,
         deadline
       ) do
    if k1 <= d - k1end do
      k1_offset = v_offset + k1
      v1_minus1 = v1[k1_offset - 1]
      v1_plus1 = v1[k1_offset + 1]

      x1 =
        if k1 == -d || (k1 != d && v1_minus1 < v1_plus1) do
          v1_plus1
        else
          v1_minus1 + 1
        end

      y1 = x1 - k1
      {x1, y1} = advance1(x1, y1, text1, text1_length, text2, text2_length)
      v1 = Map.put(v1, k1_offset, x1)

      delta = text1_length - text2_length

      # If the total number of characters is odd, then the front path will
      # collide with the reverse path.
      front = rem(delta, 2) != 0

      {k1start, k1end, diffs} =
        cond do
          x1 > text1_length ->
            # Ran off the right of the graph.
            {k1start, k1end + 2, nil}

          y1 > text2_length ->
            # Ran off the bottom of the graph.
            {k1start + 2, k1end, nil}

          front ->
            k2_offset = v_offset + delta - k1

            if k2_offset >= 0 && k2_offset < v_length && v2[k2_offset] != -1 do
              # Mirror x2 onto top-left coordinate system.
              x2 = text1_length - v2[k2_offset]

              if x1 >= x2 do
                # Overlap detected.

                {k1start, k1end, bisect_split(text1, text2, x1, y1, deadline)}
              else
                {k1start, k1end, nil}
              end
            else
              {k1start, k1end, nil}
            end

          true ->
            {k1start, k1end, nil}
        end

      if !is_nil(diffs) do
        {v1, diffs}
      else
        advance_front(
          d,
          v_offset,
          v_length,
          k1 + 2,
          k1start,
          k1end,
          v1,
          v2,
          text1,
          text1_length,
          text2,
          text2_length,
          deadline
        )
      end
    else
      {v1, nil}
    end
  end

  @spec advance1(
          non_neg_integer(),
          non_neg_integer(),
          String.t(),
          non_neg_integer(),
          String.t(),
          non_neg_integer()
        ) :: {non_neg_integer(), non_neg_integer()}
  defp advance1(x1, y1, text1, text1_length, text2, text2_length) do
    if x1 < text1_length && y1 < text2_length &&
         String.at(text1, x1) == String.at(text2, y1) do
      advance1(x1 + 1, y1 + 1, text1, text1_length, text2, text2_length)
    else
      {x1, y1}
    end
  end

  defp advance_reverse(
         d,
         v_offset,
         v_length,
         k2,
         k2start,
         k2end,
         v1,
         v2,
         text1,
         text1_length,
         text2,
         text2_length,
         deadline
       ) do
    if k2 <= d - k2end do
      k2_offset = v_offset + k2
      v2_minus1 = v2[k2_offset - 1]
      v2_plus1 = v2[k2_offset + 1]

      x2 =
        if k2 == -d || (k2 != d && v2_minus1 < v2_plus1) do
          v2_plus1
        else
          v2_minus1 + 1
        end

      y2 = x2 - k2
      {x2, y2} = advance2(x2, y2, text1, text1_length, text2, text2_length)
      v2 = Map.put(v2, k2_offset, x2)

      delta = text1_length - text2_length

      # If the total number of characters is odd, then the front path will
      # collide with the reverse path.
      front = rem(delta, 2) != 0

      {k2start, k2end, diffs} =
        cond do
          x2 > text1_length ->
            # Ran off the right of the graph.
            {k2start, k2end + 2, nil}

          y2 > text2_length ->
            # Ran off the bottom of the graph.
            {k2start + 2, k2end, nil}

          !front ->
            k1_offset = v_offset + delta - k2

            if k1_offset >= 0 && k1_offset < v_length && v1[k1_offset] != -1 do
              x1 = v1[k1_offset]
              y1 = v_offset + x1 - k1_offset
              # Mirror x2 onto top-left coordinate system.
              x2 = text1_length - x2

              if x1 >= x2 do
                # Overlap detected.

                {k2start, k2end, bisect_split(text1, text2, x1, y1, deadline)}
              else
                {k2start, k2end, nil}
              end
            else
              {k2start, k2end, nil}
            end

          true ->
            {k2start, k2end, nil}
        end

      if !is_nil(diffs) do
        {v2, diffs}
      else
        advance_reverse(
          d,
          v_offset,
          v_length,
          k2 + 2,
          k2start,
          k2end,
          v1,
          v2,
          text1,
          text1_length,
          text2,
          text2_length,
          deadline
        )
      end
    else
      {v2, nil}
    end
  end

  @spec advance2(
          non_neg_integer(),
          non_neg_integer(),
          String.t(),
          non_neg_integer(),
          String.t(),
          non_neg_integer()
        ) :: {non_neg_integer(), non_neg_integer()}
  defp advance2(x2, y2, text1, text1_length, text2, text2_length) do
    if x2 < text1_length && y2 < text2_length &&
         String.at(text1, text1_length - x2 - 1) == String.at(text2, text2_length - y2 - 1) do
      advance2(x2 + 1, y2 + 1, text1, text1_length, text2, text2_length)
    else
      {x2, y2}
    end
  end

  # Given the location of the 'middle snake', split the diff in two parts
  # and recurse.
  #
  # `text1` - Old string to be diffed.
  # `text2` - New string to be diffed.
  # `x` - Index of split point in text1.
  # `y` - Index of split point in text2.
  # `deadline` - Unix timestamp (in milliseconds) at which to bail if not yet complete.
  #
  # Returns a difflist.
  @spec bisect_split(
          String.t(),
          String.t(),
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer()
        ) :: difflist()
  defp bisect_split(text1, text2, x, y, deadline) do
    {text1a, text1b} = String.split_at(text1, x)
    {text2a, text2b} = String.split_at(text2, y)

    # Compute both diffs serially.
    diffsa = main_impl(text1a, text2a, false, deadline)
    diffsb = main_impl(text1b, text2b, false, deadline)

    diffsa ++ diffsb
  end

  @doc """
  Split two texts into a list of strings.  Reduce the texts to a string of
  hashes where each Unicode character represents one line.

  `text1` - First string.
  `text2` - Second string.

  Returns a tuple containing the encoded `text1`, the encoded `text2` and
    the list of unique strings.  The zeroth element of the list of
    unique strings is intentionally blank.
  """
  @spec lines_to_chars(String.t(), String.t()) :: {String.t(), String.t(), list(String.t())}
  def lines_to_chars(text1, text2) do
    # e.g. Enum.at(line_array, 4) == "Hello\n"
    # e.g. Map.get(line_hash, "Hello\n") == 4

    # "\x00" is a valid character, but various debuggers don't like it.
    # So we'll insert a junk entry in line_array to avoid generating a null character.
    # Bail out at 55_295 because to_string([55_296]) raises "invalid code point 55296"
    # Allocate 2/3rds of the space for text1, the rest for text2.
    {line_hash, line_array, chars1} = lines_to_chars_munge(text1, {%{}, [""], nil, nil}, 37_000)

    {_line_hash, line_array, chars2} =
      lines_to_chars_munge(text2, {line_hash, line_array, nil, nil}, 55_295)

    {chars1, chars2, line_array}
  end

  defp pop_line(text) do
    case String.split(text, "\n", parts: 2) do
      [line] ->
        {line, ""}

      [line, rest] ->
        {line <> "\n", rest}

      _ ->
        raise "pop_line error"
    end
  end

  @type munge_line_hash() :: %{String.t() => non_neg_integer()}
  @type munge_line_acc() ::
          {munge_line_hash(), list(String.t()), nil | list(non_neg_integer()),
           nil | non_neg_integer()}
  @type munge_line_result() :: {munge_line_hash(), list(String.t()), String.t()}

  @spec lines_to_chars_munge(
          String.t(),
          munge_line_acc(),
          non_neg_integer()
        ) :: munge_line_result()
  # Verified tail-recursive
  # Case 1. Initial text is ""
  def lines_to_chars_munge("", {h, arr, nil, _}, _max_lines) do
    {h, arr, ""}
  end

  # Case 2. No more text, or overflowed max_lines
  def lines_to_chars_munge("", {h, arr, chars, _}, _max_lines) do
    {h, Enum.reverse(arr), Enum.reverse(chars) |> List.to_string()}
  end

  # Case 3. Process one line of text and recurse
  def lines_to_chars_munge(text, {h, arr, chars, next_val}, max_lines) do
    # First time if chars == nil
    {arr, chars, next_val} =
      if is_nil(chars) do
        {Enum.reverse(arr), [], Enum.count(arr)}
      else
        {arr, chars, next_val}
      end

    {line, rest} = pop_line(text)

    {rest, {h, arr, chars, next_val}} =
      cond do
        Map.has_key?(h, line) ->
          val = Map.get(h, line)
          {rest, {h, arr, [val | chars], next_val}}

        next_val < max_lines ->
          {rest, {Map.put(h, line, next_val), [line | arr], [next_val | chars], next_val + 1}}

        true ->
          # Bail out
          {"", {h, arr, chars, next_val}}
      end

    lines_to_chars_munge(rest, {h, arr, chars, next_val}, max_lines)
  end

  @doc """
  Rehydrate the text in a diff from a string of line hashes to real lines of
  text.

  `diffs` - a difflist.
  `line_array` - list of unique strings.

  Returns the rehydrated difflist.
  """
  @spec chars_to_lines(difflist(), list(String.t())) :: difflist()
  def chars_to_lines(diffs, line_array) do
    Enum.map(diffs, fn {op, encoded_text} ->
      {op,
       encoded_text
       |> String.to_charlist()
       |> Enum.reduce("", fn i, text -> text <> Enum.at(line_array, i) end)}
    end)
  end

  @doc """
  Determine the common prefix of two strings.

  `text1` - First string.
  `text2` - Second string.

  Returns a tuple `{prefix, rest1, rest2}`, where

  `prefix` - The common prefix.
  `rest1` - `text1` with the prefix removed.
  `rest2` - `text2` with the prefix removed.
  """
  @spec common_prefix(String.t(), String.t()) :: {String.t(), String.t(), String.t()}
  def common_prefix(text1, text2) do
    # Cache the text lengths to prevent multiple calls.
    text1_length = String.length(text1)
    text2_length = String.length(text2)
    n = min(text1_length, text2_length)

    if n == 0 do
      {"", text1, text2}
    else
      prefix =
        Enum.reduce_while(0..(n - 1), "", fn i, acc ->
          ch = String.at(text1, i)

          if ch == String.at(text2, i) do
            {:cont, acc <> ch}
          else
            {:halt, acc}
          end
        end)

      {prefix, String.replace_prefix(text1, prefix, ""), String.replace_prefix(text2, prefix, "")}
    end
  end

  @doc """
  Determine the common suffix of two strings.

  `text1` - First string.
  `text2` - Second string.

  Returns a tuple `{suffix, rest1, rest2}`, where

  `suffix` - The common suffix.
  `rest1` - `text1` with the suffix removed.
  `rest2` - `text2` with the suffix removed.
  """
  @spec common_suffix(String.t(), String.t()) :: {String.t(), String.t(), String.t()}
  def common_suffix(text1, text2) do
    # Cache the text lengths to prevent multiple calls.
    text1_length = String.length(text1)
    text2_length = String.length(text2)
    n = min(text1_length, text2_length)

    if n == 0 do
      {"", text1, text2}
    else
      suffix =
        Enum.reduce_while(1..n, "", fn i, acc ->
          ch = String.at(text1, text1_length - i)

          if ch == String.at(text2, text2_length - i) do
            {:cont, ch <> acc}
          else
            {:halt, acc}
          end
        end)

      {suffix, String.replace_suffix(text1, suffix, ""), String.replace_suffix(text2, suffix, "")}
    end
  end

  @doc """
  Determine if the suffix of one string is the prefix of another.

  `text1` - First string.
  `text2` - Second string.

  Returns the number of characters common to the end of the first
  string and the start of the second string.
  """
  @spec common_overlap(String.t(), String.t()) :: non_neg_integer()
  def common_overlap(text1, text2) do
    # Cache the text lengths to prevent multiple calls.
    text1_length = String.length(text1)
    text2_length = String.length(text2)
    # Eliminate the null case.
    if text1_length == 0 || text2_length == 0 do
      0
    else
      # Truncate the longer string.
      {text1, text2} =
        cond do
          text1_length > text2_length ->
            {substring(text1, text1_length - text2_length), text2}

          text1_length < text2_length ->
            {text1, substring(text2, 0, text1_length)}

          true ->
            {text1, text2}
        end

      text_length = min(text1_length, text2_length)
      # Quick check for the worst case.
      if text1 == text2 do
        text_length
      else
        best_overlap(0, 1, text_length, text1, text2)
      end
    end
  end

  # Start by looking for a single character match
  # and increase length until no match is found.
  # Performance analysis: https://neil.fraser.name/news/2010/11/04/
  @spec best_overlap(
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer(),
          String.t(),
          String.t()
        ) :: non_neg_integer()
  defp best_overlap(best, length, text_length, text1, text2) do
    pattern = substring(text1, text_length - length)

    case String.split(text2, pattern, parts: 2) do
      [left, _right] ->
        found = String.length(left)
        length = length + found

        {best, length} =
          if found == 0 || pattern == left do
            {length, length + 1}
          else
            {best, length}
          end

        best_overlap(best, length, text_length, text1, text2)

      _not_found ->
        best
    end
  end

  @doc """
  Do the two texts share a substring which is at least half the length of
  the longer text? This speedup can produce non-minimal diffs.

  `text1` - First string.
  `text2` - Second string.
  `deadline` - Unix timestamp (in milliseconds) at which to bail if not yet complete.

  Returns a `half_match_result` 5-tuple, or `nil` if there was no match.
  Returns `nil` if `deadline` is zero (no time limit specified).
  """
  @spec half_match(String.t(), String.t(), non_neg_integer()) ::
          nil | half_match_result()
  def half_match(text1, text2, deadline) do
    if deadline == 0 do
      # Don't risk returning a non-optimal diff if we have unlimited time.
      nil
    else
      # Cache the text lengths to prevent multiple calls.
      text1_length = String.length(text1)
      text2_length = String.length(text2)

      {longtext, shorttext, longtext_length, shorttext_length} =
        if text1_length > text2_length do
          {text1, text2, text1_length, text2_length}
        else
          {text2, text1, text2_length, text1_length}
        end

      if longtext_length < 4 || shorttext_length * 2 < longtext_length do
        # Pointless.
        nil
      else
        # First check if the second quarter is the seed for a half-match.
        i = div(longtext_length + 3, 4)
        hm1 = half_match_impl(longtext, shorttext, i)

        # Check again based on the third quarter.
        i = div(longtext_length + 1, 2)
        hm2 = half_match_impl(longtext, shorttext, i)

        hm =
          case {hm1, hm2} do
            {nil, nil} ->
              nil

            {_hm1, nil} ->
              hm1

            {nil, _hm2} ->
              hm2

            {{_, _, _, _, common1}, {_, _, _, _, common2}} ->
              # Both matched.  Select the longest.
              if String.length(common1) > String.length(common2) do
                hm1
              else
                hm2
              end
          end

        case hm do
          {prefix1, suffix1, prefix2, suffix2, common} ->
            # A half-match was found, sort out the return data.
            if text1_length > text2_length do
              hm
            else
              {prefix2, suffix2, prefix1, suffix1, common}
            end

          _ ->
            nil
        end
      end
    end
  end

  @spec half_match_impl(String.t(), String.t(), non_neg_integer()) ::
          nil | {String.t(), String.t(), String.t(), String.t(), String.t()}
  defp half_match_impl(longtext, shorttext, i) do
    seed_length = div(String.length(longtext), 4)
    seed = String.slice(longtext, i, seed_length)
    j = index_of(shorttext, seed)
    best_half_match_loop({"", "", "", "", ""}, seed, i, j, longtext, shorttext)
  end

  # Verified tail-recursive
  defp best_half_match_loop(
         {best_longtext_a, best_longtext_b, best_shorttext_a, best_shorttext_b, best_common},
         _seed,
         _i,
         -1,
         longtext,
         _shorttext
       ) do
    if String.length(best_common) * 2 >= String.length(longtext) do
      {best_longtext_a, best_longtext_b, best_shorttext_a, best_shorttext_b, best_common}
    else
      nil
    end
  end

  defp best_half_match_loop(
         {best_longtext_a, best_longtext_b, best_shorttext_a, best_shorttext_b, best_common},
         seed,
         i,
         j,
         longtext,
         shorttext
       ) do
    {longa, longb} = String.split_at(longtext, i)
    {shorta, shortb} = String.split_at(shorttext, j)
    {prefix, ptext1, ptext2} = common_prefix(longb, shortb)
    {suffix, stext1, stext2} = common_suffix(longa, shorta)
    common = suffix <> prefix

    {best_longtext_a, best_longtext_b, best_shorttext_a, best_shorttext_b, best_common} =
      if String.length(best_common) < String.length(common) do
        {stext1, ptext1, stext2, ptext2, common}
      else
        {best_longtext_a, best_longtext_b, best_shorttext_a, best_shorttext_b, best_common}
      end

    j = index_of(shorttext, seed, j + 1)

    best_half_match_loop(
      {best_longtext_a, best_longtext_b, best_shorttext_a, best_shorttext_b, best_common},
      seed,
      i,
      j,
      longtext,
      shorttext
    )
  end

  @typedoc "Double-ended queue of equalities."
  @type diffqueue() :: :queue.queue()

  defp safe_drop_r(queue, n \\ 1)

  defp safe_drop_r(queue, 1) do
    if :queue.is_empty(queue) do
      queue
    else
      :queue.drop_r(queue)
    end
  end

  defp safe_drop_r(queue, n) when n > 1 do
    safe_drop_r(queue) |> safe_drop_r(n - 1)
  end

  defp safe_drop_r(queue, _), do: queue

  @type semantic_acc() :: {
          boolean(),
          diffqueue(),
          nil | String.t(),
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer()
        }

  @doc """
  Reduce the number of edits by eliminating semantically trivial equalities.

  Returns the updated difflist.
  """
  @spec cleanup_semantic(difflist()) :: difflist()
  def cleanup_semantic([]), do: []

  def cleanup_semantic(diffs) do
    diffs =
      diffs
      |> Cursor.from_list(position: 0)

    {diffs, changes} =
      replace_small_equalities_loop(diffs, {false, :queue.new(), nil, 0, 0, 0, 0})

    diffs =
      if changes do
        # Normalize the diff
        diffs |> cleanup_merge()
      else
        diffs
      end

    if Enum.count(diffs) < 2 do
      diffs
    else
      diffs
      |> cleanup_semantic_lossless()
      |> Cursor.from_list(position: 1)
      |> cleanup_overlap_loop()
    end
  end

  @spec replace_small_equalities_loop(
          Cursor.t(),
          semantic_acc()
        ) :: {difflist(), boolean()}
  # `equalities` - Double-ended queue of equalities..
  # `last_equality` - Always equal to the text of `:queue.peek_r(equalities)`.
  # Verified tail-recursive
  defp replace_small_equalities_loop(
         %Cursor{current: nil} = diffs,
         {changes, _equalities, _last_equality, _length_insertions1, _length_deletions1,
          _length_insertions2, _length_deletions2}
       ) do
    diffs = Cursor.to_list(diffs)
    {diffs, changes}
  end

  defp replace_small_equalities_loop(
         %Cursor{current: this_diff} = diffs,
         {changes, equalities, last_equality, length_insertions1, length_deletions1,
          length_insertions2, length_deletions2}
       ) do
    {op, text} = this_diff

    {cursor, changes, equalities, last_equality, length_insertions1, length_deletions1,
     length_insertions2,
     length_deletions2} =
      if op == :equal do
        # Equality found. Insert at rear of queue.
        equalities = :queue.in(this_diff, equalities)

        {Cursor.move_forward(diffs), changes, equalities, text, length_insertions2,
         length_deletions2, 0, 0}
      else
        # An insertion or deletion.
        {length_insertions2, length_deletions2} =
          if op == :insert do
            {length_insertions2 + String.length(text), length_deletions2}
          else
            {length_insertions2, length_deletions2 + String.length(text)}
          end

        # Eliminate an equality that is smaller or equal to the edits on both
        # sides of it.
        if !is_nil(last_equality) &&
             String.length(last_equality) <= max(length_insertions1, length_deletions1) &&
             String.length(last_equality) <= max(length_insertions2, length_deletions2) do
          # Walk back to offending equality.
          eq = :queue.get_r(equalities)
          diffs = Cursor.find_back!(diffs, eq)

          # Replace equality with a delete.
          # Insert a corresponding an insert.
          new_diffs = [{:delete, last_equality}, {:insert, last_equality}]

          diffs =
            diffs
            |> Cursor.delete(1)
            |> Cursor.insert(new_diffs)

          # Throw away the equality we just deleted.
          # Throw away the previous equality (it needs to be reevaluated).
          equalities = safe_drop_r(equalities, 2)

          if :queue.is_empty(equalities) do
            # There are no previous equalities, walk back to the start.
            # Reset the counters

            {Cursor.move_first(diffs), true, equalities, nil, 0, 0, 0, 0}
          else
            eq = :queue.get_r(equalities)
            # There is a safe equality we can fall back to.
            # Reset the counters
            diffs = Cursor.find_back!(diffs, eq)

            {diffs, true, equalities, nil, 0, 0, 0, 0}
          end
        else
          {Cursor.move_forward(diffs), changes, equalities, last_equality, length_insertions1,
           length_deletions1, length_insertions2, length_deletions2}
        end
      end

    replace_small_equalities_loop(
      cursor,
      {changes, equalities, last_equality, length_insertions1, length_deletions1,
       length_insertions2, length_deletions2}
    )
  end

  # Find any overlaps between deletions and insertions.
  # e.g: <del>abcxxx</del><ins>xxxdef</ins>
  #   -> <del>abc</del>xxx<ins>def</ins>
  # e.g: <del>xxxabc</del><ins>defxxx</ins>
  #   -> <ins>def</ins>xxx<del>abc</del>
  # Only extract an overlap if it is as big as the edit ahead or behind it.
  # Verified tail-recursive
  @spec cleanup_overlap_loop(Cursor.t()) :: difflist()
  defp cleanup_overlap_loop(%Cursor{current: nil} = diffs) do
    diffs = Cursor.to_list(diffs)

    diffs
  end

  defp cleanup_overlap_loop(%Cursor{} = diffs) do
    {prev_diff, this_diff, _} = Cursor.get(diffs)
    {prev_op, deletion} = prev_diff
    {op, insertion} = this_diff

    diffs =
      if prev_op == :delete && op == :insert do
        overlap_length1 = common_overlap(deletion, insertion)
        overlap_length2 = common_overlap(insertion, deletion)

        if overlap_length1 >= overlap_length2 do
          if overlap_length1 * 2 >= String.length(deletion) ||
               overlap_length1 * 2 >= String.length(insertion) do
            # Overlap found. Insert an equality and trim the surrounding edits.
            overlap = substring(insertion, 0, overlap_length1)
            deletion = substring(deletion, 0, String.length(deletion) - overlap_length1)
            insertion = substring(insertion, overlap_length1)

            new_diffs = [{:delete, deletion}, {:equal, overlap}, {:insert, insertion}]

            diffs
            |> Cursor.move_back()
            |> Cursor.delete(2)
            |> Cursor.insert(new_diffs)
          else
            diffs
          end
        else
          if overlap_length2 * 2 >= String.length(deletion) ||
               overlap_length2 * 2 >= String.length(insertion) do
            # Reverse overlap found.
            # Insert an equality and swap and trim the surrounding edits.
            overlap = substring(deletion, 0, overlap_length2)
            insertion = substring(insertion, 0, String.length(insertion) - overlap_length2)
            deletion = substring(deletion, overlap_length2)

            new_diffs = [{:insert, insertion}, {:equal, overlap}, {:delete, deletion}]

            diffs
            |> Cursor.move_back(1)
            |> Cursor.delete(2)
            |> Cursor.insert(new_diffs)
          else
            diffs
          end
        end
      else
        diffs
      end

    diffs
    |> Cursor.move_forward()
    |> cleanup_overlap_loop()
  end

  @doc """
  Look for single edits surrounded on both sides by equalities
  which can be shifted sideways to align the edit to a word boundary.
  e.g: `The c<ins>at c</ins>ame.` -> `The <ins>cat </ins>came.`

  Returns the updated difflist.
  """
  @spec cleanup_semantic_lossless(difflist()) :: difflist()
  def cleanup_semantic_lossless(diffs) do
    # Intentionally ignore the first and last element (don't need checking).
    diffs
    |> Cursor.from_list(position: 1)
    |> cleanup_semantic_lossless_loop()
  end

  @type score_loop_acc() :: {non_neg_integer(), String.t(), String.t(), String.t()}

  @spec cleanup_semantic_lossless_loop(Cursor.t()) :: difflist()
  # Verified tail-recursive
  # Intentionally ignore the first and last element (don't need checking).
  defp cleanup_semantic_lossless_loop(%Cursor{next: []} = diffs) do
    Cursor.to_list(diffs)
  end

  defp cleanup_semantic_lossless_loop(%Cursor{} = diffs) do
    {prev_diff, this_diff, next_diff} = Cursor.get(diffs)
    {prev_op, prev_text} = prev_diff
    {next_op, next_text} = next_diff

    diffs =
      if prev_op == :equal && next_op == :equal do
        # This is a single edit surrounded by equalities.
        equality1 = prev_text
        {op, edit} = this_diff
        equality2 = next_text

        # First, shift the edit as far left as possible.
        {suffix, text1, text2} = common_suffix(equality1, edit)

        {equality1, edit, equality2} = {text1, suffix <> text2, suffix <> equality2}

        score1 = cleanup_semantic_score(equality1, edit)
        score2 = cleanup_semantic_score(edit, equality2)
        score = score1 + score2

        {_best_score, best_equality1, best_edit, best_equality2} =
          best_score_loop(
            equality1,
            edit,
            equality2,
            {score, equality1, edit, equality2}
          )

        if prev_text != best_equality1 do
          # We have an improvement, save it back to the diff.
          diffs =
            if best_equality1 != "" do
              new_prev = {prev_op, best_equality1}
              # Update prev_diff.
              diffs
              |> Cursor.delete_before(1)
              |> Cursor.insert_before([new_prev])
            else
              # Delete prev_diff.
              Cursor.delete_before(diffs, 1)
            end

          new_cur = {op, best_edit}
          # Update this_diff.
          diffs =
            diffs
            |> Cursor.delete(1)
            |> Cursor.insert_before([new_cur])

          # Now pointing at next_diff
          diffs =
            if best_equality2 != "" do
              new_next = {next_op, best_equality2}
              # Update next_diff
              diffs
              |> Cursor.delete(1)
              |> Cursor.insert_before([new_next])
              |> Cursor.move_back(2)
            else
              # Delete next_diff
              diffs
              |> Cursor.delete(1)
              |> Cursor.move_back(1)
            end

          # Now back to pointing at this_diff
          diffs
        else
          diffs
        end
      else
        diffs
      end

    diffs
    |> Cursor.move_forward()
    |> cleanup_semantic_lossless_loop()
  end

  @spec best_score_loop(
          String.t(),
          String.t(),
          String.t(),
          score_loop_acc()
        ) :: score_loop_acc()
  # Verified tail-recursive
  defp best_score_loop(
         equality1,
         edit,
         equality2,
         {best_score, _best_equality1, _best_edit, _best_equality2} = acc
       ) do
    if edit == "" || equality2 == "" do
      acc
    else
      # Second, step character by character right, looking for the best fit.
      {edit_first, edit} = String.split_at(edit, 1)
      {equality2_first, equality2} = String.split_at(equality2, 1)

      if edit_first != equality2_first do
        acc
      else
        equality1 = equality1 <> edit_first
        edit = edit <> equality2_first
        score1 = cleanup_semantic_score(equality1, edit)
        score2 = cleanup_semantic_score(edit, equality2)
        score = score1 + score2
        # The >= encourages trailing rather than leading whitespace on edits.
        if score >= best_score do
          best_score_loop(equality1, edit, equality2, {score, equality1, edit, equality2})
        else
          best_score_loop(
            equality1,
            edit,
            equality2,
            acc
          )
        end
      end
    end
  end

  # Define some regex patterns for matching boundaries.
  @alphanumeric ~r/^[0-9A-Za-z]+$/
  @whitespace ~r/^[\s]+$/
  @line_break ~r/^[\r\n]+$/
  @blank_line_end ~r/\n\r?\n\Z/
  @blank_line_start ~r/\A\r?\n\r?\n/

  # Given two strings, compute a score representing whether the internal
  # boundary falls on logical boundaries.
  # Scores range from 6 (best) to 0 (worst).
  # `one` - First string.
  # `two` - Second string.
  # Returns the score.
  @spec cleanup_semantic_score(String.t(), String.t()) :: non_neg_integer()

  # Edges are the best.
  def cleanup_semantic_score("", _two), do: 6
  def cleanup_semantic_score(_one, ""), do: 6

  def cleanup_semantic_score(one, two) do
    char1 = String.last(one)
    char2 = String.first(two)

    non_alphanumeric1 = !Regex.match?(@alphanumeric, char1)
    non_alphanumeric2 = !Regex.match?(@alphanumeric, char2)
    whitespace1 = Regex.match?(@whitespace, char1)
    whitespace2 = Regex.match?(@whitespace, char2)
    line_break1 = Regex.match?(@line_break, char1)
    line_break2 = Regex.match?(@line_break, char2)
    blank_line1 = line_break1 && Regex.match?(@blank_line_end, one)
    blank_line2 = line_break2 && Regex.match?(@blank_line_start, two)

    cond do
      blank_line1 || blank_line2 ->
        # Five points for blank lines.
        5

      line_break1 || line_break2 ->
        # Four points for line breaks.
        4

      non_alphanumeric1 && !whitespace1 && whitespace2 ->
        # Three points for end of sentences.
        3

      whitespace1 || whitespace2 ->
        # Two points for whitespace.
        2

      non_alphanumeric1 || non_alphanumeric2 ->
        # One point for non-alphanumeric.
        1

      true ->
        0
    end
  end

  @type efficiency_acc() :: {
          boolean(),
          diffqueue(),
          nil | String.t(),
          t(),
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer()
        }

  @doc """
  Reduce the number of edits by eliminating operationally trivial equalities.

  `diff_edit_cost`  Cost of an empty edit operation in terms of edit characters.

  Returns the updated difflist.
  """
  @spec cleanup_efficiency(difflist(), non_neg_integer()) :: difflist()
  def cleanup_efficiency([], _diff_edit_cost), do: []

  def cleanup_efficiency(diffs, diff_edit_cost) do
    first_diff = List.first(diffs)

    {diffs, changes} =
      diffs
      |> Cursor.from_list(position: 0)
      |> cleanup_efficiency_loop(
        {false, :queue.new(), nil, first_diff, 0, 0, 0, 0},
        diff_edit_cost
      )

    if changes do
      cleanup_merge(diffs)
    else
      diffs
    end
  end

  # `equalities` - Double-ended queue of equalities.
  # `last_equality` - Always equal to the text of `equalities.get_r()`
  # `safe_diff` - The last Diff that is known to be unsplittable.
  # `pre_ins` - 1 if there is an insertion operation before the last equality.
  # `pre_del` - 1 if there is a deletion operation before the last equality.
  # `post_ins` - 1 if there is an insertion operation after the last equality.
  # `post_del` - 1 if there is a deletion operation after the last equality.
  # `diff_edit_cost` - Cost of an empty edit operation in terms of edit characters.
  # Verified tail-recursive
  @spec cleanup_efficiency_loop(Cursor.t(), efficiency_acc(), non_neg_integer()) ::
          {difflist(), boolean()}
  def cleanup_efficiency_loop(
        %Cursor{current: nil} = diffs,
        {changes, _equalities, _last_equality, _safe_diff, _pre_ins, _pre_del, _post_ins,
         _post_del},
        _diff_edit_cost
      ),
      do: {Cursor.to_list(diffs), changes}

  def cleanup_efficiency_loop(
        %Cursor{current: this_diff} = diffs,
        {changes, equalities, last_equality, safe_diff, pre_ins, pre_del, post_ins, post_del},
        diff_edit_cost
      ) do
    {op, text} = this_diff

    {diffs, acc} =
      if op == :equal do
        # Equality found.

        {equalities, last_equality, safe_diff, pre_ins, pre_del} =
          if String.length(text) < diff_edit_cost && (post_ins != 0 || post_del != 0) do
            # Candidate found. Insert at rear of queue.
            equalities = :queue.in(this_diff, equalities)
            {equalities, text, safe_diff, post_ins, post_del}
          else
            # Not a candidate, and can never become one.
            {:queue.new(), nil, this_diff, pre_ins, pre_del}
          end

        {Cursor.move_forward(diffs),
         {changes, equalities, last_equality, safe_diff, pre_ins, pre_del, 0, 0}}
      else
        # An insertion or deletion.
        {post_ins, post_del} =
          if op == :delete do
            {post_ins, 1}
          else
            {1, post_del}
          end

        # Five types to be split:
        # <ins>A</ins><del>B</del>XY<ins>C</ins><del>D</del>
        # <ins>A</ins>X<ins>C</ins><del>D</del>
        # <ins>A</ins><del>B</del>X<ins>C</ins>
        # <ins>A</del>X<ins>C</ins><del>D</del>
        # <ins>A</ins><del>B</del>X<del>C</del>
        ins_del_count = pre_ins + pre_del + post_ins + post_del

        if !is_nil(last_equality) &&
             (ins_del_count == 4 ||
                (String.length(last_equality) * 2 < diff_edit_cost && ins_del_count == 3)) do
          # Walk back to offending equality.
          # Replace equality with a delete.
          # Insert a corresponding an insert.

          eq = :queue.get_r(equalities)

          diffs =
            diffs
            |> Cursor.find_back!(eq)
            |> Cursor.delete(1)
            |> Cursor.insert_before([{:delete, last_equality}, {:insert, last_equality}])

          # Throw away the equality we just deleted.
          equalities = safe_drop_r(equalities)

          {diffs, equalities, post_ins, post_del} =
            if pre_ins != 0 && pre_del != 0 do
              # No changes made which could affect previous entry, keep going.
              {Cursor.move_forward(diffs), :queue.new(), 1, 1}
            else
              # Throw away the previous equality (it needs to be reevaluated).
              equalities = safe_drop_r(equalities)

              next_diff =
                case :queue.peek_r(equalities) do
                  {:value, eq} ->
                    # There is an equality we can fall back to.
                    eq

                  :empty ->
                    # There are no previous questionable equalities,
                    # walk back to the last known safe diff.
                    safe_diff
                end

              diffs = Cursor.find_back!(diffs, next_diff)

              {diffs, equalities, 0, 0}
            end

          {diffs, {true, equalities, nil, safe_diff, pre_ins, pre_del, post_ins, post_del}}
        else
          {Cursor.move_forward(diffs),
           {changes, equalities, last_equality, safe_diff, pre_ins, pre_del, post_ins, post_del}}
        end
      end

    diffs
    |> cleanup_efficiency_loop(
      acc,
      diff_edit_cost
    )
  end

  @doc """
  Reorder and merge like edit sections.  Merge equalities.
  Any edit section can move as long as it doesn't cross an equality.

  Returns the updated difflist.
  """
  @spec cleanup_merge(difflist()) :: difflist()
  def cleanup_merge([]), do: []

  def cleanup_merge(diffs) do
    {diffs, changes} =
      diffs
      |> cleanup_merge_first_pass()
      |> cleanup_merge_second_pass()

    if changes do
      # If shifts were made, the diff needs reordering and another shift sweep.
      cleanup_merge(diffs)
    else
      diffs
    end
  end

  @spec cleanup_merge_first_pass(difflist()) :: difflist()
  defp cleanup_merge_first_pass([]), do: []

  defp cleanup_merge_first_pass(diffs) do
    # Add a dummy entry at the end
    (diffs ++ [{:equal, ""}])
    |> Cursor.from_list(position: 0)
    |> first_pass_loop({0, 0, "", ""})
    |> remove_dummy()
  end

  # Extract text for prefix and suffix from prev_diff and next_diff
  defp undiff({op, text}), do: {op, text}
  defp undiff(_), do: {:equal, ""}

  @type first_pass_acc() ::
          {non_neg_integer(), non_neg_integer(), String.t(), String.t()}

  @spec first_pass_loop(Cursor.t(), first_pass_acc()) :: difflist()
  # Verified tail-recursive
  defp first_pass_loop(%Cursor{current: nil} = diffs, _acc) do
    Cursor.to_list(diffs)
  end

  defp first_pass_loop(
         %Cursor{} = diffs,
         {count_delete, count_insert, text_delete, text_insert}
       ) do
    {prev_diff, this_diff, _next_diff} = Cursor.get(diffs)

    {op, text} = this_diff

    {diffs, acc} =
      case op do
        :insert ->
          {diffs, {count_delete, count_insert + 1, text_delete, text_insert <> text}}

        :delete ->
          {diffs, {count_delete + 1, count_insert, text_delete <> text, text_insert}}

        :equal ->
          # Upon reaching an equality, check for prior redundancies.
          diffs =
            if count_delete + count_insert > 1 do
              # Delete the offending records
              diffs = Cursor.delete_before(diffs, count_delete + count_insert)

              {diffs, text_insert, text_delete} =
                if count_delete > 0 && count_insert > 0 do
                  # Both types.
                  # Factor out any common prefixes.
                  {prefix, text1, text2} = common_prefix(text_insert, text_delete)

                  {diffs, text_insert, text_delete} =
                    if prefix != "" do
                      {prev_diff, _, _} = Cursor.get(diffs)

                      diffs1 =
                        if !is_nil(prev_diff) do
                          {prev_op, prev_text} = undiff(prev_diff)

                          if prev_op != :equal do
                            raise "Previous diff should have been an equality."
                          end

                          new_prev = {:equal, prev_text <> prefix}

                          diffs
                          |> Cursor.delete_before(1)
                          |> Cursor.insert_before([new_prev])
                        else
                          new_head = {:equal, prefix}

                          diffs
                          |> Cursor.insert_at_head([new_head])
                        end

                      {diffs1, text1, text2}
                    else
                      {diffs, text_insert, text_delete}
                    end

                  # Factor out any common suffixes.
                  {suffix, text1, text2} = common_suffix(text_insert, text_delete)

                  if suffix != "" do
                    new_cur = {:equal, suffix <> text}

                    diffs1 =
                      diffs
                      |> Cursor.delete(1)
                      |> Cursor.insert([new_cur])

                    {diffs1, text1, text2}
                  else
                    {diffs, text_insert, text_delete}
                  end
                else
                  {diffs, text_insert, text_delete}
                end

              # Insert the merged records.
              diffs =
                if text_delete != "" do
                  Cursor.insert_before(diffs, [{:delete, text_delete}])
                else
                  diffs
                end

              if text_insert != "" do
                Cursor.insert_before(diffs, [{:insert, text_insert}])
              else
                diffs
              end
            else
              {prev_op, prev_text} = undiff(prev_diff)

              if !is_nil(prev_diff) && prev_op == :equal do
                # Merge this equality with the previous one.

                new_cur = {prev_op, prev_text <> text}

                diffs
                |> Cursor.move_back(1)
                |> Cursor.delete(2)
                |> Cursor.insert([new_cur])
              else
                diffs
              end
            end

          {diffs, {0, 0, "", ""}}
      end

    diffs
    |> Cursor.move_forward()
    |> first_pass_loop(acc)
  end

  # Second pass: look for single edits surrounded on both sides by equalities
  # which can be shifted sideways to eliminate an equality.
  # e.g: A<ins>BA</ins>C -> <ins>AB</ins>AC
  @spec cleanup_merge_second_pass(difflist()) :: {difflist(), boolean()}
  defp cleanup_merge_second_pass([]), do: {[], false}

  defp cleanup_merge_second_pass(diffs) do
    {diffs, changes} =
      Cursor.from_list(diffs, position: 1)
      |> second_pass_loop(false)

    {diffs, changes}
  end

  @spec second_pass_loop(Cursor.t(), boolean()) :: {difflist(), boolean()}
  defp second_pass_loop(%Cursor{next: []} = diffs, changes), do: {Cursor.to_list(diffs), changes}

  defp second_pass_loop(%Cursor{} = diffs, changes) do
    {prev_diff, this_diff, next_diff} = Cursor.get(diffs)

    {prev_op, prev_text} = prev_diff
    {op, text} = this_diff
    {next_op, next_text} = next_diff

    {cursor, changes} =
      if prev_op == :equal && next_op == :equal do
        # This is a single edit surrounded by equalities.
        cond do
          String.ends_with?(text, prev_text) ->
            # Shift the edit over the previous equality.
            diffs =
              if prev_text != "" do
                text =
                  prev_text <> substring(text, 0, String.length(text) - String.length(prev_text))

                next_text = prev_text <> next_text
                new_cur_and_next = [{op, text}, {next_op, next_text}]

                # Delete this_diff and next_diff
                # Update this_diff and next_diff
                diffs
                |> Cursor.delete(2)
                |> Cursor.insert(new_cur_and_next)
              else
                diffs
              end

            # Delete prev_diff
            diffs =
              diffs
              |> Cursor.delete_before(1)

            {diffs, true}

          String.starts_with?(text, next_text) ->
            # Shift the edit over the next equality.
            prev_text = prev_text <> next_text
            text = substring(text, String.length(next_text)) <> next_text

            new_prev_and_cur = [{prev_op, prev_text}, {op, text}]

            # Delete prev_diff
            # Delete this_diff
            # Update prev_diff and this_diff
            # Delete next_diff
            diffs =
              diffs
              |> Cursor.move_back()
              |> Cursor.delete(2)
              |> Cursor.insert(new_prev_and_cur)
              |> Cursor.move_forward(2)
              |> Cursor.delete(1)

            {diffs, true}

          true ->
            {diffs, changes}
        end
      else
        {diffs, changes}
      end

    cursor
    |> Cursor.move_forward()
    |> second_pass_loop(changes)
  end

  # Remove the dummy entry at the end.
  @spec remove_dummy(difflist()) :: difflist()
  defp remove_dummy(diffs) do
    case List.last(diffs) do
      {_, ""} -> Enum.drop(diffs, -1)
      _ -> diffs
    end
  end

  @doc """
  Compute and return the source text (all equalities and deletions).

  `diffs` - a difflist.

  Returns the source text.
  """
  @spec text1(difflist()) :: String.t()
  def text1(diffs) do
    Enum.reduce(diffs, "", fn {op, text}, acc ->
      if op != :insert do
        acc <> text
      else
        acc
      end
    end)
  end

  @doc """
  Compute and return the destination text (all equalities and insertions).

  `diffs` - a difflist.

  Returns the destination text.
  """
  @spec text2(difflist()) :: String.t()
  def text2(diffs) do
    Enum.reduce(diffs, "", fn {op, text}, acc ->
      if op != :delete do
        acc <> text
      else
        acc
      end
    end)
  end
end
