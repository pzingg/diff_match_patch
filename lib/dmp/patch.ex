defmodule Dmp.Patch do
  @moduledoc """
  PATCH FUNCTIONS
  """

  import Dmp.StringUtils

  alias Dmp.{Cursor, Diff, Options}

  alias __MODULE__

  defstruct diffs: [], start1: 0, start2: 0, length1: 0, length2: 0

  @type t() :: %Patch{
          diffs: Diff.difflist(),
          start1: non_neg_integer(),
          start2: non_neg_integer(),
          length1: non_neg_integer(),
          length2: non_neg_integer()
        }
  @type patchlist() :: list(t())
  @type options() :: Options.t()

  defimpl String.Chars, for: Patch do
    @doc """
    Emulate GNU diff's format.
    Header: @@ -382,8 +481,9 @@
    Indices are printed as 1-based, not 0-based.
    Returns the GNU diff string.
    """
    def to_string(%Patch{
          diffs: diffs,
          start1: start1,
          start2: start2,
          length1: length1,
          length2: length2
        }) do
      coords1 =
        case length1 do
          0 ->
            "#{start1},0"

          1 ->
            "#{start1 + 1}"

          _ ->
            "#{start1 + 1},#{length1}"
        end

      coords2 =
        case length2 do
          0 ->
            "#{start2},0"

          1 ->
            "#{start2 + 1}"

          _ ->
            "#{start2 + 1},#{length2}"
        end

      header = "@@ - #{coords1} +#{coords2} @@\n"
      # Escape the body of the patch with %xx notation.
      diffs
      |> Enum.reduce(header, fn {op, text}, acc ->
        op_text =
          case op do
            :insert ->
              "+"

            :delete ->
              "-"

            :equal ->
              " "
          end

        text = URI.encode(text) |> String.replace("+", " ")
        acc <> op_text <> text <> "\n"
      end)
      |> Patch.unescape_for_encode_uri_compatability()
    end
  end

  @doc """
  Unescape selected chars for compatability with JavaScript's encodeURI.
  In speed critical applications this could be dropped since the
  receiving application will certainly decode these fine.
  Note that this function is case-sensitive.  Thus "%3f" would not be
  unescaped.  But this is ok because it is only called with the output of
  `URI.encode` which returns uppercase hex.

  Example: "%3F" -> "?", "%24" -> "$", etc.

  `str` The string to escape.

  Returns the unescaped string.
  """

  @encoding_map [
    {"%21", "!"},
    {"%7E", "~"},
    {"%27", "'"},
    {"%28", "("},
    {"%29", ")"},
    {"%3B", ";"},
    {"%2F", "/"},
    {"%3F", "?"},
    {"%3A", ":"},
    {"%40", "@"},
    {"%26", "&"},
    {"%3D", "="},
    {"%2B", "+"},
    {"%24", "$"},
    {"%2C", ","},
    {"%23", "#"}
  ]

  def unescape_for_encode_uri_compatability(str) do
    Enum.reduce(@encoding_map, str, fn {from, to}, acc -> String.replace(acc, from, to) end)
  end

  @doc """
  Increase the context until it is unique,
  but don't let the pattern expand beyond match_max_bits.
  `patch` - The patch to grow.
  `text` - Source text.
  """
  @spec add_context(t(), String.t(), non_neg_integer(), non_neg_integer()) :: String.t()
  def add_context(patch, "", _patch_margin, _match_max_bits), do: patch

  def add_context(
        %Patch{length1: length1, start2: start2} = patch,
        text,
        patch_margin,
        match_max_bits
      ) do
    pattern = substring(text, start2, start2 + length1)
    padding = increase_padding(0, pattern, patch, text, patch_margin, match_max_bits)

    prefix = substring(text, max(0, patch.start2 - padding), patch.start2)
    prefix_length = String.length(prefix)

    patch =
      if prefix != "" do
        # Add the prefix.
        # Roll back the start points.
        %Patch{
          patch
          | diffs: [{:equal, prefix} | patch.diffs],
            start1: patch.start1 - prefix_length,
            start2: patch.start2 - prefix_length
        }
      else
        patch
      end

    suffix =
      substring(
        text,
        patch.start2 + patch.length1,
        min(String.length(text), patch.start2 + patch.length1 + padding)
      )

    if suffix != "" do
      suffix_length = String.length(suffix)
      # Add the suffix.
      # Extend the lengths.
      %Patch{
        patch
        | diffs: patch.diffs ++ [{:equal, suffix}],
          length1: patch.length1 + prefix_length + suffix_length,
          length2: patch.length2 + prefix_length + suffix_length
      }
    else
      patch
    end
  end

  defp increase_padding(padding, pattern, patch, text, patch_margin, match_max_bits) do
    # Look for the first and last matches of pattern in text.  If two different
    # matches are found, increase the pattern length.
    if index_of(text, pattern) != last_index_of(text, pattern) &&
         String.length(pattern) < match_max_bits - patch_margin * 2 do
      padding = padding + patch_margin

      pattern =
        substring(
          text,
          max(0, patch.start2 - padding),
          min(String.length(text), patch.start2 + patch.length1 + padding)
        )

      increase_padding(padding, pattern, patch, text, patch_margin, match_max_bits)
    else
      # Add one chunk for good luck.
      padding + patch_margin
    end
  end

  @doc """
  Compute a list of patches to turn text1 into text2.
  A set of diffs will be computed.

  `text1` - Old text.
  `text2` - New text.

  Returns list of Patch objects.
  """
  def from_texts(text1, text2, opts \\ nil) do
    opts = opts || Options.default()
    diff_edit_cost = Map.fetch!(opts, :diff_edit_cost)

    # No diffs provided, compute our own.
    diffs = Diff.main(text1, text2, true)

    diffs =
      if Enum.count(diffs) > 2 do
        diffs
        |> Diff.cleanup_semantic()
        |> Diff.cleanup_efficiency(diff_edit_cost)
      else
        diffs
      end

    make(text1, diffs, opts)
  end

  @doc """
  Compute a list of patches to turn `text1` into `text2`.

  `text1` will be derived from the provided diffs.
  `diffs` - Array of Diff objects for `text1` to `text2`.

  Returns list of Patch objects.
  """
  def from_diffs(diffs, opts \\ nil) do
    # No origin string provided, compute our own.
    text1 = Diff.text1(diffs)
    make(text1, diffs, opts)
  end

  @doc """
  Compute a list of patches to turn `text1` into `text2`.
  `text2` is not provided, diffs are the delta between `text1` and `text2`.

  `text1` - Old text.
  `diffs` - Array of Diff objects for `text1` to `text2`.

  Returns list of Patch objects.
  """
  # Get rid of the null case.
  @spec make(String.t(), Diff.difflist(), nil | options()) :: patchlist()
  def make(text1, diffs, opts \\ nil)
  def make(_text1, [], _opts), do: []

  def make(text1, diffs, opts) do
    opts = opts || Options.default()
    patch_margin = Map.fetch!(opts, :patch_margin)
    match_max_bits = Map.fetch!(opts, :match_max_bits)

    # Start with text1 (prepatch_text) and apply the diffs until we arrive at
    # text2 (postpatch_text). We recreate the patches one by one to determine
    # context info.
    acc0 = {[], %Patch{}, text1, text1, 0, 0, List.last(diffs)}

    # `char_count1` Number of characters into the text1 string.
    # `char_count2` Number of characters into the text2 string.
    {patches, patch, prepatch_text, _postpatch_text, _char_count1, _char_count2, _last_diff} =
      Enum.reduce(diffs, acc0, fn diff,
                                  {patches, patch, prepatch_text, postpatch_text, char_count1,
                                   char_count2, last_diff} ->
        {op, text} = diff

        patch =
          if patch.diffs == [] && op != :equal do
            # A new patch starts here.
            %Patch{patch | start1: char_count1, start2: char_count2}
          else
            patch
          end

        text_length = String.length(text)

        {patches, patch, prepatch_text, postpatch_text, char_count1} =
          case op do
            :insert ->
              {patches,
               %Patch{patch | diffs: patch.diffs ++ [diff], length2: patch.length2 + text_length},
               prepatch_text,
               substring(postpatch_text, 0, char_count2) <>
                 text <> substring(postpatch_text, char_count2), char_count1}

            :delete ->
              {patches,
               %Patch{patch | diffs: patch.diffs ++ [diff], length1: patch.length1 + text_length},
               prepatch_text,
               substring(postpatch_text, 0, char_count2) <>
                 substring(postpatch_text, char_count2 + text_length), char_count1}

            :equal ->
              patch =
                if text_length <= 2 * patch_margin &&
                     patch.diffs != [] && diff != last_diff do
                  # Small equality inside a patch.
                  %Patch{
                    patch
                    | diffs: patch.diffs ++ [diff],
                      length1: patch.length1 + text_length,
                      length2: patch.length2 + text_length
                  }
                else
                  patch
                end

              if text_length >= 2 * patch_margin && diffs != [] do
                # Time for a new patch.
                if diffs != [] do
                  patch = add_context(patch, prepatch_text, patch_margin, match_max_bits)

                  {
                    patches ++ [patch],
                    %Patch{},
                    # Unlike Unidiff, our patch lists have a rolling context.
                    # https://github.com/google/diff-match-patch/wiki/Unidiff
                    # Update prepatch text & pos to reflect the application of the
                    # just completed patch.
                    prepatch_text,
                    postpatch_text,
                    char_count2
                  }
                else
                  {patches, patch, prepatch_text, postpatch_text, char_count1}
                end
              else
                {patches, patch, prepatch_text, postpatch_text, char_count1}
              end
          end

        # Update the current character count.
        {char_count1, char_count2} =
          case op do
            :insert ->
              {char_count1, char_count2 + text_length}

            :delete ->
              {char_count1 + text_length, char_count2}

            :equal ->
              {char_count1 + text_length, char_count2 + text_length}
          end

        {patches, patch, prepatch_text, postpatch_text, char_count1, char_count2, last_diff}
      end)

    if patch.diffs != [] do
      # Pick up the leftover patch if not empty.
      patch = add_context(patch, prepatch_text, patch_margin, match_max_bits)
      patches ++ [patch]
    else
      patches
    end
  end

  @doc """
  Merge a set of patches onto the text.  Return a patched text, as well
  as an array of true/false values indicating which patches were applied.

  `patches` - Array of Patch objects
  `text` - Old text.
  `opts` - Options.

  Returns two element Object array, containing the new text and an array of
  boolean values.
  """
  @spec apply(patchlist(), String.t(), nil | options()) :: {String.t(), list(boolean())}
  def apply([], text), do: {text, []}

  def apply(patches, text, opts \\ nil) do
    opts = opts || Options.default()
    match_max_bits = Map.fetch!(opts, :match_max_bits)
    patch_margin = Map.fetch!(opts, :match_max_bits)

    {patches, null_padding} = add_padding(patches, patch_margin)
    _text = null_padding <> text <> null_padding
    _patches = split_max(patches, match_max_bits, patch_margin)

    # TODO
  end

  @doc """
  Add some padding on text start and end so that edges can match something.
  Intended to be called only from within `Patch.apply`.

  `patches` - Array of Patch objects.
  `patch_margin` - Chunk size for context length.

  Returns The padding string added to each side.
  """
  @spec add_padding(patchlist(), non_neg_integer()) :: {patchlist(), String.t()}
  def add_padding(patches, patch_margin) do
    padding_length = patch_margin
    null_padding = Enum.reduce(1..padding_length, "", fn x, s -> s <> to_string([x]) end)

    # Bump all the patches forward.
    patches =
      Enum.map(patches, fn patch ->
        %Patch{
          patch
          | start1: patch.start1 + padding_length,
            start2: patch.start2 + padding_length
        }
      end)

    {first_patch, mid_patches} = List.pop_at(patches, 0)
    {last_patch, mid_patches} = List.pop_at(mid_patches, -1)

    if is_nil(last_patch) do
      {patches, null_padding}
    else
      # Add some padding on start of first diff.
      first_diff = List.first(first_patch.diffs)

      first_patch =
        if is_nil(first_diff) || elem(first_diff, 0) != :equal do
          # Add null_padding equality.
          # start1 and start2 should be 0.
          %Patch{
            first_patch
            | diffs: [{:equal, null_padding} | first_patch.diffs],
              start1: first_patch.start1 - padding_length,
              start2: first_patch.start2 - padding_length,
              length1: first_patch.length1 + padding_length,
              length2: first_patch.length2 + padding_length
          }
        else
          {op, text} = first_diff
          text_length = String.length(text)

          if padding_length > text_length do
            # Grow first equality.
            extra_length = padding_length - text_length
            text = substring(null_padding, text_length) <> text
            first_diff = {op, text}

            %Patch{
              first_patch
              | diffs: [first_diff | Enum.drop(first_patch.diffs, 1)],
                start1: first_patch.start1 - extra_length,
                start2: first_patch.start2 - extra_length,
                length1: first_patch.length1 + extra_length,
                length2: first_patch.length2 + extra_length
            }
          else
            first_patch
          end
        end

      # Add some padding on end of last diff.
      last_diff = List.last(last_patch.diffs)

      last_patch =
        if is_nil(last_diff) || elem(last_diff, 0) != :equal do
          # Add null_padding equality.
          %Patch{
            last_patch
            | diffs: last_patch.diffs ++ [{:equal, null_padding}],
              length1: last_patch.length1 + padding_length,
              length2: last_patch.length2 + padding_length
          }
        else
          {op, text} = last_diff
          text_length = String.length(text)

          if padding_length > text_length do
            # Grow last equality.
            extra_length = padding_length - text_length
            last_diff = {op, text <> substring(null_padding, 0, extra_length)}

            %Patch{
              last_patch
              | diffs: Enum.drop(last_patch.diffs, -1) ++ [last_diff],
                length1: last_patch.length1 + extra_length,
                length2: last_patch.length2 + extra_length
            }
          else
            last_patch
          end
        end

      {[first_patch | mid_patches] ++ [last_patch], null_padding}
    end
  end

  @doc """
  Look through the patches and break up any which are longer than the
  maximum limit of the match algorithm.
  Intended to be called only from within `Patch.apply`.

  `patches` - list of Patch objects.
  `max_match_bits` - The number of bits in an int.
  """
  def split_max(patches, match_max_bits, patch_margin) do
    cursor = Cursor.from_list(patches, position: :first)
    check_split(cursor, match_max_bits, patch_margin)
  end

  def check_split(%Cursor{current: bigpatch} = patches, match_max_bits, patch_margin) do
    if !is_nil(bigpatch) do
      patches =
        if bigpatch.length1 > match_max_bits do
          # Remove the big old patch.
          patches
          |> Cursor.delete(1)
          |> create_subpatches(bigpatch.start1, bigpatch.start2, "", match_max_bits, patch_margin)
        else
          patches
        end

      patches
      |> Cursor.move_forward()
      |> check_split(match_max_bits, patch_margin)
    else
      Cursor.to_list(patches)
    end
  end

  def create_subpatches(
        %Cursor{current: bigpatch} = patches,
        start1,
        start2,
        precontext,
        match_max_bits,
        patch_margin
      ) do
    if !is_nil(bigpatch) && bigpatch.diffs != [] do
      bigpatch_diffs = bigpatch.diffs
      precontext_length = String.length(precontext)

      # Create one of several smaller patches.
      patch = %Patch{start1: start1 - precontext_length, start2: start2 - precontext_length}

      patch =
        if precontext_length != 0 do
          %Patch{
            patch
            | length1: precontext_length,
              length2: precontext_length,
              diffs: [{:equal, precontext}]
          }
        else
          patch
        end

      {bigpatch_diffs, patch, start1, start2, empty} =
        create_subpatch_loop(
          bigpatch_diffs,
          patch,
          start1,
          start2,
          true,
          match_max_bits,
          patch_margin
        )

      # Compute the head context for the next patch.
      precontext = Diff.text2(patch.diffs)
      precontext = substring(precontext, max(0, String.length(precontext) - patch_margin))

      # Append the end context for this patch.
      text1 = Diff.text1(bigpatch_diffs)

      postcontext =
        if String.length(text1) > patch_margin do
          substring(text1, 0, patch_margin)
        else
          text1
        end

      patch =
        if postcontext != "" do
          postcontext_length = String.length(postcontext)
          last_diff = List.last(patch.diffs)

          patch_diffs =
            if !is_nil(last_diff) && elem(last_diff, 0) == :equal do
              Enum.drop(patch.diffs, -1) ++ [{:equal, elem(last_diff, 1) <> postcontext}]
            else
              patch.diffs ++ [{:equal, postcontext}]
            end

          %Patch{
            patch
            | diffs: patch_diffs,
              length1: patch.length1 + postcontext_length,
              length2: patch.length2 + postcontext_length
          }
        else
          patch
        end

      # Update bigpatch.diffs
      patches =
        patches
        |> Cursor.insert_before([%Patch{bigpatch | diffs: bigpatch_diffs}])
        |> Cursor.delete(1)

      patches =
        if !empty do
          Cursor.insert(patches, [patch])
        else
          patches
        end

      patches
      |> Cursor.move_forward()
      |> create_subpatches(start1, start2, precontext, match_max_bits, patch_margin)
    else
      patches
    end
  end

  def create_subpatch_loop(
        bigpatch_diffs,
        patch,
        start1,
        start2,
        empty,
        match_max_bits,
        patch_margin
      ) do
    if bigpatch_diffs != [] && patch.length1 < match_max_bits - patch_margin do
      [{diff_type, diff_text}, _] = bigpatch_diffs
      diff_text_length = String.length(diff_text)

      {diffs, patch, start1, start2, empty} =
        cond do
          diff_type == :insert ->
            # Insertions are harmless.
            [first_diff | rest] = bigpatch_diffs

            {rest,
             %Patch{
               patch
               | diffs: patch.diffs ++ [first_diff],
                 length2: patch.length2 + diff_text_length
             }, start1, start2 + diff_text_length, false}

          diff_type == :delete && Enum.count(patch.diffs) == 1 &&
            List.first(patch.diffs) |> elem(0) == :equal &&
              diff_text_length > 2 * match_max_bits ->
            # This is a large deletion.  Let it pass in one chunk.
            [_first_diff | rest] = bigpatch_diffs

            {rest,
             %Patch{
               patch
               | diffs: patch.diffs ++ [{diff_type, diff_text}],
                 length1: patch.length1 + diff_text_length
             }, start1 + diff_text_length, start2, false}

          true ->
            # Deletion or equality.  Only take as much as we can stomach.
            diff_text =
              substring(
                diff_text,
                0,
                min(diff_text_length, match_max_bits - patch.length1 - patch_margin)
              )

            diff_text_length = String.length(diff_text)
            patch = %Patch{patch | length1: patch.length1 + diff_text_length}
            start1 = start1 + diff_text_length

            {patch, start2, empty} =
              if diff_type == :equal do
                {%Patch{patch | length2: patch.length2 + diff_text_length},
                 start2 + diff_text_length, empty}
              else
                {patch, start2, false}
              end

            [{first_op, first_text} | rest] = bigpatch_diffs

            diffs =
              if diff_text == first_text do
                rest
              else
                first_text = substring(first_text, diff_text_length)
                [{first_op, first_text} | rest]
              end

            {diffs, %Patch{patch | diffs: patch.diffs ++ [{diff_type, diff_text}]}, start1,
             start2, empty}
        end

      {diffs, patch, start1, start2, empty}
    else
      {bigpatch_diffs, patch, start1, start2, empty}
    end
  end

  @doc """
  Take a list of patches and return a textual representation.

  `patches` - List of Patch objects.

  Returns text representation of patches.
  """
  def to_text(patches) do
    Enum.reduce(patches, "", fn patch, acc -> acc <> to_string(patch) end)
  end

  @doc """
  Parse a textual representation of patches and return a List of Patch
  objects.

  `textline` - Text representation of patches.

  Returns List of Patch objects.
  """
  @patch_header ~r/^@@ -(\d+),?(\d*) \+(\d+),?(\d*) @@$/

  @spec from_string(String.t()) :: patchlist()
  def from_string(""), do: []

  def from_string(text) do
    text
    |> String.split("\n")
    |> Cursor.from_list(position: :first)
    |> parse_patch_header([])
    |> Enum.reverse()
  end

  @spec parse_patch_header(Cursor.t(), patchlist()) :: patchlist()
  def parse_patch_header(%Cursor{current: line} = lines, patches) do
    case line do
      nil ->
        patches

      "" ->
        Cursor.move_forward(lines)
        |> parse_patch_header(patches)

      _ ->
        case Regex.run(@patch_header, line) do
          nil ->
            raise "Invalid patch header: #{line}"

          groups ->
            start1 =
              case Integer.parse(Enum.at(groups, 1)) do
                {start1, ""} -> start1
                _ -> raise "Invalid patch header (start1): #{line}"
              end

            {start1, length1} =
              case Enum.at(groups, 2) do
                "" ->
                  {start1 - 1, 1}

                "0" ->
                  {start1, 0}

                n ->
                  case Integer.parse(n) do
                    {length1, ""} -> {start1 - 1, length1}
                    _ -> raise "Invalid patch header (length1): #{line}"
                  end
              end

            start2 =
              case Integer.parse(Enum.at(groups, 3)) do
                {start2, ""} -> start2
                _ -> raise "Invalid patch header (start2): #{line}"
              end

            {start2, length2} =
              case Enum.at(groups, 4) do
                "" ->
                  {start2 - 1, 1}

                "0" ->
                  {start2, 0}

                n ->
                  case Integer.parse(n) do
                    {length2, ""} -> {start2 - 1, length2}
                    _ -> raise "Invalid patch header (length2): #{line}"
                  end
              end

            {lines, diffs} =
              lines
              |> Cursor.move_forward()
              |> parse_patch_body([])

            parse_patch_header(
              lines,
              [
                %Patch{
                  diffs: Enum.reverse(diffs),
                  start1: start1,
                  start2: start2,
                  length1: length1,
                  length2: length2
                }
                | patches
              ]
            )
        end
    end
  end

  @spec parse_patch_body(Cursor.t(), Diff.difflist()) :: {Cursor.t(), Diff.difflist()}
  def parse_patch_body(%Cursor{current: line} = lines, diffs) do
    case line do
      nil ->
        {lines, diffs}

      "" ->
        Cursor.move_forward(lines)
        |> parse_patch_body(diffs)

      _ ->
        {sign, line1} = String.split_at(line, 1)
        # Decode would change all "+" to " "
        line1 = line1 |> String.replace("+", "%2B") |> URI.decode()

        diff =
          case sign do
            "-" ->
              # Deletion.
              {:delete, line1}

            "+" ->
              # Insertion.
              {:insert, line1}

            " " ->
              # Minor equality.
              {:equal, line1}

            "@" ->
              nil

            _ ->
              # WTF?
              raise "Invalid patch mode '#{sign}' in: #{line}"
          end

        if is_nil(diff) do
          {lines, diffs}
        else
          Cursor.move_forward(lines)
          |> parse_patch_body([diff | diffs])
        end
    end
  end
end
