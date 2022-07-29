defmodule Dmp.Patch do
  @moduledoc """
  Apply a list of patches onto plain text. Use best effort to apply
  patch even when the underlying text doesn't match.
  """

  import Dmp.StringUtils

  alias Dmp.{Cursor, Diff, Match, Options}

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

      header = "@@ -#{coords1} +#{coords2} @@\n"
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

        text = uri_encode(text)
        acc <> op_text <> text <> "\n"
      end)
      |> unescape_for_encode_uri_compatability()
    end
  end

  @doc """
  Increase the context until it is unique,
  but don't let the pattern expand beyond match_max_bits.

  `patch` - The patch to grow.
  `text` - Source text.
  `patch_margin` - Chunk size for context length.
  `match_max_bits` - The number of bits in an integer (default is expected 32).

  Returns updated Patch.
  """
  @spec add_context(t(), String.t(), non_neg_integer(), non_neg_integer()) :: String.t()
  def add_context(patch, text, patch_margin, match_max_bits \\ 32)
  def add_context(patch, "", _patch_margin, _match_max_bits), do: patch

  def add_context(
        %Patch{} = patch,
        text,
        patch_margin,
        match_max_bits
      ) do
    pattern = substring(text, patch.start2, patch.start2 + patch.length1)
    {padding, _pattern} = increase_padding(0, pattern, patch, text, patch_margin, match_max_bits)

    prefix = substring(text, max(0, patch.start2 - padding), patch.start2)
    prefix_length = String.length(prefix)

    patch =
      if prefix_length != 0 do
        # Add the prefix.
        %Patch{patch | diffs: [{:equal, prefix} | patch.diffs]}
      else
        patch
      end

    suffix =
      substring(
        text,
        patch.start2 + patch.length1,
        patch.start2 + patch.length1 + padding
      )

    suffix_length = String.length(suffix)

    patch =
      if suffix_length != 0 do
        # Add the suffix.
        %Patch{patch | diffs: patch.diffs ++ [{:equal, suffix}]}
      else
        patch
      end

    # Roll back the start points.
    # Extend lengths.
    %Patch{
      patch
      | start1: patch.start1 - prefix_length,
        start2: patch.start2 - prefix_length,
        length1: patch.length1 + prefix_length + suffix_length,
        length2: patch.length2 + prefix_length + suffix_length
    }
  end

  defp increase_padding(padding, pattern, patch, text, patch_margin, match_max_bits) do
    # Look for the first and last matches of pattern in text.  If two different
    # matches are found, increase the pattern length.
    if index_of(text, pattern) != last_index_of(text, pattern) &&
         (match_max_bits == 0 ||
            String.length(pattern) < match_max_bits - patch_margin * 2) do
      padding = padding + patch_margin

      pattern =
        substring(
          text,
          max(0, patch.start2 - padding),
          patch.start2 + patch.length1 + padding
        )

      increase_padding(padding, pattern, patch, text, patch_margin, match_max_bits)
    else
      # Add one chunk for good luck.
      {padding + patch_margin, pattern}
    end
  end

  @doc """
  Compute a list of patches to turn `text1` into `text2`. `text1` will be derived
  from the provided diffs.

  `diffs` - A difflist from `text1` to `text2`.
  `opts` - A `DiffMatchPatch.Options` struct, or `nil` to use default options.

  Returns a patchlist.
  """
  def from_diffs(diffs, opts \\ nil) do
    # No origin string provided, compute our own.
    text1 = Diff.text1(diffs)
    make(text1, diffs, opts)
  end

  @doc """
  Deprecated

  Compute a list of patches to turn `text1` into `text2`. `text2` is ignored.
  `diffs` are the delta between text1 and text2.

  `text1` - Old text.
  `text2` - Ignored.
  `diffs` - A difflist from `text1` to `text2`.
  `opts` - A `DiffMatchPatch.Options` struct, or `nil` to use default options.

  Returns a patchlist.
  """
  def from_texts_and_diffs(text1, _text2, diffs, opts \\ nil) do
    make(text1, diffs, opts)
  end

  @doc """
  This function can be called two ways. In either case the first argument,
  `a` is the original text (`text1`).

  The second argument `b` has two cases:

  If `b` is a String `text2`, a difflist that turns `text1` into `text2` will be computed.
  If `b` is a difflist, it is the delta between `text1` and the target `text2`.

  `opts` - A `DiffMatchPatch.Options` struct, or `nil` to use default options.

  Returns a patchlist.
  """
  @spec make(String.t(), String.t() | Diff.difflist(), nil | options()) :: patchlist()
  def make(a, b, opts \\ nil)

  def make(text1, text2, opts) when is_binary(text2) do
    opts = Options.valid_options!(opts)

    # No diffs provided, compute our own.
    diffs = Diff.main(text1, text2, true)

    diffs =
      if Enum.count(diffs) > 2 do
        diffs
        |> Diff.cleanup_semantic()
        |> Diff.cleanup_efficiency(opts.diff_edit_cost)
      else
        diffs
      end

    make_impl(text1, diffs, opts)
  end

  def make(text1, diffs, opts) when is_list(diffs) do
    opts = Options.valid_options!(opts)
    make_impl(text1, diffs, opts)
  end

  # Get rid of the null case.
  defp make_impl(_text1, [], _opts), do: []

  defp make_impl(text1, diffs, opts) do
    {patches, patch, prepatch_text} =
      diffs
      |> Cursor.from_list(position: 0)
      |> make_loop({[], %Patch{}, text1, text1, 0, 0}, opts.patch_margin, opts.match_max_bits)

    if patch.diffs != [] do
      # Pick up the leftover patch if not empty.
      patch = add_context(patch, prepatch_text, opts.patch_margin, opts.match_max_bits)
      patches = patches ++ [patch]

      patches
    else
      patches
    end
  end

  # Verified tail-recursive
  def make_loop(
        %Cursor{current: nil},
        {patches, patch, prepatch_text, _postpatch_text, _char_count1, _char_count2},
        _patch_margin,
        _match_max_bits
      ) do
    {patches, patch, prepatch_text}
  end

  # Start with text1 (prepatch_text) and apply the diffs until we arrive at
  # text2 (postpatch_text). We recreate the patches one by one to determine
  # context info.
  # `char_count1` Number of characters into the text1 string.
  # `char_count2` Number of characters into the text2 string.
  def make_loop(
        %Cursor{current: {op, text} = diff} = diffs,
        {patches, patch, prepatch_text, postpatch_text, char_count1, char_count2},
        patch_margin,
        match_max_bits
      ) do
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
                 patch.diffs != [] && Cursor.has_next?(diffs) do
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

          if text_length >= 2 * patch_margin do
            # Time for a new patch.
            if patch.diffs != [] do
              patch = add_context(patch, prepatch_text, patch_margin, match_max_bits)
              patches = patches ++ [patch]

              {
                patches,
                %Patch{},
                # Unlike Unidiff, our patch lists have a rolling context.
                # https://github.com/google/diff-match-patch/wiki/Unidiff
                # Update prepatch text & pos to reflect the application of the
                # just completed patch.
                postpatch_text,
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

    diffs
    |> Cursor.move_forward()
    |> make_loop(
      {patches, patch, prepatch_text, postpatch_text, char_count1, char_count2},
      patch_margin,
      match_max_bits
    )
  end

  @doc """
  Merge a set of patches onto the text.  Return a patched text, as well
  as an array of true/false values indicating which patches were applied.

  `patches` - a patchlist.
  `text` - Old text.
  `opts` - A `DiffMatchPatch.Options` struct, or `nil` to use default options.

  Returns two element Object array, containing the new text and an array of
  boolean values.
  """
  @spec apply(patchlist(), String.t(), nil | options()) :: {String.t(), list(boolean())}
  def apply([], text), do: {text, []}

  def apply(patches, text, opts \\ nil) do
    opts = Options.valid_options!(opts)
    {patches, null_padding} = add_padding(patches, opts.patch_margin)
    text = null_padding <> text <> null_padding
    patches = split_max(patches, opts.patch_margin, opts.match_max_bits)

    {results, text, _delta, _opts} =
      Enum.reduce(
        patches,
        {[], text, 0, opts},
        fn patch, acc -> apply_loop(patch, acc) end
      )

    # Strip the padding off.
    null_padding_length = String.length(null_padding)
    text = substring(text, null_padding_length, String.length(text) - null_padding_length)

    {text, Enum.reverse(results)}
  end

  @type apply_loop_acc() ::
          {list(boolean()), String.t(), integer(), options()}

  defp apply_loop(
         patch,
         {results, text, delta, opts}
       ) do
    expected_loc = patch.start2 + delta
    text1 = Diff.text1(patch.diffs)
    text1_length = String.length(text1)
    match_max_bits = opts.match_max_bits

    {start_loc, end_loc} =
      if text1_length > match_max_bits do
        # split_max will only provide an oversized pattern in the case of
        # a monster delete.
        start_loc = Match.main(text, substring(text1, 0, match_max_bits), expected_loc, opts)

        if start_loc != -1 do
          end_loc =
            Match.main(
              text,
              substring(text1, text1_length - match_max_bits),
              expected_loc + text1_length - match_max_bits,
              opts
            )

          if end_loc == -1 || start_loc >= end_loc do
            # Can't find valid trailing context.  Drop this patch.
            {-1, end_loc}
          else
            {start_loc, end_loc}
          end
        else
          {start_loc, -1}
        end
      else
        {Match.main(text, text1, expected_loc, opts), -1}
      end

    if start_loc == -1 do
      # No match found.  :(
      # Subtract the delta for this failed patch from subsequent patches.
      delta = delta - (patch.length2 - patch.length1)
      {[false | results], text, delta, opts}
    else
      # Found a match.  :)
      delta = start_loc - expected_loc

      text2 =
        if end_loc == -1 do
          substring(text, start_loc, start_loc + text1_length)
        else
          substring(text, start_loc, end_loc + match_max_bits)
        end

      {found, text} =
        if text1 == text2 do
          # Perfect match, just shove the replacement text in.
          text2 = Diff.text2(patch.diffs)

          text =
            substring(text, 0, start_loc) <>
              text2 <>
              substring(text, start_loc + text1_length)

          {true, text}
        else
          # Imperfect match.  Run a diff to get a framework of equivalent
          # indices.
          diffs = Diff.main_(text1, text2, false, opts)
          lev = Diff.levenshtein(diffs)

          if text1_length > match_max_bits &&
               lev / text1_length > opts.patch_delete_threshold do
            # The end points match, but the content is unacceptably bad.

            {false, text}
          else
            diffs = Diff.cleanup_semantic_lossless(diffs)

            {text, _index1} =
              Enum.reduce(patch.diffs, {text, 0}, fn {op, dtext}, {text, index1} ->
                dtext_length = String.length(dtext)

                case op do
                  :equal ->
                    {text, index1 + dtext_length}

                  :insert ->
                    # Insertion
                    index2 = Diff.x_index(diffs, index1)

                    text =
                      substring(text, 0, start_loc + index2) <>
                        dtext <>
                        substring(text, start_loc + index2)

                    {text, index1 + dtext_length}

                  :delete ->
                    # Deletion
                    index2 = Diff.x_index(diffs, index1)
                    index3 = Diff.x_index(diffs, index1 + dtext_length)

                    text =
                      substring(text, 0, start_loc + index2) <>
                        substring(text, start_loc + index3)

                    {text, index1}
                end
              end)

            {true, text}
          end
        end

      {[found | results], text, delta, opts}
    end
  end

  @doc """
  Add some padding on text start and end so that edges can match something.
  Intended to be called only from within `Patch.apply`.

  `patches` - a patchlist..
  `patch_margin` - Chunk size for context length.

  Returns a tuple of the padded patchlist and the padding string added to each side.
  """
  @spec add_padding(patchlist(), non_neg_integer()) :: {patchlist(), String.t()}
  def add_padding([], _patch_margin), do: {[], ""}

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

    {first_patch, mid_patches, last_patch} =
      case patches do
        [one_patch] ->
          {one_patch, [], nil}

        [first_patch, last_patch] ->
          {first_patch, [], last_patch}

        [first_patch | mid_patches] ->
          {last_patch, mid_patches} = List.pop_at(mid_patches, -1)
          {first_patch, mid_patches, last_patch}
      end

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
    {last_patch, last_is_first} =
      if is_nil(last_patch) do
        {first_patch, true}
      else
        {last_patch, false}
      end

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

    if last_is_first do
      {[last_patch], null_padding}
    else
      {[first_patch | mid_patches] ++ [last_patch], null_padding}
    end
  end

  @doc """
  Look through the patches and break up any which are longer than the
  maximum limit of the match algorithm.
  Intended to be called only from within `Patch.apply`.

  `patches` - a patchlist.
  `patch_margin` - Chunk size for context length.
  `match_max_bits` - The number of bits in an int (default 32).
  """
  @spec split_max(patchlist(), non_neg_integer(), non_neg_integer()) :: patchlist()
  def split_max(patches, patch_margin, match_max_bits \\ 32) do
    if match_max_bits <= patch_margin do
      patches
    else
      patches
      |> Cursor.from_list(position: 0)
      |> split_max_loop(patch_margin, match_max_bits)
    end
  end

  # Verified tail-recursive
  @spec split_max_loop(Cursor.t(), non_neg_integer(), non_neg_integer()) :: patchlist()
  defp split_max_loop(%Cursor{current: nil} = patches, _patch_margin, _patch_size),
    do: Cursor.to_list(patches)

  defp split_max_loop(%Cursor{current: bigpatch} = patches, patch_margin, patch_size) do
    patches =
      if bigpatch.length1 > patch_size do
        # Remove the big old patch.
        patches
        |> Cursor.delete(1)
        |> Cursor.move_back()
        |> create_subpatches(
          bigpatch.diffs,
          bigpatch.start1,
          bigpatch.start2,
          "",
          patch_margin,
          patch_size
        )
      else
        Cursor.move_forward(patches)
      end

    split_max_loop(patches, patch_margin, patch_size)
  end

  # Verified tail-recursive
  defp create_subpatches(
         patches,
         bigpatch_diffs,
         start1,
         start2,
         precontext,
         patch_margin,
         patch_size
       ) do
    if bigpatch_diffs == [] do
      patches
    else
      precontext_length = String.length(precontext)
      # Create one of several smaller patches.
      patch = %Patch{start1: start1 - precontext_length, start2: start2 - precontext_length}

      patch =
        if precontext_length != 0 do
          %Patch{
            patch
            | diffs: [{:equal, precontext}],
              length1: precontext_length,
              length2: precontext_length
          }
        else
          patch
        end

      {bigpatch_diffs, patch, start1, start2, empty} =
        subpatch_loop(
          bigpatch_diffs,
          patch,
          start1,
          start2,
          true,
          patch_margin,
          patch_size
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

      patches =
        if !empty do
          patches =
            patches
            |> Cursor.move_forward()
            |> Cursor.insert_before([patch])

          patches
        else
          patches
        end

      # BUG - should do it again for example 4
      # Call again, without advancing
      create_subpatches(
        patches,
        bigpatch_diffs,
        start1,
        start2,
        precontext,
        patch_margin,
        patch_size
      )
    end
  end

  def subpatch_loop(
        bigpatch_diffs,
        patch,
        start1,
        start2,
        empty,
        patch_margin,
        patch_size
      ) do
    if bigpatch_diffs != [] && patch.length1 < patch_size - patch_margin do
      {diff_type, diff_text} = List.first(bigpatch_diffs)
      diff_text_length = String.length(diff_text)

      {bigpatch_diffs, patch, start1, start2, empty} =
        cond do
          diff_type == :insert ->
            # Insertions are harmless.
            [first_diff | rest] = bigpatch_diffs

            patch = %Patch{
              patch
              | diffs: patch.diffs ++ [first_diff],
                length2: patch.length2 + diff_text_length
            }

            {rest, patch, start1, start2 + diff_text_length, false}

          diff_type == :delete && Enum.count(patch.diffs) == 1 &&
            List.first(patch.diffs) |> elem(0) == :equal &&
              diff_text_length > 2 * patch_size ->
            # This is a large deletion.  Let it pass in one chunk.
            [_first_diff | rest] = bigpatch_diffs

            patch = %Patch{
              patch
              | diffs: patch.diffs ++ [{diff_type, diff_text}],
                length1: patch.length1 + diff_text_length
            }

            {rest, patch, start1 + diff_text_length, start2, false}

          true ->
            # Deletion or equality.  Only take as much as we can stomach.
            diff_text =
              substring(
                diff_text,
                0,
                min(diff_text_length, patch_size - patch.length1 - patch_margin)
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

            patch = %Patch{patch | diffs: patch.diffs ++ [{diff_type, diff_text}]}

            bigpatch_diffs =
              if diff_text == first_text do
                rest
              else
                first_text = substring(first_text, diff_text_length)
                [{first_op, first_text} | rest]
              end

            {bigpatch_diffs, patch, start1, start2, empty}
        end

      subpatch_loop(
        bigpatch_diffs,
        patch,
        start1,
        start2,
        empty,
        patch_margin,
        patch_size
      )
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

  @spec from_text(String.t()) :: patchlist()
  def from_text(""), do: []

  def from_text(text) do
    text
    |> String.split("\n")
    |> Cursor.from_list(position: 0)
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
