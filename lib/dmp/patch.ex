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
    Emulate GNU diff's format:

    ```
    @@ -382,8 +481,9 @@
    +change
     same

    ```

    Indices are printed as 1-based, not 0-based.
    """
    def to_string(%Patch{
          diffs: diffs,
          start1: start1,
          start2: start2,
          length1: length1,
          length2: length2
        }) do
      coords1 = patch_location(start1, length1)
      coords2 = patch_location(start2, length2)
      header = "@@ -#{coords1} +#{coords2} @@\n"
      # Escape the body of the patch with %xx notation.
      diffs
      |> Enum.reduce(header, fn diff, acc -> append_diff_contents(diff, acc) end)
      |> unescape_for_encode_uri_compatability()
    end

    defp patch_location(start, length) do
      case length do
        0 ->
          "#{start},0"

        1 ->
          "#{start + 1}"

        _ ->
          "#{start + 1},#{length}"
      end
    end

    defp append_diff_contents({op, text}, acc) do
      op_text =
        case op do
          :insert ->
            "+"

          :delete ->
            "-"

          :equal ->
            " "

          _ ->
            raise RuntimeError, "Invalid operation #{inspect(op)}"
        end

      text = uri_encode(text)
      acc <> op_text <> text <> "\n"
    end
  end

  @doc """
  Increase the context until it is unique,
  but don't let the pattern expand beyond match_max_bits.

    * `patch` - The `Patch` to grow.
    * `text` - Source text.
    * `patch_margin` - Chunk size for context length.
    * `match_max_bits` - The number of bits in an integer (default is expected 32).

  Returns the updated `Patch`.
  """
  @spec add_context(t(), String.t(), non_neg_integer(), non_neg_integer()) :: t()
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

    * `diffs` - A difflist from `text1` to `text2`.
    * `opts` - A options keyword list, `[]` to use the default options.

  Returns a patchlist.
  """
  @spec from_diffs(Diff.difflist(), options()) :: patchlist()
  def from_diffs(diffs, opts \\ []) do
    # No origin string provided, compute our own.
    text1 = Diff.text1(diffs)
    make(text1, diffs, opts)
  end

  @doc """
  Deprecated

  Compute a list of patches to turn `text1` into `text2`. `text2` is ignored.
  `diffs` are the delta between text1 and text2.

    * `text1` - Old text.
    * `text2` - Ignored.
    * `diffs` - A difflist from `text1` to `text2`.
    * `opts` - A options keyword list, `[]` to use the default options.

  Returns a patchlist.
  """
  @spec from_texts_and_diffs(String.t(), String.t(), Diff.difflist(), options()) ::
          patchlist()
  def from_texts_and_diffs(text1, _text2, diffs, opts \\ []) do
    make(text1, diffs, opts)
  end

  @doc """
  This function can be called two ways. In either case the first argument,
  `a` is the original text (`text1`).

  The second argument `b` has two cases:

    * If `b` is a String `text2`, a difflist that turns `text1` into `text2` will be computed.
    * If `b` is a difflist, it is the delta between `text1` and the target `text2`.
    * `opts` - A options keyword list, `[]` to use the default options.

  Returns a patchlist.
  """
  @spec make(String.t(), String.t() | Diff.difflist(), options()) :: patchlist()
  def make(a, b, opts \\ [])

  def make(text1, text2, opts) when is_binary(text2) do
    opts = Options.valid_options!(opts)
    diff_edit_cost = Keyword.fetch!(opts, :diff_edit_cost)

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

    make_impl(text1, diffs, opts)
  end

  def make(text1, diffs, opts) when is_list(diffs) do
    opts = Options.valid_options!(opts)
    make_impl(text1, diffs, opts)
  end

  # Get rid of the null case.
  @spec make_impl(String.t(), Diff.difflist(), options()) :: patchlist()
  defp make_impl(_text1, [], _opts), do: []

  defp make_impl(text1, diffs, opts) do
    patch_margin = Keyword.fetch!(opts, :patch_margin)
    match_max_bits = Keyword.fetch!(opts, :match_max_bits)

    {patches, patch, prepatch_text} =
      diffs
      |> Cursor.from_list(position: 0)
      |> make_loop(
        {[], %Patch{}, text1, text1, 0, 0},
        patch_margin,
        match_max_bits
      )

    if patch.diffs != [] do
      # Pick up the leftover patch if not empty.
      patch =
        add_context(
          patch,
          prepatch_text,
          patch_margin,
          match_max_bits
        )

      patches = patches ++ [patch]

      patches
    else
      patches
    end
  end

  # Start with text1 (`prepatch_text`) and apply the diffs until we arrive at
  # text2 (`postpatch_text`).
  # We recreate the patches one by one to determine context info.
  # `char_count1` Number of characters into the text1 string.
  # `char_count2` Number of characters into the text2 string.

  defp make_loop(
         %Cursor{current: nil},
         {patches, patch, prepatch_text, _postpatch_text, _char_count1, _char_count2},
         _patch_margin,
         _match_max_bits
       ) do
    {patches, patch, prepatch_text}
  end

  defp make_loop(
         diffs,
         {patches, patch, prepatch_text, postpatch_text, char_count1, char_count2},
         patch_margin,
         match_max_bits
       ) do
    patch = maybe_new_patch(diffs, patch, char_count1, char_count2)

    acc = {patches, patch, prepatch_text, postpatch_text, char_count1, char_count2}

    # Add to patch.diffs, then update the current character count
    # (`char_count1` and `char_count2`).
    acc = add_diff_to_patch(diffs, acc, patch_margin, match_max_bits)

    diffs
    |> Cursor.move_forward()
    |> make_loop(
      acc,
      patch_margin,
      match_max_bits
    )
  end

  # For `:insert` and `:delete` diffs, a new patch starting at the current character count.
  defp maybe_new_patch(%Cursor{current: {:equal, _}}, patch, _start1, _start2), do: patch

  defp maybe_new_patch(_diffs, %Patch{diffs: []} = patch, start1, start2) do
    %Patch{patch | start1: start1, start2: start2}
  end

  defp maybe_new_patch(_diffs, patch, _start1, _start2), do: patch

  # Add an `:insert` diff and update the current character count.
  defp add_diff_to_patch(
         %Cursor{current: {:insert, text} = this_diff},
         {patches, patch, prepatch_text, postpatch_text, char_count1, char_count2},
         _patch_margin,
         _match_max_bits
       ) do
    text_length = String.length(text)

    {patches,
     %Patch{patch | diffs: patch.diffs ++ [this_diff], length2: patch.length2 + text_length},
     prepatch_text,
     substring(postpatch_text, 0, char_count2) <>
       text <> substring(postpatch_text, char_count2), char_count1, char_count2 + text_length}
  end

  # Add a `:delete` diff and update the current character count.
  defp add_diff_to_patch(
         %Cursor{current: {:delete, text} = this_diff},
         {patches, patch, prepatch_text, postpatch_text, char_count1, char_count2},
         _patch_margin,
         _match_max_bits
       ) do
    text_length = String.length(text)

    {patches,
     %Patch{patch | diffs: patch.diffs ++ [this_diff], length1: patch.length1 + text_length},
     prepatch_text,
     substring(postpatch_text, 0, char_count2) <>
       substring(postpatch_text, char_count2 + text_length), char_count1 + text_length,
     char_count2}
  end

  # Nothing to do for equality with empty diffs, just update the current character count.
  defp add_diff_to_patch(
         %Cursor{current: {:equal, text}},
         {patches, %Patch{diffs: []} = patch, prepatch_text, postpatch_text, char_count1,
          char_count2},
         _patch_margin,
         _match_max_bits
       ) do
    text_length = String.length(text)

    {patches, patch, prepatch_text, postpatch_text, char_count1 + text_length,
     char_count2 + text_length}
  end

  # Add an `:equal` diff and update the current character count.
  defp add_diff_to_patch(
         %Cursor{current: {:equal, text}} = diffs,
         {patches, patch, prepatch_text, postpatch_text, char_count1, char_count2},
         patch_margin,
         match_max_bits
       ) do
    text_length = String.length(text)

    if text_length <= 2 * patch_margin do
      patch = small_equality_patch(diffs, patch, text_length)

      {patches, patch, prepatch_text, postpatch_text, char_count1 + text_length,
       char_count2 + text_length}
    else
      {patches, patch, prepatch_text, postpatch_text, char_count1} =
        large_equality_patch(
          patches,
          patch,
          prepatch_text,
          postpatch_text,
          char_count1,
          char_count2,
          patch_margin,
          match_max_bits
        )

      {patches, patch, prepatch_text, postpatch_text, char_count1 + text_length,
       char_count2 + text_length}
    end
  end

  # Small equality inside a patch.
  # No op if this is the last diff.
  defp small_equality_patch(%Cursor{next: []}, patch, _), do: patch

  defp small_equality_patch(%Cursor{current: this_diff}, patch, text_length) do
    %Patch{
      patch
      | diffs: patch.diffs ++ [this_diff],
        length1: patch.length1 + text_length,
        length2: patch.length2 + text_length
    }
  end

  # Equality diff is too big.
  # Time for a new patch.
  defp large_equality_patch(
         patches,
         patch,
         prepatch_text,
         postpatch_text,
         _char_count1,
         char_count2,
         patch_margin,
         match_max_bits
       ) do
    patch = add_context(patch, prepatch_text, patch_margin, match_max_bits)
    patches = patches ++ [patch]

    {
      patches,
      %Patch{},
      # Unlike Unidiff, our patch lists have a rolling context.
      # https://github.com/google/diff-match-patch/wiki/Unidiff
      # Update prepatch text and pos to reflect the application of the
      # just completed patch.
      postpatch_text,
      postpatch_text,
      char_count2
    }
  end

  @doc """
  Merge a set of patches onto the text.  Return a patched text, as well
  as an array of true/false values indicating which patches were applied.

    * `patches` - A patchlist.
    * `text` - Text to apply patch to.
    * `opts` - A options keyword list, `[]` to use the default options.

  Returns a tuple with two elements: the patched text, and a list of
  boolean values. Each boolean corresponds to a patch in the patchlist,
  and is `true` if a match was found for the corresponding patch.
  """
  @spec apply(patchlist(), String.t(), options()) :: {String.t(), list(boolean())}
  def apply([], text), do: {text, []}

  def apply(patches, text, opts \\ []) do
    opts = Options.valid_options!(opts)
    patch_margin = Keyword.fetch!(opts, :patch_margin)
    match_max_bits = Keyword.fetch!(opts, :match_max_bits)
    {patches, null_padding} = add_padding(patches, patch_margin)
    text = null_padding <> text <> null_padding

    patches =
      split_max(
        patches,
        patch_margin,
        match_max_bits
      )

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

    {start_loc, end_loc} = find_expected_match(text, text1, expected_loc, opts)

    if start_loc == -1 do
      # No match found.  :(
      # Subtract the delta for this failed patch from subsequent patches.
      delta = delta - (patch.length2 - patch.length1)
      {[false | results], text, delta, opts}
    else
      # Found a match.  :)
      text2 =
        if end_loc == -1 do
          substring(text, start_loc, start_loc + String.length(text1))
        else
          match_max_bits = Keyword.fetch!(opts, :match_max_bits)
          substring(text, start_loc, end_loc + match_max_bits)
        end

      {found, text} = apply_at_match(patch, text, text1, text2, start_loc, opts)
      delta = start_loc - expected_loc
      {[found | results], text, delta, opts}
    end
  end

  defp find_expected_match(text, text1, expected_loc, opts) do
    text1_length = String.length(text1)
    match_max_bits = Keyword.fetch!(opts, :match_max_bits)

    if text1_length > match_max_bits do
      # split_max will only provide an oversized pattern in the case of
      # a monster delete.
      start_loc =
        Match.main(
          text,
          substring(text1, 0, match_max_bits),
          expected_loc,
          opts
        )

      if start_loc == -1 do
        {-1, -1}
      else
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
      end
    else
      {Match.main(text, text1, expected_loc, opts), -1}
    end
  end

  defp apply_at_match(patch, text, text1, text2, start_loc, _opts) when text1 == text2 do
    # Perfect match, just shove the replacement text in.
    {true,
     substring(text, 0, start_loc) <>
       Diff.text2(patch.diffs) <>
       substring(text, start_loc + String.length(text1))}
  end

  defp apply_at_match(patch, text, text1, text2, start_loc, opts) do
    # Imperfect match.  Run a diff to get a framework of equivalent indices.
    diffs = Diff.main_(text1, text2, false, opts)

    if bad_match?(diffs, text1, opts) do
      # The end points match, but the content is unacceptably bad.
      {false, text}
    else
      diffs = Diff.cleanup_semantic_lossless(diffs)

      {acc_text, _index1} =
        Enum.reduce(patch.diffs, {text, 0}, fn diff, {acc_text, index1} ->
          apply_match_diff(diff, acc_text, index1, diffs, start_loc)
        end)

      {true, acc_text}
    end
  end

  # Returns true if the end points match, but the content is unacceptably bad.
  def bad_match?(diffs, text1, opts) do
    text1_length = String.length(text1)

    if text1_length > Keyword.fetch!(opts, :match_max_bits) do
      normalized_lev = Diff.levenshtein(diffs) / text1_length
      normalized_lev > Keyword.fetch!(opts, :patch_delete_threshold)
    else
      false
    end
  end

  def apply_match_diff({op, first_text}, acc_text, index1, diffs, start_loc) do
    dtext_length = String.length(first_text)

    case op do
      :equal ->
        {acc_text, index1 + dtext_length}

      :insert ->
        # Insertion
        index2 = Diff.x_index(diffs, index1)

        acc_text =
          substring(acc_text, 0, start_loc + index2) <>
            first_text <>
            substring(acc_text, start_loc + index2)

        {acc_text, index1 + dtext_length}

      :delete ->
        # Deletion
        index2 = Diff.x_index(diffs, index1)
        index3 = Diff.x_index(diffs, index1 + dtext_length)

        acc_text =
          substring(acc_text, 0, start_loc + index2) <>
            substring(acc_text, start_loc + index3)

        {acc_text, index1}

      _ ->
        raise RuntimeError, "Invalid operation #{inspect(op)}"
    end
  end

  @doc """
  Add some padding on text start and end so that edges can match something.

  Intended to be called only from within `Patch.apply`.

    * `patches` - A patchlist..
    * `patch_margin` - Chunk size for context length.

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

    # Separate first and last patch
    {first_patch, mid_patches, last_patch} = split_patches(patches)

    # Add padding before and after
    first_patch = pad_first_patch(first_patch, null_padding)
    # Sometimes the first patch and the last patch are the same
    if is_nil(last_patch) do
      first_patch = pad_last_patch(first_patch, null_padding)
      {[first_patch], null_padding}
    else
      # Recombine
      last_patch = pad_last_patch(last_patch, null_padding)
      {[first_patch | mid_patches] ++ [last_patch], null_padding}
    end
  end

  # Separate first and last patch
  defp split_patches([one_patch]), do: {one_patch, [], nil}
  defp split_patches([first_patch, last_patch]), do: {first_patch, [], last_patch}

  defp split_patches([first_patch | mid_patches]) do
    {last_patch, mid_patches} = List.pop_at(mid_patches, -1)
    {first_patch, mid_patches, last_patch}
  end

  # Add some padding on start of first diff.
  defp pad_first_patch(first_patch, null_padding) do
    padding_length = String.length(null_padding)
    first_diff = List.first(first_patch.diffs)

    {op, text} = Diff.undiff(first_diff)

    if op == :equal do
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
    else
      # :nil, :insert, or :delete
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
    end
  end

  # Add some padding on end of last diff.
  defp pad_last_patch(last_patch, null_padding) do
    padding_length = String.length(null_padding)
    last_diff = List.last(last_patch.diffs)

    {op, text} = Diff.undiff(last_diff)

    if op == :equal do
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
    else
      # :nil, :insert, or :delete
      # Add null_padding equality.
      %Patch{
        last_patch
        | diffs: last_patch.diffs ++ [{:equal, null_padding}],
          length1: last_patch.length1 + padding_length,
          length2: last_patch.length2 + padding_length
      }
    end
  end

  @doc """
  Look through the patches and break up any which are longer than the
  maximum limit of the match algorithm.

  Intended to be called only from within `Patch.apply`.

    * `patches` - A patchlist.
    * `patch_margin` - Chunk size for context length.
    * `match_max_bits` - The number of bits in an int (default 32).

  Returns the updated patchlist.
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
          {"", bigpatch.start1, bigpatch.start2, patch_margin, patch_size}
        )
      else
        Cursor.move_forward(patches)
      end

    split_max_loop(patches, patch_margin, patch_size)
  end

  defp create_subpatches(patches, [], _acc), do: patches

  defp create_subpatches(
         patches,
         bigpatch_diffs,
         {precontext, start1, start2, patch_margin, patch_size}
       ) do
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

    {bigpatch_diffs, patch, {start1, start2, empty, _patch_margin, _patch_size}} =
      subpatch_loop(bigpatch_diffs, patch, {start1, start2, true, patch_margin, patch_size})

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
        {op, text} = Diff.undiff(last_diff)

        patch_diffs =
          if op == :equal do
            Enum.drop(patch.diffs, -1) ++ [{:equal, text <> postcontext}]
          else
            # :nil, :insert, or :delete
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
      if empty do
        patches
      else
        patches
        |> Cursor.move_forward()
        |> Cursor.insert_before([patch])
      end

    # Call again, without advancing
    create_subpatches(
      patches,
      bigpatch_diffs,
      {precontext, start1, start2, patch_margin, patch_size}
    )
  end

  def subpatch_loop([], patch, acc), do: {[], patch, acc}

  def subpatch_loop(
        bigpatch_diffs,
        %Patch{length1: length1} = patch,
        {_start1, _start2, _empty, patch_margin, patch_size} = acc
      )
      when length1 >= patch_size - patch_margin do
    {bigpatch_diffs, patch, acc}
  end

  def subpatch_loop([first_diff | rest], patch, acc) do
    {bigpatch_diffs, patch, acc} = add_diff_to_subpatch(first_diff, rest, patch, acc)
    subpatch_loop(bigpatch_diffs, patch, acc)
  end

  def add_diff_to_subpatch(
        {:insert, first_text} = first_diff,
        rest,
        patch,
        {start1, start2, _empty, patch_margin, patch_size}
      ) do
    # Insertions are harmless.
    text_length = String.length(first_text)

    patch = %Patch{
      patch
      | diffs: patch.diffs ++ [first_diff],
        length2: patch.length2 + text_length
    }

    {rest, patch, {start1, start2 + text_length, false, patch_margin, patch_size}}
  end

  # If the patch is just one equality
  # and we have a large deletion...
  def add_diff_to_subpatch(
        {:delete, first_text} = first_diff,
        rest,
        %Patch{diffs: [{:equal, _}]} = patch,
        {start1, start2, _empty, patch_margin, patch_size} = acc
      ) do
    text_length = String.length(first_text)

    if text_length > 2 * patch_size do
      # This is a large deletion.  Let it pass in one chunk.
      patch = %Patch{
        patch
        | diffs: patch.diffs ++ [first_diff],
          length1: patch.length1 + text_length
      }

      {rest, patch, {start1 + text_length, start2, false, patch_margin, patch_size}}
    else
      # Handle same as :equal
      add_other_diff_to_subpatch(first_diff, rest, patch, acc)
    end
  end

  # Equality, or deletion when patch is not a single equality
  def add_diff_to_subpatch(first_diff, rest, patch, acc) do
    add_other_diff_to_subpatch(first_diff, rest, patch, acc)
  end

  # Equality, or small deletion
  def add_other_diff_to_subpatch(
        {first_op, first_text},
        rest,
        patch,
        {start1, start2, empty, patch_margin, patch_size}
      ) do
    # Deletion or equality.  Only take as much as we can stomach.
    diff_text =
      substring(
        first_text,
        0,
        patch_size - patch.length1 - patch_margin
      )

    text_length = String.length(diff_text)
    patch = %Patch{patch | length1: patch.length1 + text_length}
    start1 = start1 + text_length

    {patch, start2, empty} =
      if first_op == :equal do
        {%Patch{patch | length2: patch.length2 + text_length}, start2 + text_length, empty}
      else
        {patch, start2, false}
      end

    patch = %Patch{patch | diffs: patch.diffs ++ [{first_op, diff_text}]}

    bigpatch_diffs =
      if diff_text == first_text do
        rest
      else
        first_text = substring(first_text, text_length)
        [{first_op, first_text} | rest]
      end

    {bigpatch_diffs, patch, {start1, start2, empty, patch_margin, patch_size}}
  end

  @doc """
  Return the textual representation of a patchlist.
  """
  def to_text(patches) do
    Enum.reduce(patches, "", fn patch, acc -> acc <> to_string(patch) end)
  end

  @header_regex ~r/^@@ -(\d+),?(\d*) \+(\d+),?(\d*) @@$/

  @doc """
  Parse a textual representation of patches and return a patchlist.

  Raises an `ArgumentError` if the text has invalid contents.
  """
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
  defp parse_patch_header(%Cursor{current: line} = lines, patches) do
    case line do
      nil ->
        patches

      "" ->
        Cursor.move_forward(lines)
        |> parse_patch_header(patches)

      _ ->
        case Regex.run(@header_regex, line) do
          nil ->
            raise ArgumentError, "Invalid patch header: #{line}"

          groups ->
            {start1, length1} =
              parse_start_length(Enum.at(groups, 1), Enum.at(groups, 2), line, "1")

            {start2, length2} =
              parse_start_length(Enum.at(groups, 3), Enum.at(groups, 4), line, "2")

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

  defp parse_start_length(s1, s2, line, suffix) do
    start =
      case Integer.parse(s1) do
        {start, ""} -> start
        _ -> raise ArgumentError, "Invalid patch header (start#{suffix}): #{line}"
      end

    case s2 do
      "" ->
        {start - 1, 1}

      "0" ->
        {start, 0}

      _ ->
        case Integer.parse(s2) do
          {length, ""} -> {start - 1, length}
          _ -> raise ArgumentError, "Invalid patch header (length#{suffix}): #{line}"
        end
    end
  end

  @spec parse_patch_body(Cursor.t(), Diff.difflist()) :: {Cursor.t(), Diff.difflist()}
  defp parse_patch_body(%Cursor{current: nil} = lines, diffs), do: {lines, diffs}

  defp parse_patch_body(%Cursor{current: ""} = lines, diffs) do
    Cursor.move_forward(lines)
    |> parse_patch_body(diffs)
  end

  defp parse_patch_body(%Cursor{current: line} = lines, diffs) do
    diff = parse_patch_diff(line)

    if is_nil(diff) do
      {lines, diffs}
    else
      Cursor.move_forward(lines)
      |> parse_patch_body([diff | diffs])
    end
  end

  defp parse_patch_diff(line) do
    {sign, text} = String.split_at(line, 1)
    # Decode would change all "+" to " "
    text = text |> String.replace("+", "%2B") |> URI.decode()

    case sign do
      "-" ->
        # Deletion.
        {:delete, text}

      "+" ->
        # Insertion.
        {:insert, text}

      " " ->
        # Minor equality.
        {:equal, text}

      "@" ->
        nil

      _ ->
        # WTF?
        raise ArgumentError, "Invalid patch mode '#{sign}' in: #{line}"
    end
  end
end
