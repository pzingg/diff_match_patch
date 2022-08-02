defmodule Dmp.Match do
  @moduledoc """
  Given a search string, find its best fuzzy match in a block of plain text.
  Weighted for both accuracy and location.
  """

  use Bitwise, only_operators: true

  import Dmp.StringUtils
  import Dmp.DebugUtils

  alias Dmp.Options

  @type options() :: Options.t()

  @typedoc """
  A bitarray encoding the locations of characters within the search pattern.

  The index is the ordinal value of the character.
  """
  @type alpha() :: %{non_neg_integer() => non_neg_integer()}

  @typedoc """
  A bitarray encoding possible match sequences of the search pattern within the text.

  The index is the location of the text, plus one.
  """
  @type bitap_array() :: %{integer() => non_neg_integer()}

  @doc """
  Locate the best instance of `pattern` in `text` near `loc`.

    * `text` - The text to search.
    * `pattern` - The pattern to search for.
    * `loc` - The location to search around.
    * `opts` - A options keyword list, `[]` to use the default options.

  Returns -1 if no match found.
  """
  @spec main(String.t(), String.t(), non_neg_integer(), options()) :: integer()
  def main(text, pattern, loc, opts \\ []) do
    opts = Options.valid_options!(opts)

    text_length = String.length(text)
    pattern_length = String.length(pattern)
    loc = max(0, min(loc, text_length))

    cond do
      text == pattern ->
        # Shortcut (potentially not guaranteed by the algorithm)
        0

      text == "" ->
        # Nothing to match.
        -1

      loc + pattern_length <= text_length &&
          substring(text, loc, loc + pattern_length) == pattern ->
        # Perfect match at the perfect spot!  (Includes case of null pattern)
        loc

      true ->
        # Do a fuzzy compare.
        bitap(
          text,
          pattern,
          loc,
          Keyword.fetch!(opts, :match_threshold),
          Keyword.fetch!(opts, :match_distance)
        )
    end
  end

  @doc """
  Search for the best instance of `pattern` in `text` near `loc`, with errors,
  using the Bitap algorithm.

  See: [Fast Text Searching with Errors (Wu and Manber, 1991)](http://www.club.cc.cmu.edu/~ajo/docs/agrep.pdf).

    * `text` - The text to search.
    * `pattern` - The pattern to search for.
    * `loc` - The location to search around.
    * `match_threshold` - At what point is no match declared
      (0.0 = perfection, 1.0 = very loose, default = 0.5).
    * `match_distance` - How far to search for a match (0 = exact location,
      1000+ = broad match, default = 1000).
    * `return_score` - Control what is returned. See the following.

  If `return_score` is false (the default), returns the index of the best
    match closest to `loc` in `text`, or -1 if no suitable match was found.

  If `return_score` is true, returns a tuple with two elements:
    * The best match index, or -1.
    * The adjusted match score at the returned index (a value less than
      or equal to the `match_threshold`). If no match was found,
      the score is 1.
  """
  @spec bitap(String.t(), String.t(), non_neg_integer(), float(), non_neg_integer(), boolean()) ::
          integer()
  def bitap(
        text,
        pattern,
        loc,
        match_threshold \\ 0.5,
        match_distance \\ 1000,
        return_score \\ false
      ) do
    pattern_length = String.length(pattern)

    # `best_loc` - Is there a nearby exact match? (speedup), -1 if not found.
    # `score_threshold` - Highest score beyond which we give up.
    {best_loc, score_threshold} =
      initial_threshold(match_threshold, text, pattern, loc, match_distance)

    # Initialise the alphabet.
    s = alphabet(pattern)

    # Initialise the bit arrays.
    match_mask = 1 <<< (pattern_length - 1)
    overflow_mask = (1 <<< pattern_length) - 1
    text_length = String.length(text)

    constants =
      {text, pattern, loc, s, match_mask, overflow_mask, text_length, pattern_length,
       match_distance}

    # Uncomment to see the bitarray
    # debug_alphabet(pattern, s) |> Enum.join("\n") |> IO.puts

    # Start with `max_distance = text_length + pattern_length`
    # and the $$R_j^0$$ array all zero (empty map).
    max_distance = text_length + pattern_length
    rd_0 = %{}
    acc = {best_loc, score_threshold, max_distance, rd_0}

    # Iterate over possible error levels
    {best_loc, score, _max_distance, _rd} =
      Enum.reduce_while(0..(pattern_length - 1), acc, fn d, acc ->
        search_at_error_level(d, acc, constants)
      end)

    if return_score do
      if best_loc == -1 do
        {-1, 1.0}
      else
        {best_loc, score}
      end
    else
      best_loc
    end
  end

  # Set a starting point if an exact match can be found,
  # and lower the threshold to the score at the starting point.
  defp initial_threshold(match_threshold, text, pattern, loc, match_distance) do
    best_loc = index_of(text, pattern, loc)

    case best_loc do
      -1 ->
        {-1, match_threshold}

      _ ->
        # First match of pattern in text
        pattern_length = String.length(pattern)
        score = bitap_score(0, best_loc, loc, pattern_length, match_distance)
        score = min(score, match_threshold)

        # What about in the other direction? (speedup)
        case last_index_of(text, pattern, loc + pattern_length) do
          -1 ->
            {best_loc, score}

          best_loc2 ->
            # Second  match of pattern in text
            score2 = bitap_score(0, best_loc2, loc, pattern_length, match_distance)

            if score2 < score do
              {best_loc2, score2}
            else
              {best_loc, score}
            end
        end
    end
  end

  # Scan for the best match of the pattern in the source text with `d` errors
  # of insertion, deletion, or substitution.
  #
  # Each iteration allows for one more error.
  #
  # At each error level:
  #   1. Run a binary search to determine how far from 'loc' we can stray at
  #      this error level (`best_distance`)
  #   2. Update the bitarray $$R_j^d$$ for this error level and check
  #      for a pattern match.  Return the location of the best match.
  #   3. Check to see if there might be a better score at a higher error level.
  #
  # Use the result from this iteration as the maximum for the next.
  #
  # `d` - Error level being scanned.
  # `best_loc` - The best location found.
  # `score_threshold` - The threshold for qualifying matches. Gets smaller
  #   after each iteration.
  # `max_distance` - The highest distance from `loc` to search within.
  #   Starts at `text_length + pattern_length` and gets smaller after
  #   each iteration.
  # `last_rd` - $$R_j^{d-1}$$ in the Wu and Manber paper. Bitarray from the
  #   previous `d-1` error level.
  # `text_length - $$n$$ in the Wu and Manber paper.
  # `pattern_length` - $$m$$ in the Wu and Manber paper.
  # `match_mask` - Bitmask to test the value of $$R_{j+1}[m]$$.
  #
  # Returns updated `best_loc`, `score_threshold`, and `max_distance`,
  # and the calculated $$R_j^d$$ bitarray from this level.
  defp search_at_error_level(
         d,
         {best_loc, score_threshold, max_distance, last_rd},
         {text, _pattern, loc, s, match_mask, overflow_mask, text_length, pattern_length,
          match_distance}
       ) do
    distance =
      best_distance(
        0,
        max_distance,
        max_distance,
        {d, loc, pattern_length, score_threshold, match_distance}
      )

    start = max(1, loc - distance + 1)
    finish = min(loc + distance, text_length) + pattern_length

    # `rd` is a sparse array of integers of capacity `finish + 2`
    # for debugging we store the "size" of the array at index -1
    rd = %{(finish + 1) => (1 <<< d) - 1, -1 => finish + 2}
    acc2 = {best_loc, score_threshold, start, rd}

    constants2 =
      {d, text, loc, last_rd, s, match_mask, overflow_mask, pattern_length, match_distance}

    {best_loc, score_threshold, _j, rd} =
      Enum.reduce_while(finish..0//-1, acc2, fn j, acc ->
        bitap_update(j, acc, constants2)
      end)

    # See if the threshold for match at level `d + 1` is lower than the best
    # score we found at this level, in which case we have to continue to
    # that level.
    d1_score = bitap_score(d + 1, loc, loc, pattern_length, match_distance)

    # Uncomment to see the bitarray
    # debug_rd(text, pattern, d, rd, j, best_loc) |> Enum.join("\n") |> IO.puts

    if d1_score > score_threshold do
      # No hope for a (better) match at greater error levels.
      {:halt, {best_loc, score_threshold, distance, rd}}
    else
      {:cont, {best_loc, score_threshold, distance, rd}}
    end
  end

  # Perform a binary search to find the location with the lowest bitap score within the
  # range `loc + bin_min` to `loc + bin_max`.
  #
  # `bin_min` - Lowest distance from `loc`
  # `bin_max` - Highest distance from `loc`
  # `bin_mid` - Midpoint between `bin_min` and `bin_max`
  #
  # Returns `bin_mid`, where `loc + bin_mid` has the lowest bitap score.
  defp best_distance(bin_min, bin_mid, _, _) when bin_min >= bin_mid, do: bin_mid

  defp best_distance(
         bin_min,
         bin_mid,
         bin_max,
         {d, loc, pattern_length, score_threshold, match_distance}
       ) do
    score = bitap_score(d, loc + bin_mid, loc, pattern_length, match_distance)

    {bin_min, bin_max} =
      if score <= score_threshold do
        {bin_mid, bin_max}
      else
        {bin_min, bin_mid}
      end

    bin_mid = div(bin_max - bin_min, 2) + bin_min

    best_distance(
      bin_min,
      bin_mid,
      bin_max,
      {d, loc, pattern_length, score_threshold, match_distance}
    )
  end

  @typedoc "Accumulator for `bitap_update/3`."
  @type update_acc() :: {integer(), float(), non_neg_integer(), bitap_array()}
  @typep update_constants() ::
           {non_neg_integer(), String.t(), integer(), bitap_array(), alpha(), non_neg_integer(),
            non_neg_integer(), non_neg_integer(), non_neg_integer()}

  @doc """
  Perform the bitap algorithm and calculate error score if a match is found.

  * `acc` - Accumulator tuple, with `best_loc`, `score_threshold`, `start`, and `rd` elements.
  * `constants` - Other constant values needed for calculations.

  Updates the `rd` bitarray for the current error level `d` at the index `j`
  (representing the zero-based location `j - 1` in `text`), and
  then tests for a match (an exact match if `d == 0` or a match with
  `d` errors).

  If a match is found at position `j`, calculate the error score
  (based on the error level `d` and the distance from expected location).
  If the score is lower than the current threshold, stop calculating the update
  if we have already gone below the minimum possible location,
  or continue the update, limiting the range of `j` (increasing the
  `start` value).

  ## Notes

  The `j` index is decremented from the end of the text to the start of the text.
  Since the iteration is moving from high `j` to low, `bitap_update` does "Lshift"
  operations, not the "Rshift" operations in the Wu and Manber paper, and uses
  the previous values that were set at `j + 1`, not `j`.

  Here the calculations are:

  $$Rsubscptj^d = \\begin{cases}
    Lshift [ Rsubscpt{j+1}^d ] \\text{ AND } S_c &\\text{if } d = 0 \\cr
    Lshift [ Rsubscpt{j+1}^d ] \\text{ AND } S_c \\text{ OR } Lshift [ Rsubscptj^{d-1} \\text{ OR } Rsubscpt{j+1}^{d-1} ] \\text{ OR } Rsubscpt{j+1}^{d-1} &\\text{otherwise}
  \\end{cases}$$

  versus in Wu and Manber's paper:

  $$Rsubscptj^d = \\begin{cases}
    Rshift [ Rsubscpt{j}^d ] \\text{ AND } S_c &\\text{if } d = 0 \\cr
    Rshift [ Rsubscpt{j}^d ] \\text{ AND } S_c \\text{ OR } Rshift [ Rsubscptj^{d-1} \\text{ OR } Rsubscpt{j+1}^{d-1} ] \\text{ OR } Rsubscpt{j}^{d-1} &\\text{otherwise}
  \\end{cases}$$

  """
  @spec bitap_update(non_neg_integer(), update_acc(), update_constants()) ::
          {:cont | :halt, update_acc()}
  def bitap_update(j, acc, constants)

  def bitap_update(
        j,
        {best_loc, score_threshold, start, rd},
        _constants
      )
      when j < start do
    # Exceeded our current distance from loc. Done.
    # Return `j + 1` in the `start` position and break the iteration.
    {:halt, {best_loc, score_threshold, j + 1, rd}}
  end

  def bitap_update(
        j,
        {best_loc, score_threshold, start, rd},
        {d, text, loc, last_rd, s, match_mask, overflow_mask, pattern_length, match_distance}
      ) do
    # $$S_c$$
    char_match = s_c(s, String.at(text, j - 1))

    # Perform shift-OR update

    # $$Lshift[R_{j+1}^d] AND S_c$$
    shift_d_and_s_c = (Map.get(rd, j + 1, 0) <<< 1 ||| 1) &&& char_match

    rd_j =
      if d == 0 do
        # First pass: exact match.
        shift_d_and_s_c
      else
        # Subsequent passes: fuzzy match.
        # $$R_{j+1}^{d-1}$$
        rd_d1_j1 = Map.get(last_rd, j + 1, 0)
        # $$R_j^{d-1}$$
        rd_d1_j = Map.get(last_rd, j, 0)

        # Restrict shifted values to pattern_length with $$AND overflow_mask$$
        # $$Lshift[R_j^{d-1} OR R_{j+1}^{d-1}]$$
        shift_d1 = ((rd_d1_j ||| rd_d1_j1) <<< 1 ||| 1) &&& overflow_mask

        # $$Lshift[R_{j+1}^d] AND S_c OR Lshift[R_j^{d-1} OR R_{j+1}^{d-1}] OR R_{j+1}^{d-1}$$
        shift_d_and_s_c ||| shift_d1 ||| rd_d1_j1
      end

    # Update mask array
    rd = Map.put(rd, j, rd_j)

    # Test for a match: $$if Rd_j+1[m] = 1$$
    if (rd_j &&& match_mask) != 0 do
      # Found a match
      test_score_at_match(
        d,
        j,
        loc,
        {best_loc, score_threshold, start, rd},
        {pattern_length, match_distance}
      )
    else
      {:cont, {best_loc, score_threshold, start, rd}}
    end
  end

  # We found a match during `bitap_update/3`. Verify
  # that it is the best match and either stop the `rd` array update
  # if we have already passed `loc` or reduce the range of indices
  # that need to be calculated.
  defp test_score_at_match(
         d,
         j,
         loc,
         {best_loc, score_threshold, start, rd},
         {pattern_length, match_distance}
       ) do
    score = bitap_score(d, j - 1, loc, pattern_length, match_distance)
    # This match will almost certainly be better than any existing
    # match. But check anyway.
    if score <= score_threshold do
      # Told you so.
      best_loc = j - 1

      if best_loc > loc do
        # When passing loc, don't exceed our current distance from loc.
        start = max(1, 2 * loc - best_loc)
        # Return a new `start` (calculation endpoint) for the update calculations.
        {:cont, {best_loc, score, start, rd}}
      else
        # Already passed loc, downhill from here on in.
        # Return `j` in the `start` position. No more calcuations necessary.
        {:halt, {best_loc, score, j, rd}}
      end
    else
      {:cont, {best_loc, score_threshold, start, rd}}
    end
  end

  @doc """
  Compute and return a weighted score for a match with `e` errors and `x` location.

    * `e` - Number of errors in match.
    * `x` - Location of match.
    * `loc` - Expected location of match.
    * `pattern_length` - Length of pattern being sought.
    * `match_distance` - How far to search for a match (0 = exact location, 1000+ = broad match).

  Returns overall score for match (0.0 = good, 1.0 = bad).
  """
  @spec bitap_score(non_neg_integer(), integer(), non_neg_integer(), integer(), non_neg_integer()) ::
          float()
  def bitap_score(e, x, loc, pattern_length, match_distance) do
    accuracy = e / pattern_length
    proximity = abs(loc - x)

    if match_distance == 0 do
      # Dodge divide by zero error.
      if proximity == 0 do
        accuracy
      else
        1.0
      end
    else
      accuracy + proximity / match_distance
    end
  end

  @doc """
  Initialise the alphabet for the Bitap algorithm.

  `pattern` - The text to encode.

  Returns map of character locations within the pattern. $$S_c$$ in the Wu and Manber paper.
  """
  @spec alphabet(String.t()) :: alpha()
  def alphabet(pattern) when is_binary(pattern) do
    pattern_length = String.length(pattern)

    pattern
    |> String.to_charlist()
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {ch, i}, acc ->
      mask = 1 <<< (pattern_length - i - 1)
      Map.update(acc, ch, mask, fn val -> val ||| mask end)
    end)
  end

  # Look up a character in the alphabet and return its encoded bitmap.
  #
  # `s` - An alphabet constructed with `alphabet/1`.
  # `ch` - A single character string.
  #
  # Returns $$S_c$$, a bitarray of positions in the `pattern` for the
  # character `ch`.
  @spec s_c(alpha(), String.t()) :: non_neg_integer()
  defp s_c(_s, nil), do: 0
  defp s_c(_s, ""), do: 0

  defp s_c(s, ch) do
    [ord | _] = String.to_charlist(ch)
    Map.get(s, ord, 0)
  end
end
