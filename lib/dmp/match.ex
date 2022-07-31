defmodule Dmp.Match do
  @moduledoc """
  Given a search string, find its best fuzzy match in a block of plain text.
  Weighted for both accuracy and location.
  """

  use Bitwise, only_operators: true

  import Dmp.StringUtils

  alias Dmp.Options

  @type options() :: Options.t()

  @doc """
  Locate the best instance of `pattern` in `text` near `loc`.

    * `text` - The text to search.
    * `pattern` - The pattern to search for.
    * `loc` - The location to search around.
    * `opts` - An `Options` struct, or `nil` to use default options.

  Returns -1 if no match found.
  """
  @spec main(String.t(), String.t(), non_neg_integer(), nil | options()) :: integer()
  def main(text, pattern, loc, opts \\ nil) do
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
        bitap(text, pattern, loc, opts.match_threshold, opts.match_distance)
    end
  end

  @doc """
  Locate the best instance of `pattern` in `text` near `loc` using the Bitap algorithm.

    * `text` - The text to search.
    * `pattern` - The pattern to search for.
    * `loc` - The location to search around.
    * `match_threshold` - At what point is no match declared
      (0.0 = perfection, 1.0 = very loose, default = 0.5).
    * `match_distance` - How far to search for a match (0 = exact location,
      1000+ = broad match, default = 1000).

  Returns best match index or -1.
  """
  @spec bitap(String.t(), String.t(), non_neg_integer(), float(), non_neg_integer()) :: integer()
  def bitap(text, pattern, loc, match_threshold \\ 0.5, match_distance \\ 1000) do
    pattern_length = String.length(pattern)

    # `best_loc` - Is there a nearby exact match? (speedup), -1 if not found.
    # `score_threshold` - Highest score beyond which we give up.
    {best_loc, score_threshold} =
      initial_threshold(match_threshold, text, pattern, loc, match_distance)

    # Initialise the alphabet.
    s = alphabet(pattern)

    # Initialise the bit arrays.
    matchmask = 1 <<< (pattern_length - 1)
    text_length = String.length(text)
    constants = {text, loc, s, matchmask, text_length, pattern_length, match_distance}

    # Start with `max_distance = text_length + pattern_length`
    # and the `rd` array all zeros.
    acc = {best_loc, score_threshold, text_length + pattern_length, %{}}

    # Iterate over possible error levels
    {best_loc, _score_threshold, _max_distance, _rd} =
      Enum.reduce_while(0..(pattern_length - 1), acc, fn d, acc ->
        best_match_at_error_level(d, acc, constants)
      end)

    best_loc
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

          best_loc_2 ->
            # Second  match of pattern in text
            score_2 = bitap_score(0, best_loc_2, loc, pattern_length, match_distance)

            if score_2 < score do
              {best_loc_2, score_2}
            else
              {best_loc, score}
            end
        end
    end
  end

  # Scan for the best match; each iteration allows for one more error.
  # Run a binary search to determine how far from 'loc' we can stray at
  # this error level.
  # Use the result from this iteration as the maximum for the next.
  #
  # `d` - Error level being scanned.
  # `best_loc` - The best location found.
  # `score_threshold` - The threshold for qualifying matches.
  # `max_distance` - The highest distance from `loc` to seach within.
  #   Starts at `text_length + pattern_length` and gets smaller.
  # `last_rd` - Sparse array from previous error level.
  defp best_match_at_error_level(
         d,
         {best_loc, score_threshold, max_distance, last_rd},
         {text, loc, s, matchmask, text_length, pattern_length, match_distance}
       ) do
    constants_1 = {d, loc, pattern_length, score_threshold, match_distance}

    distance =
      best_distance(
        0,
        max_distance,
        max_distance,
        constants_1
      )

    finish = min(loc + distance, text_length) + pattern_length
    start = max(1, loc - distance + 1)

    # `rd` is a sparse array of integers of capacity `finish + 2`
    rd = %{(finish + 1) => (1 <<< d) - 1}
    acc_2 = {best_loc, score_threshold, start, rd}

    constants_2 = {d, text, loc, last_rd, s, matchmask, pattern_length, match_distance}

    {best_loc, score_threshold, _start, rd} =
      Enum.reduce_while(finish..0//-1, acc_2, fn j, acc ->
        bitap_update(j, acc, constants_2)
      end)

    # One last time
    score = bitap_score(d + 1, loc, loc, pattern_length, match_distance)

    if score > score_threshold do
      # No hope for a (better) match at greater error levels.
      {:halt, {best_loc, score_threshold, distance, rd}}
    else
      {:cont, {best_loc, score_threshold, distance, rd}}
    end
  end

  # Use a recursive binary search to find the
  # location with the lowest bitap score within the
  # range `loc + bin_min` to `loc + bin_max`.
  #
  # `bin_min` - Lowest distance from `loc`
  # `bin_max` - Highest distance from `loc`
  # `bin_mid` - Midpoint between `bin_min` and `bin_max`
  #
  # Returns `bin_mid`, where `loc + bin_mid` has
  # the lowest bitap score.
  defp best_distance(bin_min, bin_mid, _bin_max, _)
       when bin_min >= bin_mid do
    # Done
    bin_mid
  end

  defp best_distance(
         bin_min,
         bin_mid,
         bin_max,
         {d, loc, pattern_length, score_threshold, match_distance}
       ) do
    # Loop
    {bin_min, bin_max} =
      if bitap_score(d, loc + bin_mid, loc, pattern_length, match_distance) <= score_threshold do
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

  # This is the heart of the bitap algorithm
  # Updates the bit array `rd` at the index `j` (representing the
  # location `j - 1` in `text`, and then tests for a match.
  # If a match is found
  defp bitap_update(j, {_best_loc, _score_threshold, start, _rd} = acc, _)
       when j < start do
    # Exceeded our current distance from loc. Done.
    {:halt, acc}
  end

  defp bitap_update(
         j,
         {best_loc, score_threshold, start, rd},
         {d, text, loc, last_rd, s, matchmask, pattern_length, match_distance}
       ) do
    char_match = char_bitmask_at(s, text, j - 1)
    rd_j_1 = (Map.get(rd, j + 1, 0) <<< 1 ||| 1) &&& char_match

    rd_j =
      if d == 0 do
        # First pass: exact match.
        rd_j_1
      else
        # Subsequent passes: fuzzy match.
        last_rd_j_1 = Map.get(last_rd, j + 1, 0)
        last_rd_j = Map.get(last_rd, j, 0) ||| last_rd_j_1
        rd_j_1 ||| (last_rd_j <<< 1 ||| 1) ||| last_rd_j_1
      end

    # Update mask array
    rd = Map.put(rd, j, rd_j)

    if (rd_j &&& matchmask) != 0 do
      # Found a match
      best_loc_at_error_level(
        d,
        j,
        loc,
        {best_loc, score_threshold, start, rd},
        pattern_length,
        match_distance
      )
    else
      {:cont, {best_loc, score_threshold, start, rd}}
    end
  end

  defp char_bitmask_at(s, text, index) do
    case String.at(text, index) do
      nil ->
        0

      "" ->
        0

      ch ->
        ord = String.to_charlist(ch) |> List.first()
        Map.get(s, ord, 0)
    end
  end

  defp best_loc_at_error_level(
         d,
         j,
         loc,
         {best_loc, score_threshold, start, rd},
         pattern_length,
         match_distance
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

        {:cont, {best_loc, score, start, rd}}
      else
        # Already passed loc, downhill from here on in.

        {:halt, {best_loc, score, start, rd}}
      end
    else
      {:cont, {best_loc, score_threshold, start, rd}}
    end
  end

  @doc """
  Compute and return the score for a match with e errors and x location.

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

  Returns map of character locations.
  """
  @spec alphabet(String.t()) :: %{integer() => integer()}
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
end
