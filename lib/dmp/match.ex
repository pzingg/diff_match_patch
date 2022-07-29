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

    # Iterate over possible error levels
    {best_loc, _score_threshold, _rd, _bin_max} =
      Enum.reduce_while(
        0..(pattern_length - 1),
        {best_loc, score_threshold, %{}, text_length + pattern_length},
        fn d, acc -> best_match_at_error_level(d, acc, constants) end
      )

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
  # `last_rd` - Sparse match array from previous error level.
  # `bin_max` - The highest index (from 0 to `text_length + pattern_length`)
  defp best_match_at_error_level(
         d,
         {best_loc, score_threshold, last_rd, bin_max},
         {text, loc, s, matchmask, text_length, pattern_length, match_distance}
       ) do
    constants_1 = {d, loc, pattern_length, score_threshold, match_distance}

    bin_max =
      bin_mid =
      binary_search_scores(
        0,
        bin_max,
        bin_max,
        constants_1
      )

    finish = min(loc + bin_mid, text_length) + pattern_length
    start = max(1, loc - bin_mid + 1)
    rd = %{(finish + 1) => (1 <<< d) - 1}
    acc_2 = {best_loc, score_threshold, rd, start}

    constants_2 = {d, text, loc, last_rd, s, matchmask, pattern_length, match_distance}

    {best_loc, score_threshold, rd, _start} =
      Enum.reduce_while(finish..0//-1, acc_2, fn j, acc ->
        update_error_level_scores(j, acc, constants_2)
      end)

    # One last time
    score = bitap_score(d + 1, loc, loc, pattern_length, match_distance)

    if score > score_threshold do
      # No hope for a (better) match at greater error levels.
      {:halt, {best_loc, score_threshold, rd, bin_max}}
    else
      {:cont, {best_loc, score_threshold, rd, bin_max}}
    end
  end

  # Use a recursive binary search to find a partition of
  # locations that meet the score threshold.
  # Return the middle location of the search.
  defp binary_search_scores(bin_min, bin_mid, _bin_max, _)
       when bin_min >= bin_mid do
    # Done
    bin_mid
  end

  defp binary_search_scores(
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

    binary_search_scores(
      bin_min,
      bin_mid,
      bin_max,
      {d, loc, pattern_length, score_threshold, match_distance}
    )
  end

  defp update_error_level_scores(j, {_best_loc, _score_threshold, _rd, start} = acc, _)
       when j < start do
    # Exceeded our current distance from loc. Done.
    {:halt, acc}
  end

  defp update_error_level_scores(
         j,
         {best_loc, score_threshold, rd, start},
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
      update_score_at(
        d,
        j,
        loc,
        {best_loc, score_threshold, rd, start},
        pattern_length,
        match_distance
      )
    else
      {:cont, {best_loc, score_threshold, rd, start}}
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

  defp update_score_at(
         d,
         j,
         loc,
         {best_loc, score_threshold, rd, start},
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

        {:cont, {best_loc, score, rd, start}}
      else
        # Already passed loc, downhill from here on in.

        {:halt, {best_loc, score, rd, start}}
      end
    else
      {:cont, {best_loc, score_threshold, rd, start}}
    end
  end

  @doc """
  Compute and return the score for a match with e errors and x location.

  `e` - Number of errors in match.
  `x` - Location of match.
  `loc` - Expected location of match.
  `pattern_length` - Length of pattern being sought.
  `match_distance` - How far to search for a match (0 = exact location, 1000+ = broad match).

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
