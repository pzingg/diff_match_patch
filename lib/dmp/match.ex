defmodule Dmp.Match do
  @moduledoc """
  MATCH FUNCTIONS
  """

  use Bitwise, only_operators: true

  import Dmp.StringUtils

  alias Dmp.{Cursor, Options}

  @type options() :: Options.t()

  @doc """
  Locate the best instance of 'pattern' in 'text' near 'loc'.
  Returns -1 if no match found.

  `text` The text to search.
  `pattern` The pattern to search for.
  `loc` The location to search around.
  `opts` - A `DiffMatchPatch.Options` struct, or `nil` to use the
    defaults.
  """
  @spec main(String.t(), String.t(), non_neg_integer(), nil | options()) :: integer()
  def main(text, pattern, loc, opts \\ nil) do
    opts = opts || Options.default()
    match_threshold = Map.fetch!(opts, :match_threshold)
    match_distance = Map.fetch!(opts, :match_distance)

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
        bitap(text, pattern, loc, match_threshold, match_distance)
    end
  end

  @doc """
  Locate the best instance of 'pattern' in 'text' near 'loc' using the
  #Bitap algorithm.  Returns -1 if no match found.

  `text` The text to search.
  `pattern` The pattern to search for.
  `loc` The location to search around.
  `match_threshold` - At what point is no match declared (0.0 = perfection, 1.0 = very loose).
  `match_distance` - How far to search for a match (0 = exact location, 1000+ = broad match).

  Returns best match index or -1.
  """
  @spec bitap(String.t(), String.t(), non_neg_integer(), float(), non_neg_integer()) :: integer()
  def bitap(text, pattern, loc, match_threshold, match_distance) do
    text_length = String.length(text)
    pattern_length = String.length(pattern)

    # Initialise the alphabet.
    s = alphabet(pattern)

    # Highest score beyond which we give up.
    # Is there a nearby exact match? (speedup)
    best_loc = index_of(text, pattern, loc)

    score_threshold =
      case best_loc do
        -1 ->
          match_threshold

        best_loc_1 ->
          score_1 =
            min(bitap_score(0, best_loc_1, loc, pattern_length, match_distance), match_threshold)

          # What about in the other direction? (speedup)
          case last_index_of(text, pattern, loc + pattern_length) do
            -1 ->
              score_1

            best_loc_2 ->
              min(bitap_score(0, best_loc_2, loc, pattern_length, match_distance), score_1)
          end
      end

    # Initialise the bit arrays.
    matchmask = 1 <<< (pattern_length - 1)

    Enum.reduce_while(
      0..(pattern_length - 1),
      {-1, %{}, text_length + pattern_length, score_threshold},
      fn d, {best_loc, last_rd, bin_max, score_threshold} ->
        # Scan for the best match; each iteration allows for one more error.
        # Run a binary search to determine how far from 'loc' we can stray at
        # this error level.
        # Use the result from this iteration as the maximum for the next.
        bin_max =
          bin_mid =
          bin_score(0, bin_max, bin_max, d, loc, pattern_length, score_threshold, match_distance)

        finish = min(loc + bin_mid, text_length) + pattern_length
        start = max(1, loc - bin_mid + 1)
        rd_0 = %{(finish + 1) => 1 <<< (d - 1)}

        {best_loc, rd, score_threshold} =
          Enum.reduce_while(finish..0//-1, {start, best_loc, rd_0, score_threshold}, fn j,
                                                                                        {start,
                                                                                         best_loc,
                                                                                         rd,
                                                                                         score_threshold} ->
            if j < start do
              {:halt, {rd, score_threshold}}
            else
              ch = String.at(text, j - 1)
              char_match = Map.get(s, ch, 0)

              rd_j =
                if d == 0 do
                  # First pass: exact match.
                  (Map.get(rd, j + 1, 0) ||| 1) &&& char_match
                else
                  # Subsequent passes: fuzzy match.
                  ((Map.get(rd, j + 1) <<< 1 ||| 1) &&& char_match) |||
                    ((Map.get(last_rd, j + 1) ||| Map.get(last_rd, j)) <<< 1 ||| 1) |||
                    Map.get(last_rd, j + 1)
                end

              rd = Map.put(rd, j, rd_j)

              if (rd_j &&& matchmask) != 0 do
                score = bitap_score(d, j - 1, loc, pattern_length, match_distance)
                # This match will almost certainly be better than any existing
                # match.  But check anyway.
                if score <= score_threshold do
                  # Told you so.
                  best_loc = j - 1

                  if best_loc > loc do
                    # When passing loc, don't exceed our current distance from loc.
                    start = max(1, 2 * loc - best_loc)
                    {:cont, {start, best_loc, rd, score}}
                  else
                    # Already passed loc, downhill from here on in.
                    {:halt, {best_loc, rd, score}}
                  end
                else
                  {:cont, {start, best_loc, rd, score_threshold}}
                end
              else
                {:cont, {start, best_loc, rd, score_threshold}}
              end
            end
          end)

        if bitap_score(d + 1, loc, loc, pattern_length, match_distance) > score_threshold do
          # No hope for a (better) match at greater error levels.
          {:halt, best_loc}
        else
          {:cont, {best_loc, rd, bin_max}}
        end
      end
    )
  end

  @spec bin_score(
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer(),
          float(),
          non_neg_integer()
        ) :: non_neg_integer()
  def bin_score(
        bin_min,
        bin_mid,
        bin_max,
        d,
        loc,
        pattern_length,
        score_threshold,
        match_distance
      ) do
    if bin_min >= bin_mid do
      # Done
      bin_mid
    else
      # Loop
      {bin_min, bin_max} =
        if bitap_score(d, loc + bin_mid, loc, pattern_length, match_distance) <= score_threshold do
          {bin_mid, bin_max}
        else
          {bin_min, bin_mid}
        end

      bin_mid = div(bin_max - bin_min, 2) + bin_min

      bin_score(
        bin_min,
        bin_mid,
        bin_max,
        d,
        loc,
        pattern_length,
        score_threshold,
        match_distance
      )
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
