defmodule Dmp.Options do
  @moduledoc """
  Adjustable parameters that control algorithm efficiency and accuracy.

    * `:diff_timeout` - Number of seconds to map a diff before giving up (0 for infinity).
    * `:diff_edit_cost` - Cost of an empty edit operation in terms of edit characters.
    * `:match_max_bits` - The number of bits in an integer (default is expected 32).
    * `:match_threshold` - At what point is no match declared (0.0 = perfection, 1.0 = very loose).
    * `:match_distance` - How far to search for a match (0 = exact location, 1000+ = broad match).
      A match this many characters away from the expected location will add
      1.0 to the score (0.0 is a perfect match).
    * `:patch_delete_threshold` - When deleting a large block of text (over ~64 characters), how close do
      the contents have to be to match the expected contents. (0.0 = perfection,
      1.0 = very loose).  Note that `:match_threshold` controls how closely the
      end points of a delete need to match.
    * `:patch_margin` - Chunk size for context length.
  """

  alias __MODULE__

  defstruct diff_timeout: 1.0,
            diff_edit_cost: 4,
            match_max_bits: 32,
            match_threshold: 0.5,
            match_distance: 1000,
            patch_delete_threshold: 0.5,
            patch_margin: 4

  @type option() ::
          {:diff_timeout, float()}
          | {:diff_edit_cost, non_neg_integer()}
          | {:match_max_bits, non_neg_integer()}
          | {:match_threshold, float()}
          | {:match_distance, non_neg_integer()}
          | {:patch_delete_threshold, float()}
          | {:patch_margin, non_neg_integer()}

  @type t() :: [option()]

  @doc """
  Returns an `Options` struct with good default values:

  ## Examples

      iex> Options.default()
      [
        diff_edit_cost: 4,
        diff_timeout: 1.0,
        match_distance: 1000,
        match_max_bits: 32,
        match_threshold: 0.5,
        patch_delete_threshold: 0.5,
        patch_margin: 4
      ]

  """
  @spec default() :: t()
  def default(), do: %Options{} |> Map.from_struct() |> Enum.into([])

  @doc """
  Validates an `Options` list, raising an `ArgumentError` if it contains invalid values.

  If `[]` is passed, `Options.default()` will be returned.

  ## Examples

      iex> Options.valid_options!([])
      [
        diff_edit_cost: 4,
        diff_timeout: 1.0,
        match_distance: 1000,
        match_max_bits: 32,
        match_threshold: 0.5,
        patch_delete_threshold: 0.5,
        patch_margin: 4
      ]

      iex> Options.valid_options!(match_max_bits: -1)
      ** (ArgumentError) Invalid options: match_max_bits (-1)

  """
  @spec valid_options!(t()) :: t()
  def valid_options!([]), do: default()

  # credo:disable-for-lines:21 Credo.Check.Refactor.CyclomaticComplexity
  def valid_options!(opts) when is_list(opts) do
    opts = Keyword.merge(default(), opts) |> Enum.sort()

    {_, errors} =
      {opts, []}
      |> validate_diff_timeout()
      |> validate_diff_edit_cost()
      |> validate_match_max_bits()
      |> validate_match_threshold()
      |> validate_match_distance()
      |> validate_patch_delete_threshold()
      |> validate_patch_margin()

    if errors == [] do
      opts
    else
      errors = Enum.map_join(errors, ", ", fn {name, value} -> name <> " (#{value})" end)
      raise ArgumentError, "Invalid options: #{errors}"
    end
  end

  defp valid_match_max_bits?(match_max_bits) do
    match_max_bits > 0 && match_max_bits <= 128
  end

  defp valid_threshold?(value) do
    value >= 0 and value <= 1
  end

  defp validate_diff_timeout({opts, errors}) do
    diff_timeout = Keyword.fetch!(opts, :diff_timeout)

    if diff_timeout >= 0 do
      {opts, errors}
    else
      {opts, [{"diff_timeout", diff_timeout} | errors]}
    end
  end

  defp validate_diff_edit_cost({opts, errors}) do
    diff_edit_cost = Keyword.fetch!(opts, :diff_edit_cost)

    if diff_edit_cost >= 0 do
      {opts, errors}
    else
      {opts, [{"diff_edit_cost", diff_edit_cost} | errors]}
    end
  end

  defp validate_match_max_bits({opts, errors}) do
    match_max_bits = Keyword.fetch!(opts, :match_max_bits)

    if valid_match_max_bits?(match_max_bits) do
      {opts, errors}
    else
      {opts, [{"match_max_bits", match_max_bits} | errors]}
    end
  end

  defp validate_match_threshold({opts, errors}) do
    match_threshold = Keyword.fetch!(opts, :match_threshold)

    if valid_threshold?(match_threshold) do
      {opts, errors}
    else
      {opts, [{"match_threshold", match_threshold} | errors]}
    end
  end

  defp validate_match_distance({opts, errors}) do
    match_distance = Keyword.fetch!(opts, :match_distance)

    if match_distance >= 0 do
      {opts, errors}
    else
      {opts, [{"match_distance", match_distance} | errors]}
    end
  end

  defp validate_patch_delete_threshold({opts, errors}) do
    patch_delete_threshold = Keyword.fetch!(opts, :patch_delete_threshold)

    if valid_threshold?(patch_delete_threshold) do
      {opts, errors}
    else
      {opts, [{"patch_delete_threshold", patch_delete_threshold} | errors]}
    end
  end

  defp validate_patch_margin({opts, errors}) do
    match_max_bits = Keyword.fetch!(opts, :match_max_bits)
    patch_margin = Keyword.fetch!(opts, :patch_margin)

    if patch_margin >= 0 &&
         (!valid_match_max_bits?(match_max_bits) ||
            patch_margin < match_max_bits) do
      {opts, errors}
    else
      {opts, [{"patch_margin", patch_margin} | errors]}
    end
  end
end
