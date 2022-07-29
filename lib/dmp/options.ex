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

  @type t() :: %Options{
          diff_timeout: float(),
          diff_edit_cost: non_neg_integer(),
          match_max_bits: non_neg_integer(),
          match_threshold: float(),
          match_distance: non_neg_integer(),
          patch_delete_threshold: float(),
          patch_margin: non_neg_integer()
        }

  @doc """
  Returns an `Options` struct with good default values:

  ## Examples

    iex> Options.default()
    %Options{
        diff_edit_cost: 4,
        diff_timeout: 1.0,
        match_distance: 1000,
        match_max_bits: 32,
        match_threshold: 0.5,
        patch_delete_threshold: 0.5,
        patch_margin: 4}

  """
  def default(), do: %__MODULE__{}

  @doc """
  Validates an `Options` struct, raising an `ArgumentError` if it contains invalid values.

  If `nil` is passed, `Options.default()` will be returned.

  ## Examples

    iex> Options.valid_options!(nil)
    %Options{
        diff_edit_cost: 4,
        diff_timeout: 1.0,
        match_distance: 1000,
        match_max_bits: 32,
        match_threshold: 0.5,
        patch_delete_threshold: 0.5,
        patch_margin: 4}

    iex> Options.valid_options!(%Options{match_max_bits: -1})
    ** (ArgumentError) Invalid Options value(s)

  """
  @spec valid_options!(nil | t()) :: t()
  def valid_options!(nil), do: default()

  # credo:disable-for-lines:25 Credo.Check.Refactor.CyclomaticComplexity
  def valid_options!(
        %Options{
          diff_timeout: diff_timeout,
          diff_edit_cost: diff_edit_cost,
          match_max_bits: match_max_bits,
          match_threshold: match_threshold,
          match_distance: match_distance,
          patch_delete_threshold: patch_delete_threshold,
          patch_margin: patch_margin
        } = opts
      ) do
    valid =
      match_max_bits >= 0 && match_max_bits <= 64 &&
        patch_margin >= 0 && patch_margin < match_max_bits &&
        match_threshold >= 0 && match_threshold <= 1.0 &&
        patch_delete_threshold >= 0 && patch_delete_threshold <= 1.0 &&
        match_distance >= 0 &&
        diff_edit_cost >= 0 &&
        diff_timeout >= 0

    if valid do
      opts
    else
      raise ArgumentError, "Invalid Options value(s)"
    end
  end
end
