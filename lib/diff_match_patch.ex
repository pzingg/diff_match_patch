defmodule DiffMatchPatch do
  @moduledoc false

  defmodule Options do
    @moduledoc """
    Defaults.
    Set these on your `DiffMatchPatch` instance to override the defaults.

    `:diff_timeout` - Number of seconds to map a diff before giving up (0 for infinity).
    `:diff_edit_cost` - Cost of an empty edit operation in terms of edit characters.
    `:match_threshold` - At what point is no match declared (0.0 = perfection, 1.0 = very loose).
    `:match_distance` - How far to search for a match (0 = exact location, 1000+ = broad match).
      A match this many characters away from the expected location will add
      1.0 to the score (0.0 is a perfect match).
    `:patch_delete_threshold` - When deleting a large block of text (over ~64 characters), how close do
      the contents have to be to match the expected contents. (0.0 = perfection,
      1.0 = very loose).  Note that Match_Threshold controls how closely the
      end points of a delete need to match.
    `:patch_margin` - Chunk size for context length.
    `:match_max_bits` - The number of bits in an int.
    """
    defstruct diff_timeout: 1.0,
              diff_edit_cost: 4,
              match_threshold: 0.5,
              match_distance: 1000,
              patch_delete_threshold: 0.5,
              patch_margin: 4,
              match_max_bits: 32

    @type t() :: %__MODULE__{
            diff_timeout: float(),
            diff_edit_cost: non_neg_integer(),
            match_threshold: float(),
            match_distance: non_neg_integer(),
            patch_delete_threshold: float(),
            patch_margin: non_neg_integer(),
            match_max_bits: non_neg_integer()
          }
  end
end
