# These tests depend on a specific `:match_max_bits` value.
excluded =
  case Dmp.Options.default() |> Keyword.fetch!(:match_max_bits) do
    32 -> []
    _ -> [:match_max_32]
  end

ExUnit.start(exclude: [:skip | excluded])
