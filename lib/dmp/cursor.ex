defmodule Dmp.Cursor do
  @moduledoc """
  A container for Elixir lists, that can be used to iterate forward
  and backward, with a focused "current" item in the list, and "prev" and "next"
  lists of items that come before and after the current item.

  In Elm this has been described as a "Zipper".
  """

  defstruct prev: [], current: nil, next: []

  alias __MODULE__

  @type t() :: %Cursor{
          prev: list(),
          current: term(),
          next: list()
        }

  @typedoc "A value used to set the Cursor position."
  @type position_value() :: -1 | non_neg_integer() | :last | :tail

  @type init_option() :: {:position, position_value()}
  @type init_options() :: list(init_option())

  @doc """
  Create a Cursor containing no items.

  ## Examples

      iex> Cursor.new()
      %Cursor{current: nil, next: [], prev: []}

  """

  @spec new() :: t()
  def new(), do: %Cursor{prev: [], current: nil, next: []}

  @doc """
  Create a Cursor from a list of items.

  `opts` - the keyword `:position` can specify a position value,
  such as `-1`, `0`, `1`, `:last`, or `tail`
  in order to set the position of the created Cursor.

  ## Examples

      iex> Cursor.from_list([1, 2, 3])
      %Cursor{current: nil, next: [1, 2, 3], prev: []}

      iex> Cursor.from_list([1, 2, 3]) |> Cursor.move_forward()
      %Cursor{current: 1, next: [2, 3], prev: []}

      iex> Cursor.from_list([1, 2, 3], position: 1)
      %Cursor{current: 2, next: [3], prev: [1]}

  """
  @spec from_list(list(), init_options()) :: t()
  def from_list(items, opts \\ []) when is_list(items) do
    %Cursor{prev: [], current: nil, next: items} |> with_init_options(opts)
  end

  @doc """
  Create a Cursor from two lists.

  `opts` - the keyword `:position` can specify a position value,
  such as `-1`, `0`, `1`, `:last`, or `tail`
  in order to set the position of the created Cursor.

  ## Examples

      iex> Cursor.from_split([1, 2, 3], [4, 5, 6])
      %Cursor{current: 4, next: [5, 6], prev: [3, 2, 1]}

  """
  @spec from_split(list(), list(), init_options()) :: t()
  def from_split(prev, next, opts \\ [])

  def from_split([], next, opts) when is_list(next) do
    %Cursor{prev: [], current: nil, next: next} |> with_init_options(opts)
  end

  def from_split(prev, [], opts) when is_list(prev) do
    %Cursor{prev: Enum.reverse(prev), current: nil, next: []} |> with_init_options(opts)
  end

  def from_split(prev, [h | t], opts) when is_list(prev) do
    %Cursor{prev: Enum.reverse(prev), current: h, next: t} |> with_init_options(opts)
  end

  @spec with_init_options(t(), init_options()) :: t()
  defp with_init_options(%Cursor{} = c, opts) do
    case Keyword.get(opts, :position, nil) do
      pos when is_integer(pos) -> move_to(c, pos)
      _ -> c
    end
  end

  @doc """
  Extract the list from a Cursor.

  ## Examples

      iex> Cursor.from_list([1, 2, 3]) |> Cursor.to_list()
      [1, 2, 3]

  """
  @spec to_list(t()) :: list()
  def to_list(%Cursor{prev: prev, current: nil, next: next}) do
    Enum.reverse(prev) ++ next
  end

  def to_list(%Cursor{prev: prev, current: current, next: next}) do
    Enum.reverse(prev) ++ [current | next]
  end

  @doc """
  Returns true if there are no items in the Cursor.

  ## Examples

      iex> Cursor.from_list([]) |> Cursor.empty?()
      true

      iex> Cursor.from_list([1, 2, 3]) |> Cursor.empty?()
      false

  """
  @spec empty?(t()) :: boolean()
  def empty?(%Cursor{prev: prev, current: nil, next: next}),
    do: Enum.empty?(prev) && Enum.empty?(next)

  def empty?(%Cursor{}), do: false

  @doc """
  Returns true if the Cursor is positioned before the first item.

  ## Examples

      iex> Cursor.from_list([1, 2, 3]) |> Cursor.before_first?()
      true

      iex> Cursor.from_list([1, 2, 3]) |> Cursor.move_forward() |> Cursor.before_first?()
      false

  """
  @spec before_first?(t()) :: boolean()
  def before_first?(%Cursor{prev: [], current: nil}), do: true
  def before_first?(%Cursor{}), do: false

  @doc """
  Returns false if the Cursor is positioned at or before the first item.

  ## Examples

      iex> Cursor.from_list([1, 2]) |> Cursor.has_previous?()
      false

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward() |> Cursor.has_previous?()
      false

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(2) |> Cursor.has_previous?()
      true

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(3) |> Cursor.has_previous?()
      true

  """
  @spec has_previous?(t()) :: boolean()
  def has_previous?(%Cursor{prev: []}), do: false
  def has_previous?(%Cursor{}), do: true

  @doc """
  Returns true if the Cursor is positioned after the last item.

  ## Examples

      iex> Cursor.from_list([1, 2]) |> Cursor.after_last?()
      false

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(2) |> Cursor.after_last?()
      false

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(3) |> Cursor.after_last?()
      true

  """
  @spec after_last?(t()) :: boolean()
  def after_last?(%Cursor{current: nil, next: []}), do: true
  def after_last?(%Cursor{}), do: false

  @doc """
  Returns false if the Cursor is positioned at or after the last item.

  ## Examples

      iex> Cursor.from_list([1, 2]) |> Cursor.has_next?()
      true

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(2) |> Cursor.has_next?()
      false

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(3) |> Cursor.has_next?()
      false

  """
  @spec has_next?(t()) :: boolean()
  def has_next?(%Cursor{next: []}), do: false
  def has_next?(%Cursor{}), do: true

  @doc """
  Returns the total number of items in the Cursor.

  ## Examples

      iex> Cursor.from_list([1, 2, 3]) |> Cursor.count()
      3

  """
  @spec count(t()) :: non_neg_integer()
  def count(%Cursor{prev: prev, current: nil, next: next}),
    do: Enum.count(prev) + Enum.count(next)

  def count(%Cursor{prev: prev, next: next}), do: Enum.count(prev) + Enum.count(next) + 1

  @doc """
  Returns the current position of the Cursor.

  A return value of `-1` means the Cursor is positioned before the first item.
  A return value of `Cursor.count(c)` means the Cursor is positioned after the last item.

  ## Examples

      iex> Cursor.from_list([1, 2]) |> Cursor.position()
      -1

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward() |> Cursor.position()
      0

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(2) |> Cursor.position()
      1

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(3) |> Cursor.position()
      2

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(4) |> Cursor.position()
      2

  """
  @spec position(t()) :: integer()
  def position(%Cursor{prev: [], current: nil}), do: -1
  def position(%Cursor{prev: prev}), do: Enum.count(prev)

  @doc """
  Changes the current position of the Cursor.

  `pos` - The desired position.

      `-1` means the Cursor is positioned before the first item.
      `0` means the Cursor is positioned at the first item (if it is not empty).
      `:last` means the Cursor is positioned at the last item (if it is not empty).
      `:tail` means the Cursor is positioned after the last item.

  ## Examples

    iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_to(-1)
    %Cursor{current: nil, next: [1, 2, 3, 4, 5], prev: []}

    iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_to(0)
    %Cursor{current: 1, next: [2, 3, 4, 5], prev: []}

    iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_to(1)
    %Cursor{current: 2, next: [3, 4, 5], prev: [1]}

    iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_to(:last)
    %Cursor{current: 5, next: [], prev: [4, 3, 2, 1]}

    iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_to(:tail)
    %Cursor{current: nil, next: [], prev: [5, 4, 3, 2, 1]}

  """
  @spec move_to(t(), position_value()) :: t()
  def move_to(%Cursor{prev: prev, current: current, next: next}, :tail) do
    current_and_next =
      if is_nil(current) do
        prev
      else
        [current | next]
      end

    %Cursor{prev: Enum.reverse(current_and_next) ++ prev, current: nil, next: []}
  end

  def move_to(%Cursor{} = c, :last), do: c |> move_to(:tail) |> move_back(1)

  def move_to(%Cursor{prev: prev, current: current, next: next}, -1) do
    current_and_next =
      if is_nil(current) do
        next
      else
        [current | next]
      end

    %Cursor{prev: [], current: nil, next: Enum.reverse(prev) ++ current_and_next}
  end

  def move_to(%Cursor{} = c, pos) when is_integer(pos) and pos >= 0 do
    c |> move_to(-1) |> move_forward(pos + 1)
  end

  @doc """
  Resets the position of the Cursor to before the first item.
  Alias for `move_to(c, -1)`.

  ## Examples

      iex> Cursor.new() |> Cursor.reset()
      %Cursor{current: nil, next: [], prev: []}

      iex> Cursor.from_list([1]) |> Cursor.move_forward() |> Cursor.reset()
      %Cursor{current: nil, next: [1], prev: []}

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward() |> Cursor.reset()
      %Cursor{current: nil, next: [1, 2], prev: []}

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.reset()
      %Cursor{current: nil, next: [1, 2, 3, 4, 5], prev: []}

  """
  def reset(%Cursor{} = c), do: move_to(c, -1)

  @doc """
  Moves the position of the Cursor to the first item.
  Alias of `move_to(c, 0)`.

  ## Examples

      iex> Cursor.new() |> Cursor.move_first()
      %Cursor{current: nil, next: [], prev: []}

      iex> Cursor.from_list([1]) |> Cursor.move_first()
      %Cursor{current: 1, next: [], prev: []}

      iex> Cursor.from_list([1, 2]) |> Cursor.move_first()
      %Cursor{current: 1, next: [2], prev: []}

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_first()
      %Cursor{current: 1, next: [2, 3, 4, 5], prev: []}

  """
  @spec move_first(t()) :: t()
  def move_first(%Cursor{} = c), do: move_to(c, 0)

  @doc """
  Moves the position of the Cursor forward a number of steps.

  ## Examples

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_forward()
      %Cursor{current: 4, next: [5], prev: [3, 2, 1]}

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_forward(2)
      %Cursor{current: 5, next: [], prev: [4, 3, 2, 1]}

      iex> Cursor.from_list([1, 2, 3, 4, 5]) |> Cursor.move_forward(3)
      %Cursor{current: 3, next: [4, 5], prev: [2, 1]}

      iex> Cursor.from_list([1, 2, 3, 4, 5]) |> Cursor.move_forward(5)
      %Cursor{current: 5, next: [], prev: [4, 3, 2, 1]}

      iex> Cursor.from_list([1, 2, 3, 4, 5]) |> Cursor.move_forward(6)
      %Cursor{current: nil, next: [], prev: [5, 4, 3, 2, 1]}

      iex> %Cursor{current: {:delete, "abc"}, next: [{:equal, "xxx"}, {:insert, "def"}], prev: []} |> Cursor.move_forward(1)
      %Cursor{current: {:equal, "xxx"}, next: [{:insert, "def"}], prev: [{:delete, "abc"}]}

      iex> %Cursor{current: {:delete, "abc"}, next: [{:equal, "xxx"}, {:insert, "def"}], prev: []} |> Cursor.move_forward(2)
      %Cursor{current: {:insert, "def"}, next: [], prev: [{:equal, "xxx"}, {:delete, "abc"}]}

      iex> %Cursor{current: {:delete, "abc"}, next: [{:equal, "xxx"}, {:insert, "def"}], prev: []} |> Cursor.move_forward(3)
      %Cursor{current: nil, next: [], prev: [{:insert, "def"}, {:equal, "xxx"}, {:delete, "abc"}]}

  """
  @spec move_forward(t(), integer()) :: t()
  def move_forward(cursor, count \\ 1)
  def move_forward(%Cursor{} = c, count) when is_integer(count) and count <= 0, do: c

  def move_forward(%Cursor{prev: prev, current: current, next: next}, count)
      when is_integer(count) do
    current_and_prev =
      if is_nil(current) do
        prev
      else
        [current | prev]
      end

    if count > Enum.count(next) do
      %Cursor{prev: Enum.reverse(next) ++ current_and_prev, current: nil, next: []}
    else
      {moved, next_next} = Enum.split(next, count)

      {next_cur, to_prev} =
        if Enum.empty?(moved) do
          {nil, []}
        else
          [next_cur | to_prev] = Enum.reverse(moved)
          {next_cur, to_prev}
        end

      %Cursor{prev: to_prev ++ current_and_prev, current: next_cur, next: next_next}
    end
  end

  @doc """
  Moves the position of the Cursor forward through the "next" list
  until the given item is found. Returns `nil` and if the item cannot be found.

  ## Examples

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.find_forward(5)
      %Cursor{current: 5, next: [], prev: [4, 3, 2, 1]}

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.find_forward(1)
      nil

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.find_forward(3)
      nil

  """
  @spec find_forward(t(), term()) :: nil | t()
  def find_forward(%Cursor{next: next} = c, item) do
    case Enum.find_index(next, fn i -> i == item end) do
      nil -> nil
      i -> move_forward(c, i + 1)
    end
  end

  @doc """
  Moves the position of the Cursor back a number of steps.

  ## Examples

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_back(1)
      %Cursor{current: 2, next: [3, 4, 5], prev: [1]}

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_back(2)
      %Cursor{current: 1, next: [2, 3, 4, 5], prev: []}

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.move_back(3)
      %Cursor{current: nil, next: [1, 2, 3, 4, 5], prev: []}

  """
  @spec move_back(t(), integer()) :: t()
  def move_back(cursor, count \\ 1)
  def move_back(%Cursor{} = c, count) when is_integer(count) and count <= 0, do: c

  def move_back(%Cursor{prev: prev, current: current, next: next}, count)
      when is_integer(count) do
    current_and_next =
      if is_nil(current) do
        next
      else
        [current | next]
      end

    if count > Enum.count(prev) do
      %Cursor{prev: [], current: nil, next: Enum.reverse(prev) ++ current_and_next}
    else
      {moved, next_prev} = Enum.split(prev, count)

      {next_cur, to_next} =
        if Enum.empty?(moved) do
          {nil, []}
        else
          [next_cur | to_next] = Enum.reverse(moved)
          {next_cur, to_next}
        end

      %Cursor{prev: next_prev, current: next_cur, next: to_next ++ current_and_next}
    end
  end

  @doc """
  Moves the position of the Cursor back through the "prev" list
  until the given item is found. Returns `nil` and if the item cannot be found.

  ## Examples

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.find_back(1)
      %Cursor{current: 1, next: [2, 3, 4, 5], prev: []}

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.find_back(5)
      nil

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.find_back(3)
      nil

  """
  @spec find_back(t(), term()) :: nil | t()
  def find_back(%Cursor{prev: prev} = c, item) do
    case Enum.find_index(prev, fn i -> i == item end) do
      nil -> nil
      i -> move_back(c, i + 1)
    end
  end

  @doc """
  Moves the position of the Cursor back through the "prev" list to the given item.
  Raises if the item cannot be found.
  """
  @spec find_back!(t(), term()) :: t()
  def find_back!(%Cursor{} = c, item) do
    case find_back(c, item) do
      nil -> raise "item #{inspect(item)} not found in Cursor"
      found -> found
    end
  end

  @doc """
  Return a 3-tuple of the previous, current, and next items relative to
  the Cursor's current position.

  Any elements of the tuple may be `nil`.

  ## Examples

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.get()
      {2, 3, 4}

      iex> Cursor.from_list([]) |> Cursor.get()
      {nil, nil, nil}

      iex> Cursor.from_list([1, 2]) |> Cursor.get()
      {nil, nil, 1}

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward() |> Cursor.get()
      {nil, 1, 2}

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(2) |> Cursor.get()
      {1, 2, nil}

      iex> Cursor.from_list([1, 2]) |> Cursor.move_forward(3) |> Cursor.get()
      {2, nil, nil}
  """
  @spec get(t()) :: {term(), term(), term()}
  def get(%Cursor{prev: prev, current: current, next: next}) do
    {List.first(prev), current, List.first(next)}
  end

  @doc """
  Remove items at the Cursor's current position, leaving the
  previous items alone.

  ## Examples

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.delete(1)
      %Cursor{current: 4, next: [5], prev: [2, 1]}

  """
  @spec delete(t(), integer()) :: t()
  def delete(cursor, count \\ 1)
  def delete(%Cursor{} = c, count) when is_integer(count) and count <= 0, do: c

  def delete(%Cursor{current: current, next: next} = c, count) when is_integer(count) do
    next_next =
      if is_nil(current) do
        Enum.drop(next, count)
      else
        Enum.drop(next, count - 1)
      end

    case next_next do
      [] -> %Cursor{c | current: nil, next: []}
      [next_cur | rest] -> %Cursor{c | current: next_cur, next: rest}
    end
  end

  @doc """
  Remove items before the Cursor's current position, leaving the
  current and next items alone.

  ## Examples

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.delete_before(1)
      %Cursor{current: 3, next: [4, 5], prev: [1]}

  """
  @spec delete_before(t(), integer()) :: t()
  def delete_before(cursor, count \\ 1)
  def delete_before(%Cursor{} = c, count) when is_integer(count) and count <= 0, do: c

  def delete_before(%Cursor{prev: prev} = c, count) when is_integer(count) do
    %Cursor{c | prev: Enum.drop(prev, count)}
  end

  @doc """
  Insert items at the Cursor's current position, leaving the
  previous items alone. After the insertion, the Cursor points
  to the first inserted item.

  ## Examples

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.insert([10, 11])
      %Cursor{current: 10, next: [11, 3, 4, 5], prev: [2, 1]}

  """
  @spec insert(t(), list()) :: t()
  def insert(%Cursor{} = c, []), do: c

  def insert(%Cursor{current: nil, next: next} = c, [next_cur | rest]) do
    %Cursor{c | current: next_cur, next: rest ++ next}
  end

  def insert(%Cursor{current: current, next: next} = c, [next_cur | rest]) do
    %Cursor{c | current: next_cur, next: rest ++ [current | next]}
  end

  @doc """
  Insert items before the Cursor's current position, leaving the
  current and next items alone.

  ## Examples

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.insert_before([10, 11])
      %Cursor{current: 3, next: [4, 5], prev: [11, 10, 2, 1]}

  """
  @spec insert_before(t(), list()) :: t()
  def insert_before(%Cursor{} = c, []), do: c

  def insert_before(%Cursor{prev: prev} = c, items) when is_list(items) do
    %Cursor{c | prev: Enum.reverse(items) ++ prev}
  end

  @doc """
  Insert items at the Cursor's head position, leaving the current position pointer alone.

  ## Examples

      iex> %Cursor{current: 3, next: [4, 5], prev: [2, 1]} |> Cursor.insert_at_head([10, 11])
      %Cursor{current: 3, next: [4, 5], prev: [2, 1, 11, 10]}

  """
  @spec insert_at_head(t(), list()) :: t()
  def insert_at_head(%Cursor{} = c, []), do: c

  def insert_at_head(%Cursor{prev: prev} = c, items) when is_list(items) do
    %Cursor{c | prev: prev ++ Enum.reverse(items)}
  end
end
