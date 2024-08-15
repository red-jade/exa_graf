defmodule Exa.Graf.DotTypes do
  @moduledoc """
  Types for handling GraphViz DOT format.
  """

  alias Exa.Graf.Types, as: G

  @typedoc "A graph element is a node or an edge."
  @type dot_elem() :: G.vert() | G.edge()

  @typedoc """
  Keyword attributes for graph elements.

  Attribute names are converted to atoms to be keys.
  """
  @type attr_kw() :: Keyword.t()

  @typedoc """
  Key for attribute map: 
  - graph/subgraph name
  - node or edge
  - `:node` or `:edge` global property keys
  """
  @type gkey() :: :node | :edge | G.vert() | G.edge() | G.gname()

  @typedoc """
  A map of keyword lists for node, edge, graph and subgraph (cluster) attributes.

  For example:
  - graph attribute: `"mydot" => [{:size, {4,4}}, {:rankdir, :TB}]`
  - node attributes: `3 => [{:label, "foo", {:color, "red"}, {:shape, :ellipse}]`
  - edge attributes: `{1,3} => [{:style, :dashed}, {:direction, :both}]`
  """
  @type graph_attrs() :: %{gkey() => attr_kw()}

  @typedoc """
  Index of alias names to integer id.
  Nodes are identified by integer.

  The 'alias' is the identifier used in the DOT file,
  when the node name is not a raw integer.

  The 'label' is an optional arbitrary string, which can be multi-line.
  A node with an integer id may have an alias, or a label, or both.
  """
  @type aliases() :: %{String.t() => G.vert()}

  # output types ----------

  @typedoc "Allowed output rendering formats."
  @type format() :: :png | :svg | :bmp | :dot | :fig | :gif | :pdf | :ps | :ps2 | :plain

  # ----------------------------
  # attribute value enumerations
  # ----------------------------

  @typedoc "Rankdir for the graph: top-to-bottom, left-to-right, etc."
  @type rankdir() :: :TB | :LR | :BT | :RL

  @typedoc "Direction values for arrowheads on edges."
  @type direction() :: :forward | :back | :both | :none

  @typedoc "Styles for nodes and edges."
  @type style() :: :solid | :dashed | :dotted | :bold | :invis | :filled | :diagonals | :rounded

  @typedoc "Rank for nodes."
  @type rank() :: :same | :min | :max | :source | :sink

  @typedoc "Rank for clusters."
  @type cluster_rank() :: :global | :none

  @typedoc "Aspect ratio."
  @type aspect_ratio() :: float() | :fill | :auto

  @typedoc "Orientation for the page."
  @type orientation() :: :landscape | :portrait

  @typedoc """
  Alignment for horizontal jutification (labeljust) 
  and vertical alignment (labelloc)
  """
  @type align() :: :c | :l | :r | :t | :b

  @typedoc "Compass point for edge head/tail attachment ports."
  @type attach_port() :: :n | :ne | :e | :se | :s | :sw | :w | :nw

  @typedoc "Node shape."
  @type shape() ::
          :box
          | :polygon
          | :ellipse
          | :circle
          | :point
          | :egg
          | :triangle
          | :diamond
          | :trapezium
          | :parallelogram
          | :house
          | :hexagon
          | :octagon
          | :doublecircle
          | :doubleoctagon
          | :tripleoctagon
          | :invtriangle
          | :invtrapezium
          | :invhouse
          | :none
          | :Mdiamond
          | :Msquare
          | :Mcircle
          | :record
          | :Mrecord
end