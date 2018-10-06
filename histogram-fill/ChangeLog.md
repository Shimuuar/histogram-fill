Changes in 0.9.1.0
  * Added `Data.Histogram.Tutorial` module	
  * GHC 8.6 Compatibility

Changes in 0.9.0.0

  * GHC 8.4 compatibility. Semigroup instances added for `HBuilder` and
    `HBuilderM`. `semigroups` dependency added for older GHCs

Changes in 0.8.5.0

  * BinVar added
  * Redundant constraints on functions removed
  * Histogram data type now stores overflows/underflows as strict data type
  * Support for GHC<7.6 is dropped

Changes in 0.8.4.1

  * All fields of LogDinD are strict and unpacked. This fixes NFData
    instance and improves performance


Changes in 0.8.4.0

  * Missing instances for CutDirection added

  * Missing NFData instances for MaybeBin, BinEnum2D, BinPermute

  * Compatibility with deepseq 1.4


Changes in 0.8.3.0

  * Constraints are relaxed from `PrimMonad' to `Monad' wherever possible in
    Data.Histogram.Fill module.


Changes in 0.8.2.0

  * Smart constructors for BinF and BinD check that bin number is not
    negative.
  * Fixed bug in `binInt'
  * Fields of `Histogram' data type are strict now.
  * Compatibility with GHC 7.8


Changes in 0.8.1.0

  * Constructor of @HBuilderM@ exported.


Changes in 0.8.0.0

  * `toHBuilderM' added and internal definition of `HBuilder' is
    changed. It required adding dependency on @monad-primitive@.
  * `joinHBuilder' and @treeHBuilder@ are deprecated.


Changes in 0.7.4.0

  * Function for searching for minimum/maximum added.
  * @NFData@ instance is fixed.


Changes in 0.7.3.0

  * `mkStatefulBuilder' is added and HBuilder constructor is exposed.
  * Indexing operators for immutable histograms are added.


Changes in 0.7.2.0

  * fromMaybeBin added.


Changes in 0.7.1.0

  * breduceX and breduceY are added.


Changes in 0.7.0.0

  * mkFoldBuilder is added to `Data.Histogram.Fill'
  * fill functions in `Data.Histogram.ST' are replaced with generic
    variant.
  * Indexing for immutable histograms is added and special constructor
    for first and last bin are added to `HistIndex' data type.
  * Functions to calculate sum, minimum and maximum of immutable
    histogram are added.


Changes in 0.6.2.0

  * MaybeBin added.
  * Helper function for defining Read instances for bins are exposed.
  * mapData function is added.
  * Slicing histogram do not results in crash if indices are out of
    bounds.
  * Eq instances for BinF and BinD are added.
  * NFData instance for Bin2D is fixed.


Changes in 0.6.1.0

  * Helper function and type synonym for Bin2D


Changes in 0.6.0.1

  * Fixed compilation with GHC 7.4
