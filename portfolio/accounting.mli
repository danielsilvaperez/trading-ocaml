open Trading_core.Types

val empty : capital:float -> portfolio
val apply_trade : portfolio -> trade -> portfolio
val mark_to_market : portfolio -> instrument -> price:float -> portfolio
val exposure_by_asset_class : portfolio -> (asset_class * float) list
