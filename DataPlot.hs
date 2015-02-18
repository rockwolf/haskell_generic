------------------------------------------------------------------------------
-- | This module does data-type conversions and general data manipulations.
--   See LICENSE file for copyright and license info.
------------------------------------------------------------------------------
module DataPlot where

-----------------------------------------------------------------------------
-- ||| Imports
-----------------------------------------------------------------------------
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.SRGB
import Diagrams.Attributes
import Data.Colour.Names

-----------------------------------------------------------------------------
-- ||| Plotting
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | plotBars
-- | Create a bar chart
-----------------------------------------------------------------------------
plotBars :: PlotType -> [[Double]] -> String -> [String] -> Bool -> Renderable ()
plotBars plot_type plot_data title_main titles_series borders = toRenderable layout
 where
  layout = 
        layout_title .~ title_main ++ " " ++ btitle
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars plotData ]
      $ def :: Layout PlotIndex Double

  plotData = plot_bars_titles .~ titles_series
      $ plot_bars_values .~ addIndexes plot_data
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (cycle customColorSeq)
      $ def

  alabels = getLabelsSeries plot_type

  customColorSeq = [ toAlphaColour (sRGB 255 0 0)
                     , toAlphaColour (sRGB 0 255 0)
                     , toAlphaColour (sRGB 0 0 255)
          ]
  btitle = if borders then "" else " (no borders)"
  bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
  mkstyle c = (solidFillStyle c, bstyle)
