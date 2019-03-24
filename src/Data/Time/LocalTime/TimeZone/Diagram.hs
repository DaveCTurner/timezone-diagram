{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Time.LocalTime.TimeZone.Diagram where

import Control.Monad.Reader
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix (Matrix(..))
import Graphics.Rendering.Pango
import Control.Monad

{-| Offset of a timeline from UTC. -}
newtype Offset = Offset { doubleFromOffset :: Double }

utcOffset :: Offset
utcOffset = Offset 0

{-| A UTC time, drawn on the horizontal axis with 0 in the centre. -}
newtype UTime = UTime { doubleFromUTime :: Double }

axisOriginMargin :: Double
axisOriginMargin = 50

axisFreeEndMargin :: Double
axisFreeEndMargin = 11

transitionPointRadius :: Double
transitionPointRadius = 2.5

axisLineWidth :: Double
axisLineWidth = 2.0

timeLineWidth :: Double
timeLineWidth = 1.0

arrowSize :: Double
arrowSize = 5.0

data TimeLineGraphConfig = TimeLineGraphConfig
  { _tlgcCentreOffset :: Offset
  , _tlgcRenderSetup  :: Render ()
  }

newtype TimeLineGraph a = TimeLineGraph { runTimeLineGraph :: ReaderT TimeLineGraphConfig Render a }
  deriving (Functor, Applicative, Monad, MonadReader TimeLineGraphConfig)

liftRender :: Render a -> TimeLineGraph a
liftRender r = do
  renderSetup <- asks _tlgcRenderSetup
  TimeLineGraph $ lift $ do
    save
    renderSetup
    result <- r
    restore
    return result

getTargetWidth :: TimeLineGraph Double
getTargetWidth = liftRender $ withTargetSurface $ \surface -> fromIntegral <$> imageSurfaceGetWidth surface

getTargetHeight :: TimeLineGraph Double
getTargetHeight = liftRender $ withTargetSurface $ \surface -> fromIntegral <$> imageSurfaceGetHeight surface

type Point = (Double, Double)

getBounds :: TimeLineGraph (Point, Point, Point)
getBounds = do
  w <- getTargetWidth
  h <- getTargetHeight

  let xMin = axisOriginMargin
      xMax = w - axisFreeEndMargin
      yMin = axisFreeEndMargin
      yMax = h - axisOriginMargin

  return ((xMin, yMin), ((xMin + xMax) / 2.0, (yMin + yMax) / 2.0), (xMax, yMax))

{-| Y coordinate of the timeline with the given offset at UTC time 0 (i.e. in centre) -}
getTimelineYInCentre :: Offset -> TimeLineGraph Double
getTimelineYInCentre timelineOffset = do
  centreOffset <- asks _tlgcCentreOffset
  (_, (_, yc), _) <- getBounds
  return $ yc - doubleFromOffset centreOffset + doubleFromOffset timelineOffset

coordsFromOffset :: UTime -> Offset -> TimeLineGraph Point
coordsFromOffset (UTime x) off = do
  timelineYInCentre <- getTimelineYInCentre off
  (_, (xc, _), _) <- getBounds
  return (xc + x, timelineYInCentre - x)

earliestCoordsFromOffset :: Offset -> TimeLineGraph Point
earliestCoordsFromOffset off = do
  timelineYInCentre <- getTimelineYInCentre off
  ((xMin, _), (xc, _), (_, yMax)) <- getBounds
  let lineLength = min (xc - xMin) (yMax - timelineYInCentre)
  return (xc - lineLength, timelineYInCentre + lineLength)

latestCoordsFromOffset :: Offset -> TimeLineGraph Point
latestCoordsFromOffset off = do
  timelineYInCentre <- getTimelineYInCentre off
  ((_, yMin), (xc, yc), (xMax, _)) <- getBounds
  let lineLength = min (xMax - xc) (timelineYInCentre - yMin)
  return (xc + lineLength, timelineYInCentre - lineLength)

drawBeforeTransition :: UTime -> Offset -> TimeLineGraph ()
drawBeforeTransition x off = do
  (x0, y0) <- earliestCoordsFromOffset off
  (x1, y1) <- coordsFromOffset x off
  liftRender $ do
    setLineCap LineCapSquare
    setLineWidth timeLineWidth
    moveTo x0 y0
    lineTo (x1 - transitionPointRadius + timeLineWidth) (y1 + transitionPointRadius - timeLineWidth)
    stroke
    arc x1 y1 transitionPointRadius 0 (2*pi)
    stroke

drawAfterTransition :: UTime  -> Offset -> TimeLineGraph ()
drawAfterTransition t off = do
  (x0, y0) <- coordsFromOffset t off
  (x1, y1) <- latestCoordsFromOffset off
  liftRender $ do
    setLineCap LineCapSquare
    setLineWidth timeLineWidth
    arc x0 y0 transitionPointRadius 0 (2*pi)
    fillPreserve
    stroke
    moveTo (x0 + transitionPointRadius - timeLineWidth) (y0 - transitionPointRadius + timeLineWidth)
    lineTo x1 y1
    stroke

drawAxes :: TimeLineGraph ()
drawAxes = do
  ((xMin, yMin), _, (xMax, yMax)) <- getBounds
  liftRender $ do
    setLineWidth axisLineWidth
    moveTo xMin yMin
    lineTo xMin yMax
    lineTo xMax yMax
    stroke
    moveTo xMin (yMin - arrowSize)
    lineTo (xMin + arrowSize) (yMin +  arrowSize)
    lineTo (xMin - arrowSize) (yMin +  arrowSize)
    fill
    moveTo (xMax + arrowSize) yMax
    lineTo (xMax - arrowSize) (yMax - arrowSize)
    lineTo (xMax - arrowSize) (yMax + arrowSize)
    fill

    do
      pangoLayout <- createLayout "Universal time"
      liftIO $ do
        fontDescription <- fontDescriptionFromString "Sans 8"
        layoutSetFontDescription pangoLayout $ Just fontDescription
      PangoRectangle x0 y0 x1 y1 <- liftIO $ fst <$> layoutGetExtents pangoLayout
      moveTo (xMax - (x1 - x0)) (yMax + (y1 - y0))
      showLayout pangoLayout

    do
      pangoLayout <- createLayout "Local time"
      liftIO $ do
        fontDescription <- fontDescriptionFromString "Sans 8"
        layoutSetFontDescription pangoLayout $ Just fontDescription
      PangoRectangle x0 y0 x1 y1 <- liftIO $ fst <$> layoutGetExtents pangoLayout
      setMatrix (Matrix 0 (-1) 1 0 0 0)
      moveTo (x0 - x1 - (yMin + 2*arrowSize)) (xMin - y1 - 10)
      showLayout pangoLayout

drawConversion :: UTime -> Offset -> TimeLineGraph ()
drawConversion t offset = do
  ((xMin, _), _, (_, yMax)) <- getBounds
  (x,y) <- coordsFromOffset t offset
  liftRender $ do
    setLineCap LineCapSquare
    setLineWidth timeLineWidth
    setColourRed
    moveTo x    yMax
    lineTo x    y
    lineTo xMin y
    stroke

setColourBlue :: Render ()
setColourBlue = setSourceRGB (45/255) (112/255) (171/255)

setColourRed :: Render ()
setColourRed = setSourceRGB (182/255) (62/255) (69/255)

drawDiagram :: String -> Int -> Int -> Offset -> TimeLineGraph a -> IO a
drawDiagram fileName width height centreOffset go =
  withImageSurface FormatARGB32 width height $ \surface -> do
    result <- renderWith surface $ flip runReaderT TimeLineGraphConfig
      { _tlgcCentreOffset = centreOffset
      , _tlgcRenderSetup  = return ()
      } $ runTimeLineGraph go

    surfaceWriteToPNG surface fileName
    return result

arrowFromUTC :: UTime -> Offset -> Double -> TimeLineGraph ()
arrowFromUTC t off d = do
  (x,y) <- coordsFromOffset t off
  liftRender $ do
    setColourRed
    moveTo x (y+d)
    lineTo (x+arrowSize) (y+d+arrowSize*2)
    lineTo (x-arrowSize) (y+d+arrowSize*2)
    fill

arrowToLocal :: UTime -> Offset -> Double -> TimeLineGraph ()
arrowToLocal t off d = do
  (x,y) <- coordsFromOffset t off
  liftRender $ do
    setColourRed
    moveTo (x-d-2*arrowSize) y
    lineTo (x-d) (y+arrowSize)
    lineTo (x-d) (y-arrowSize)
    fill

arrowToUTC :: UTime -> Offset -> Double -> TimeLineGraph ()
arrowToUTC t off d = do
  (x,y) <- coordsFromOffset t off
  liftRender $ do
    setColourRed
    moveTo x (y+d+arrowSize*2)
    lineTo (x+arrowSize) (y+d)
    lineTo (x-arrowSize) (y+d)
    fill

arrowFromLocal :: UTime -> Offset -> Double -> TimeLineGraph ()
arrowFromLocal t off d = do
  (x,y) <- coordsFromOffset t off
  liftRender $ do
    setColourRed
    moveTo (x-d) y
    lineTo (x-d-2*arrowSize) (y+arrowSize)
    lineTo (x-d-2*arrowSize) (y-arrowSize)
    fill

drawConstantOffset :: Offset -> TimeLineGraph ()
drawConstantOffset off = drawConstantOffsetWithStyle off $ setLineWidth timeLineWidth

drawConstantOffsetWithStyle :: Offset -> Render () -> TimeLineGraph ()
drawConstantOffsetWithStyle off applyStyle = do
  (x0, y0) <- earliestCoordsFromOffset off
  (x1, y1) <- latestCoordsFromOffset   off
  liftRender $ do
    applyStyle
    moveTo x0 y0
    lineTo x1 y1
    stroke

drawUTC :: TimeLineGraph ()
drawUTC = do
  drawConstantOffsetWithStyle utcOffset $ do
    setLineWidth timeLineWidth
    setSourceRGB 0.7 0.7 0.7
    setDash [5,5] 0
  (x,y) <- latestCoordsFromOffset utcOffset
  liftRender $ do
    setSourceRGB 0.7 0.7 0.7
    pangoLayout <- createLayout "UTC"
    liftIO $ do
      fontDescription <- fontDescriptionFromString "Sans 8"
      layoutSetFontDescription pangoLayout $ Just fontDescription
    PangoRectangle x0 y0 x1 y1 <- liftIO $ fst <$> layoutGetExtents pangoLayout
    rotate (-pi/4)
    moveTo ((x-y) / sqrt 2 - (x1-x0)) ((x+y) / sqrt 2 + 2)
    showLayout pangoLayout

drawOffset :: UTime  -> Offset -> Offset -> TimeLineGraph ()
drawOffset t off0 off1 = do
  arrowFromUTC t off0 0
  arrowToUTC   t off1 ((-2) * arrowSize)
  (x0,y0) <- coordsFromOffset t off0
  (x1,y1) <- coordsFromOffset t off1
  liftRender $ do
    setLineWidth timeLineWidth
    setColourRed
    moveTo x0 y0
    lineTo x1 y1
    stroke

drawHorizOffset :: UTime -> Offset -> Offset -> TimeLineGraph ()
drawHorizOffset t0 off0 off1 = do
  let t1 = UTime (doubleFromUTime t0 - doubleFromOffset off0)
  arrowToLocal   t0 off0 ((-2) * arrowSize)
  arrowFromLocal t1 off1 0
  (x0,y0) <- coordsFromOffset t0 off0
  (x1,y1) <- coordsFromOffset t1 off1
  liftRender $ do
    setLineWidth timeLineWidth
    setColourRed
    moveTo x0 y0
    lineTo x1 y1
    stroke

drawGrid :: Double -> TimeLineGraph ()
drawGrid spacing = do
  (x0,y0) <- coordsFromOffset (UTime 0) utcOffset
  ((xMin, yMin), _, (xMax, yMax)) <- getBounds
  liftRender $ do
    setLineWidth 1
    setSourceRGB 0.9 0.9 0.9
    let vLine x = moveTo x yMin >> lineTo x yMax >> stroke
        hLine y = moveTo xMin y >> lineTo xMax y >> stroke
    mapM_ vLine $ takeWhile (<= xMax) $ map (\i -> x0 + i * spacing) [0..]
    mapM_ vLine $ takeWhile (>= xMin) $ map (\i -> x0 - i * spacing) [0..]
    mapM_ hLine $ takeWhile (<= yMax) $ map (\i -> y0 + i * spacing) [0..]
    mapM_ hLine $ takeWhile (>= yMin) $ map (\i -> y0 - i * spacing) [0..]

showLocalTime :: String -> UTime -> Offset -> TimeLineGraph ()
showLocalTime label t off = do
    ((xMin, _), _, (xMax, _)) <- getBounds
    (_,y) <- coordsFromOffset t off
    liftRender $ do
      setLineWidth timeLineWidth
      setSourceRGB 0.7 0.7 0.7
      setDash [5,5] 0
      moveTo xMin y
      lineTo xMax y
      stroke

    liftRender $ do
      pangoLayout <- createLayout label
      liftIO $ do
        fontDescription <- fontDescriptionFromString "Sans 8"
        layoutSetFontDescription pangoLayout $ Just fontDescription
      PangoRectangle x0 y0 x1 y1 <- liftIO $ fst <$> layoutGetExtents pangoLayout
      moveTo (xMin + 2) (y - 6 - (y1 - y0))
      showLayout pangoLayout
