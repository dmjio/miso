------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE CPP                #-}
------------------------------------------------------------------------------
module Main where
------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_, when, void, forever)
import           Data.IORef
import           Data.List (foldl')
import           Data.Maybe (mapMaybe)
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
------------------------------------------------------------------------------
import           Miso hiding (Phase, (!!))
import           Miso.Canvas
import qualified Miso.CSS as CSS
import           Miso.CSS (sheet_, selector_, pct, vh, StyleSheet)
import           Miso.CSS.Color
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Html.Event (onPointerMove)
------------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------------
cW, cH :: Double
cW = 800
cH = 600

paddleY, paddleH :: Double
paddleY = 556
paddleH = 14

defaultPW :: Double
defaultPW = 130

ballR :: Double
ballR = 8

bkW, bkH, bkGX, bkGY :: Double
bkW = 60; bkH = 20; bkGX = 8; bkGY = 6

bkStartX, bkStartY :: Double
bkStartX = 30; bkStartY = 70

baseSpeed :: Double
baseSpeed = 270

puSpeed :: Double
puSpeed = 100

------------------------------------------------------------------------------
-- Data Types
------------------------------------------------------------------------------
data BCol = BRed | BOrange | BYellow | BGreen | BBlue | BPurple | BCyan | BGold | BSilver
  deriving (Show, Eq)

data Brick = Brick
  { bkC :: !Int, bkR :: !Int
  , bkCol :: !BCol
  , bkHit :: !Int, bkPts :: !Int
  } deriving (Show, Eq)

bkLeft, bkTop :: Brick -> Double
bkLeft b = bkStartX + fromIntegral (bkC b) * (bkW + bkGX)
bkTop  b = bkStartY + fromIntegral (bkR b) * (bkH + bkGY)

data Ball = Ball { bX :: !Double, bY :: !Double, bVX :: !Double, bVY :: !Double }
  deriving (Show, Eq)

data PUKind = PKWide | PKMulti | PKSlow | PKLife | PKFire
  deriving (Show, Eq)

data PU = PU { puX :: !Double, puY :: !Double, puKind :: !PUKind }
  deriving (Show, Eq)

data Particle = Particle
  { pX :: !Double, pY :: !Double
  , pVX :: !Double, pVY :: !Double
  , pLife :: !Double
  , pR :: !Int, pG :: !Int, pB :: !Int
  , pSz :: !Double
  } deriving (Show, Eq)

data Phase = Menu | Playing | Paused | LevelComplete | GameOver | Victory
  deriving (Show, Eq)

data Model = Model
  { mPX    :: !Double, mPW    :: !Double
  , mBalls :: ![Ball]
  , mBrks  :: ![Brick]
  , mPUs   :: ![PU]
  , mPrts  :: ![Particle]
  , mScore :: !Int, mHigh  :: !Int
  , mLives :: !Int, mLevel :: !Int
  , mPhase :: !Phase
  , mLast  :: !Double
  , mFire  :: !Bool, mWide :: !Double
  } deriving (Show, Eq)

data Action
  = Tick Double
  | MouseX Double
  | KeyPress IS.IntSet
  | NoOp
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Level Definitions
------------------------------------------------------------------------------
mk :: Int -> Int -> BCol -> Int -> Int -> Brick
mk = Brick

row :: Int -> BCol -> Int -> Int -> [Brick]
row r col h p = [mk c r col h p | c <- [0..10]]

bySpec :: [(Int, [Int], BCol, Int, Int)] -> [Brick]
bySpec = concatMap (\(r,cs,col,h,p) -> [mk c r col h p | c <- cs])

level1 :: [Brick]
level1 = concat [ row 0 BGold 1 500, row 1 BOrange 1 400, row 2 BRed 1 300
                , row 3 BPurple 1 200, row 4 BBlue 1 150
                , row 5 BGreen 1 100,  row 6 BCyan 1 50 ]

level2 :: [Brick]
level2 = bySpec
  [ (0,[5],BGold,2,600), (1,[4,5,6],BOrange,1,400)
  , (2,[3..7],BRed,1,300), (3,[2..8],BPurple,1,200)
  , (4,[1..9],BBlue,1,150), (5,[0..10],BGreen,1,100)
  , (6,[0..10],BCyan,1,60) ]

level3 :: [Brick]
level3 = bySpec
  [ (0,[0,1,9,10],BGold,3,800), (1,[0..10],BSilver,2,500)
  , (2,[0,1,2,8,9,10],BRed,1,300), (3,[0,2,4,6,8,10],BOrange,1,250)
  , (4,[1,3,5,7,9],BYellow,1,200), (5,[0..10],BBlue,1,150)
  , (6,[0,1,9,10],BGreen,1,100) ]

level4 :: [Brick]
level4 = bySpec
  [ (0,[0,2,4,6,8,10],BGold,3,800), (0,[1,3,5,7,9],BSilver,2,500)
  , (1,[1,3,5,7,9],BGold,3,800),   (1,[0,2,4,6,8,10],BSilver,2,500)
  , (2,[0,2,4,6,8,10],BRed,2,400), (2,[1,3,5,7,9],BOrange,1,300)
  , (3,[1,3,5,7,9],BPurple,2,350), (3,[0,2,4,6,8,10],BBlue,1,200)
  , (4,[0,2,4,6,8,10],BCyan,1,150),(4,[1,3,5,7,9],BGreen,1,100)
  , (5,[0..10],BYellow,1,80) ]

level5 :: [Brick]
level5 = bySpec
  [ (0,[0..10],BGold,3,1000)
  , (1,[0,10],BGold,3,1000),   (1,[1..9],BSilver,2,600)
  , (2,[0,10],BGold,3,1000),   (2,[1,9],BSilver,2,600),  (2,[2..8],BRed,2,500)
  , (3,[0,10],BGold,3,1000),   (3,[1,9],BSilver,2,600),  (3,[2,8],BRed,2,500)
  , (3,[3..7],BOrange,1,400)
  , (4,[0,10],BGold,3,1000),   (4,[1,9],BSilver,2,600),  (4,[2,8],BRed,2,500)
  , (4,[3,7],BOrange,1,400),   (4,[4..6],BPurple,1,300)
  , (5,[0..10],BGold,3,1000),  (6,[0..10],BSilver,2,500) ]

allLevels :: [[Brick]]
allLevels = [level1, level2, level3, level4, level5]

numLevels :: Int
numLevels = length allLevels

getBricks :: Int -> [Brick]
getBricks n = allLevels !! ((n - 1) `mod` numLevels)

------------------------------------------------------------------------------
-- Deterministic star field (used in background)
------------------------------------------------------------------------------
stars :: [(Double, Double, Double)]
stars =
  [ ( fromIntegral ((i * 137 + 41) `mod` 800)
    , fromIntegral ((i * 251 + 17) `mod` 540) + 52
    , 0.5 + fromIntegral (((i * 73) `mod` 3) :: Int) * 0.4
    )
  | i <- [1..130 :: Int]
  ]

------------------------------------------------------------------------------
-- Initial State
------------------------------------------------------------------------------
initBall :: Ball
initBall = Ball (cW / 2) (paddleY - ballR - 2) 140 (-baseSpeed)

initModel :: Model
initModel = Model
  { mPX = cW / 2 - defaultPW / 2, mPW = defaultPW
  , mBalls = [initBall], mBrks = getBricks 1
  , mPUs = [], mPrts = []
  , mScore = 0, mHigh = 0
  , mLives = 3, mLevel = 1
  , mPhase = Menu, mLast = 0
  , mFire = False, mWide = 0
  }

resetLevel :: Model -> Int -> Model
resetLevel m n = m
  { mBalls = [initBall], mBrks = getBricks n
  , mPUs = [], mPhase = Playing
  , mFire = False, mWide = 0, mLevel = n
  , mPW = defaultPW, mPX = cW / 2 - defaultPW / 2
  }

loseLife :: Model -> Model
loseLife m =
  let l = mLives m - 1
  in m { mLives = l
       , mPhase = if l <= 0 then GameOver else Playing
       , mBalls = [initBall], mPUs = []
       , mFire = False, mWide = 0, mPW = defaultPW }

------------------------------------------------------------------------------
-- Animation Frame Subscription
------------------------------------------------------------------------------
animSub :: (Double -> action) -> Sub action
animSub toAction sink = do
  cbRef <- newIORef (error "animSub: uninit")
  let frame tsVal = do
        ts <- fromJSValUnchecked tsVal :: IO Double
        sink (toAction ts)
        cb <- readIORef cbRef
        void (requestAnimationFrame cb)
  cb <- syncCallback1 frame
  writeIORef cbRef cb
  void (requestAnimationFrame cb)
  forever (threadDelay maxBound)

------------------------------------------------------------------------------
-- Physics
------------------------------------------------------------------------------
clamp :: Double -> Double -> Double -> Double
clamp lo hi = max lo . min hi

ballSpeed :: Ball -> Double
ballSpeed b = sqrt (bVX b * bVX b + bVY b * bVY b)

setSpeed :: Double -> Ball -> Ball
setSpeed spd b =
  let cur = ballSpeed b
      s   = if cur < 0.001 then 1 else spd / cur
  in b { bVX = bVX b * s, bVY = bVY b * s }

levelSpeed :: Int -> Double
levelSpeed n = baseSpeed + fromIntegral (n - 1) * 22

moveBall :: Double -> Ball -> Ball
moveBall dt b = b { bX = bX b + bVX b * dt, bY = bY b + bVY b * dt }

wallBounce :: Ball -> Ball
wallBounce b =
  let hitL = bX b - ballR <= 0
      hitR = bX b + ballR >= cW
      hitT = bY b - ballR <= 0
  in b { bX  = clamp ballR (cW - ballR) (bX b)
       , bY  = if hitT then ballR else bY b
       , bVX = if hitL then abs (bVX b)
               else if hitR then -(abs (bVX b))
               else bVX b
       , bVY = if hitT then abs (bVY b) else bVY b
       }

paddleBounce :: Double -> Double -> Ball -> Ball
paddleBounce px pw b =
  let inX = bX b + ballR > px && bX b - ballR < px + pw
      inY = bY b + ballR >= paddleY && bY b + ballR <= paddleY + paddleH + 5
      spd = ballSpeed b
  in if inX && inY && bVY b > 0
     then let norm  = (bX b - (px + pw / 2)) / (pw / 2)
              angle = norm * (pi / 3)
          in b { bVX = spd * sin angle
               , bVY = -(abs (spd * cos angle))
               , bY  = paddleY - ballR - 1 }
     else b

-- Determine bounce axis by normalising penetration by brick half-extents
brickCollide :: Bool -> Brick -> Ball -> (Ball, Bool)
brickCollide fire brk b =
  let bx  = bkLeft brk; by = bkTop brk
      cpX = clamp bx (bx + bkW) (bX b)
      cpY = clamp by (by + bkH) (bY b)
      dx  = bX b - cpX; dy = bY b - cpY
      hit = dx*dx + dy*dy < ballR * ballR
  in if not hit
     then (b, False)
     else if fire
          then (b, True)
          else
            let dnX = abs (bX b - (bx + bkW/2)) / (bkW/2)
                dnY = abs (bY b - (by + bkH/2)) / (bkH/2)
                b'  = if dnY > dnX
                      then b { bVY = -(bVY b) }
                      else b { bVX = -(bVX b) }
            in (b', True)

------------------------------------------------------------------------------
-- Particles
------------------------------------------------------------------------------
bcolRGB :: BCol -> (Int, Int, Int)
bcolRGB = \case
  BRed -> (255,60,60); BOrange -> (255,145,30); BYellow -> (255,235,30)
  BGreen -> (60,215,80); BBlue -> (60,135,255); BPurple -> (185,60,225)
  BCyan -> (30,225,225); BGold -> (255,205,30); BSilver -> (190,205,235)

spawnParticles :: Double -> Double -> BCol -> [Particle]
spawnParticles x y col =
  let (r,g,b) = bcolRGB col
  in [ let rad = ang * pi / 180
           spd = 65 + fromIntegral ((floor ang :: Int) `mod` 50 :: Int)
       in Particle x y (cos rad * spd) (sin rad * spd) 1.0 r g b
               (2 + fromIntegral ((floor ang :: Int) `mod` 3 :: Int))
     | ang <- [0,24..335] ]

stepParticle :: Double -> Particle -> Maybe Particle
stepParticle dt p =
  let l = pLife p - 0.75 * dt
  in if l <= 0 then Nothing
     else Just p { pX = pX p + pVX p * dt, pY = pY p + pVY p * dt
                 , pVX = pVX p * 0.96, pVY = pVY p * 0.96, pLife = l }

------------------------------------------------------------------------------
-- Power-ups
------------------------------------------------------------------------------
puRGB :: PUKind -> (Int, Int, Int)
puRGB = \case
  PKWide -> (30,200,255); PKMulti -> (255,80,200); PKSlow -> (80,255,120)
  PKLife -> (255,80,80);  PKFire  -> (255,160,30)

puLabel :: PUKind -> MisoString
puLabel = \case
  PKWide -> "WIDE"; PKMulti -> "MULTI"; PKSlow -> "SLOW"
  PKLife -> "+LIFE"; PKFire  -> "FIRE"

maybePU :: Int -> Int -> Maybe PUKind
maybePU c r = case (c * 7 + r * 13 + c * r) `mod` 9 of
  0 -> Just PKWide; 1 -> Just PKMulti; 2 -> Just PKSlow
  3 -> Just PKLife; 4 -> Just PKFire;  _ -> Nothing

applyPU :: PUKind -> Model -> Model
applyPU kind m = case kind of
  PKWide  -> m { mPW = min 220 (mPW m + 55), mWide = 12 }
  PKMulti -> m { mBalls = take 5 (mBalls m ++ map (\b -> b { bVX = -(bVX b) }) (mBalls m)) }
  PKSlow  -> m { mBalls = map (setSpeed (max 130 (levelSpeed (mLevel m) * 0.72))) (mBalls m) }
  PKLife  -> m { mLives = min 5 (mLives m + 1) }
  PKFire  -> m { mFire = True }

------------------------------------------------------------------------------
-- Ball vs Bricks (returns updated ball, updated bricks, score, PUs, particles)
------------------------------------------------------------------------------
collideBricks :: Bool -> Ball -> [Brick] -> (Ball, [Brick], Int, [PU], [Particle])
collideBricks fire b = go b [] 0 [] []
  where
    go ball acc pts pus prts [] = (ball, reverse acc, pts, pus, prts)
    go ball acc pts pus prts (brk:rest) =
      let (ball', hit) = brickCollide fire brk ball
      in if not hit
         then go ball (brk:acc) pts pus prts rest
         else
           let hits' = bkHit brk - 1
               pts'  = pts + bkPts brk
               newPrts = spawnParticles (bkLeft brk + bkW/2) (bkTop brk + bkH/2) (bkCol brk)
               newPUs  = if hits' <= 0
                         then maybe [] (\k -> [PU (bkLeft brk) (bkTop brk + bkH) k])
                                    (maybePU (bkC brk) (bkR brk))
                         else []
               acc' = if hits' > 0 then brk { bkHit = hits' } : acc else acc
           in (ball', reverse acc' ++ rest, pts', pus ++ newPUs, prts ++ newPrts)

------------------------------------------------------------------------------
-- Game Step
------------------------------------------------------------------------------
stepGame :: Double -> Model -> Model
stepGame dt m =
  let spd     = levelSpeed (mLevel m)
      -- Power-up wide timer
      wide'   = max 0 (mWide m - dt)
      pw'     = if wide' <= 0 && mWide m > 0 then defaultPW else mPW m
      m0      = m { mWide = wide', mPW = pw' }
      -- Process each ball
      (balls', brks', score', pus0, prts0) =
        foldl' (\(bs, bk, sc, pu, pr) ball ->
          let moved  = setSpeed spd (wallBounce (paddleBounce (mPX m0) (mPW m0) (moveBall dt ball)))
              (b', bk', sc', pu', pr') = collideBricks (mFire m0) moved bk
          in ( if bY b' + ballR < cH then b' : bs else bs
             , bk', sc + sc', pu ++ pu', pr ++ pr' ))
          ([], mBrks m0, 0, [], []) (mBalls m0)
      -- Move existing PUs + new drops
      pus1    = map (\pu -> pu { puY = puY pu + puSpeed * dt }) (mPUs m0 ++ pus0)
      -- Collect PUs that hit paddle
      mBase   = m0 { mBrks = brks', mScore = mScore m0 + score', mBalls = balls' }
      (pus2, m1, collPrts) = collectPUs pus1 mBase
      -- Step particles
      prts1   = mapMaybe (stepParticle dt) (mPrts m0 ++ prts0 ++ collPrts)
      m2      = m1 { mPUs = pus2, mPrts = prts1 }
      -- Ball loss / level complete
      m3      = if null balls' then loseLife m2 else m2
      m4      = if null (mBrks m3) && mPhase m3 == Playing
                then m3 { mPhase = LevelComplete, mHigh = max (mHigh m3) (mScore m3) }
                else m3
  in m4

collectPUs :: [PU] -> Model -> ([PU], Model, [Particle])
collectPUs pus m =
  let px = mPX m; pw = mPW m
      (keep, collect) = foldl' (\(k,c) pu ->
        let inX = puX pu + 42 > px && puX pu < px + pw
            inY = puY pu + 18 > paddleY && puY pu < paddleY + paddleH
            alive = puY pu < cH
        in if inX && inY then (k, pu:c)
           else if alive  then (pu:k, c)
           else (k, c))
        ([],[]) pus
      prts = concatMap (\pu -> spawnParticles (puX pu + 21) (puY pu + 9) BGold) collect
      m'   = foldl' (\acc pu -> applyPU (puKind pu) acc) m collect
  in (reverse keep, m', prts)

------------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------------
updateModel :: Action -> Effect ROOT () Model Action
updateModel = \case
  NoOp -> pure ()

  MouseX x ->
    modify $ \m ->
      m { mPX = clamp 0 (cW - mPW m) (x - mPW m / 2) }

  KeyPress keys ->
    modify $ \m -> case mPhase m of
      Menu    -> if IS.member 32 keys then resetLevel (m { mScore = 0, mLives = 3 }) 1 else m
      Playing -> if IS.member 80 keys || IS.member 27 keys then m { mPhase = Paused } else m
      Paused  -> if IS.member 80 keys || IS.member 32 keys then m { mPhase = Playing } else m
      LevelComplete ->
        if IS.member 32 keys
        then let nl = mLevel m + 1
             in if nl > numLevels
                then m { mPhase = Victory, mHigh = max (mHigh m) (mScore m) }
                else resetLevel m nl
        else m
      GameOver -> if IS.member 32 keys then initModel { mHigh = mHigh m } else m
      Victory  -> if IS.member 32 keys then initModel { mHigh = mHigh m } else m

  Tick ts ->
    modify $ \m ->
      let dt = min 0.05 ((ts - mLast m) / 1000)
          m' = m { mLast = ts }
      in case mPhase m of
           Playing -> stepGame dt m'
           _       -> m'

------------------------------------------------------------------------------
-- Canvas Rendering Helpers
------------------------------------------------------------------------------
renderGame :: Model -> Canvas ()
renderGame m = do
  drawBg
  mapM_ drawBrick (mBrks m)
  mapM_ drawParticle (mPrts m)
  mapM_ drawPU (mPUs m)
  mapM_ (drawBall (mFire m)) (mBalls m)
  drawPaddle (mPX m) (mPW m)
  drawHUD m
  drawOverlay m

-- Rounded rectangle path
rRect :: Double -> Double -> Double -> Double -> Double -> Canvas ()
rRect x y w h r = do
  beginPath ()
  moveTo (x + r, y)
  lineTo (x + w - r, y)
  arcTo (x + w, y, x + w, y + r, r)
  lineTo (x + w, y + h - r)
  arcTo (x + w, y + h, x + w - r, y + h, r)
  lineTo (x + r, y + h)
  arcTo (x, y + h, x, y + h - r, r)
  lineTo (x, y + r)
  arcTo (x, y, x + r, y, r)
  closePath ()

------------------------------------------------------------------------------
-- Drawing Functions
------------------------------------------------------------------------------
drawBg :: Canvas ()
drawBg = do
  g <- createLinearGradient (0, 0, 0, cH)
  addColorStop (0.0, RGB  8  8 28) g
  addColorStop (0.5, RGB 12  8 45) g
  addColorStop (1.0, RGB  5  5 20) g
  fillStyle (gradient g)
  fillRect (0, 0, cW, cH)
  strokeStyle (color (RGBA 55 55 115 0.11))
  lineWidth 0.5
  forM_ [0,40..760 :: Double] $ \x -> do
    beginPath (); moveTo (x,0); lineTo (x,cH); stroke ()
  forM_ [52,92..580 :: Double] $ \y -> do
    beginPath (); moveTo (0,y); lineTo (cW,y); stroke ()
  forM_ stars $ \(sx,sy,sz) -> do
    fillStyle (color (RGBA 200 215 255 (0.35 + sz * 0.25)))
    beginPath ()
    arc (sx, sy, sz, 0, 2 * pi)
    fill ()
  strokeStyle (color (RGBA 75 95 255 0.32))
  lineWidth 2
  strokeRect (1, 1, cW - 2, cH - 2)

bkGrads :: BCol -> (Color, Color)
bkGrads = \case
  BRed    -> (RGB 255 85 85, RGB 195 25 25)
  BOrange -> (RGB 255 170 50, RGB 205 110 0)
  BYellow -> (RGB 255 245 80, RGB 200 185 0)
  BGreen  -> (RGB 80 225 90, RGB 20 165 35)
  BBlue   -> (RGB 85 165 255, RGB 20 85 225)
  BPurple -> (RGB 205 85 245, RGB 135 25 200)
  BCyan   -> (RGB 55 235 235, RGB 10 165 185)
  BGold   -> (RGB 255 220 55, RGB 195 155 0)
  BSilver -> (RGB 200 215 235, RGB 140 155 185)

drawBrick :: Brick -> Canvas ()
drawBrick brk = do
  let x = bkLeft brk; y = bkTop brk
      (c1, c2) = bkGrads (bkCol brk)
  shadowBlur 5
  shadowColor (RGBA 0 0 0 0.45)
  shadowOffsetX 2; shadowOffsetY 3
  g <- createLinearGradient (x, y, x, y + bkH)
  addColorStop (0.0, c1) g
  addColorStop (1.0, c2) g
  fillStyle (gradient g)
  rRect x y bkW bkH 4
  fill ()
  shadowBlur 0; shadowOffsetX 0; shadowOffsetY 0
  gG <- createLinearGradient (x, y, x, y + bkH * 0.5)
  addColorStop (0.0, RGBA 255 255 255 0.32) gG
  addColorStop (1.0, RGBA 255 255 255 0.0) gG
  fillStyle (gradient gG)
  rRect (x+2) (y+2) (bkW-4) (bkH*0.55) 3
  fill ()
  strokeStyle (color (RGBA 255 255 255 0.22))
  lineWidth 1
  rRect x y bkW bkH 4
  stroke ()
  when (bkHit brk >= 2) $ do
    fillStyle (color (RGBA 255 255 255 0.65))
    font "bold 9px monospace"
    textAlign TextAlignCenter
    textBaseline TextBaselineMiddle
    fillText (ms (bkHit brk), x + bkW/2, y + bkH/2)

drawBall :: Bool -> Ball -> Canvas ()
drawBall fire b = do
  let x = bX b; y = bY b
  shadowBlur 20
  shadowColor (if fire then RGBA 255 140 0 0.9 else RGBA 100 180 255 0.85)
  shadowOffsetX 0; shadowOffsetY 0
  g <- createRadialGradient (x - ballR*0.3, y - ballR*0.3, ballR*0.1, x, y, ballR)
  if fire
  then do addColorStop (0.0, RGB 255 255 200) g
          addColorStop (0.4, RGB 255 160  30) g
          addColorStop (1.0, RGB 200  60   0) g
  else do addColorStop (0.0, RGB 255 255 255) g
          addColorStop (0.4, RGB 190 225 255) g
          addColorStop (1.0, RGB  80 145 255) g
  fillStyle (gradient g)
  beginPath ()
  arc (x, y, ballR, 0, 2 * pi)
  fill ()
  shadowBlur 0

drawPaddle :: Double -> Double -> Canvas ()
drawPaddle px pw = do
  let py = paddleY; ph = paddleH
  shadowBlur 14
  shadowColor (RGBA 100 200 255 0.55)
  shadowOffsetX 0; shadowOffsetY 0
  g <- createLinearGradient (px, py, px, py + ph)
  addColorStop (0.0, RGB 215 235 255) g
  addColorStop (0.3, RGB 120 185 255) g
  addColorStop (0.7, RGB  60 125 225) g
  addColorStop (1.0, RGB  20  65 185) g
  fillStyle (gradient g)
  rRect px py pw ph 6
  fill ()
  shadowBlur 0
  gG <- createLinearGradient (px, py, px, py + ph * 0.5)
  addColorStop (0.0, RGBA 255 255 255 0.45) gG
  addColorStop (1.0, RGBA 255 255 255 0.0) gG
  fillStyle (gradient gG)
  rRect (px+2) (py+2) (pw-4) (ph*0.5) 4
  fill ()
  strokeStyle (color (RGBA 180 220 255 0.72))
  lineWidth 1
  rRect px py pw ph 6
  stroke ()

drawPU :: PU -> Canvas ()
drawPU pu = do
  let x = puX pu; y = puY pu
      (r,g,b) = puRGB (puKind pu)
  shadowBlur 10
  shadowColor (RGB r g b)
  fillStyle (color (RGBA r g b 0.9))
  rRect x y 42 18 4
  fill ()
  shadowBlur 0
  strokeStyle (color (RGBA 255 255 255 0.5))
  lineWidth 1
  rRect x y 42 18 4
  stroke ()
  fillStyle (color (RGB 255 255 255))
  font "bold 9px monospace"
  textAlign TextAlignCenter
  textBaseline TextBaselineMiddle
  fillText (puLabel (puKind pu), x + 21, y + 9)

drawParticle :: Particle -> Canvas ()
drawParticle p = do
  let a = pLife p
  globalAlpha (a * a)
  fillStyle (color (RGB (pR p) (pG p) (pB p)))
  beginPath ()
  arc (pX p, pY p, pSz p * a + 0.5, 0, 2 * pi)
  fill ()
  globalAlpha 1

drawHUD :: Model -> Canvas ()
drawHUD m = do
  fillStyle (color (RGBA 0 0 0 0.55))
  fillRect (0, 0, cW, 50)
  strokeStyle (color (RGBA 65 105 255 0.38))
  lineWidth 1
  beginPath (); moveTo (0,50); lineTo (cW,50); stroke ()
  -- Score
  shadowBlur 8; shadowColor (RGBA 255 200 60 0.5)
  fillStyle (color (RGB 255 220 80))
  font "bold 18px 'Courier New', monospace"
  textAlign TextAlignLeft; textBaseline TextBaselineMiddle
  fillText ("SCORE " <> ms (mScore m), 16, 25)
  shadowBlur 0
  -- Level
  shadowBlur 6; shadowColor (RGBA 100 200 255 0.5)
  fillStyle (color (RGB 120 200 255))
  textAlign TextAlignCenter
  fillText ("LEVEL " <> ms (mLevel m), cW / 2, 25)
  shadowBlur 0
  -- Lives
  fillStyle (color (RGB 255 85 85))
  font "bold 18px 'Courier New', monospace"
  textAlign TextAlignRight
  fillText ("LIVES " <> ms (mLives m), cW - 16, 25)
  -- Sub-labels
  fillStyle (color (RGBA 165 165 200 0.7))
  font "11px 'Courier New', monospace"
  textAlign TextAlignCenter
  fillText ("BEST " <> ms (mHigh m), cW / 2, 40)
  when (mFire m) $ do
    fillStyle (color (RGB 255 160 30))
    font "bold 10px monospace"
    textAlign TextAlignRight
    fillText ("FIRE", cW - 16, 40)
  when (mWide m > 0) $ do
    fillStyle (color (RGB 30 200 255))
    font "bold 10px monospace"
    textAlign TextAlignLeft
    fillText ("WIDE", 16, 40)

drawOverlay :: Model -> Canvas ()
drawOverlay m = case mPhase m of
  Menu          -> menuScreen
  Paused        -> pauseScreen
  LevelComplete -> levelOKScreen (mLevel m) (mScore m)
  GameOver      -> gameOverScreen (mScore m) (mHigh m)
  Victory       -> victoryScreen (mScore m) (mHigh m)
  Playing       -> pure ()

darkOverlay :: Canvas ()
darkOverlay = do
  fillStyle (color (RGBA 0 0 15 0.82))
  fillRect (0, 0, cW, cH)

glowTitle :: MisoString -> Int -> Int -> Int -> Canvas ()
glowTitle txt r g b = do
  shadowBlur 32; shadowColor (RGBA r g b 0.9)
  fillStyle (color (RGB r g b))
  font "bold 60px 'Courier New', monospace"
  textAlign TextAlignCenter; textBaseline TextBaselineMiddle
  fillText (txt, cW / 2, 188)
  shadowBlur 0

subLine :: MisoString -> Double -> Canvas ()
subLine txt y = do
  fillStyle (color (RGBA 200 205 235 0.88))
  font "16px 'Courier New', monospace"
  textAlign TextAlignCenter
  fillText (txt, cW / 2, y)

promptLine :: MisoString -> Double -> Canvas ()
promptLine txt y = do
  shadowBlur 10; shadowColor (RGBA 255 220 70 0.65)
  fillStyle (color (RGB 255 218 55))
  font "bold 19px 'Courier New', monospace"
  textAlign TextAlignCenter
  fillText (txt, cW / 2, y)
  shadowBlur 0

menuScreen :: Canvas ()
menuScreen = do
  darkOverlay
  glowTitle "BREAKOUT" 120 200 255
  fillStyle (color (RGB 155 215 255))
  font "20px 'Courier New', monospace"
  textAlign TextAlignCenter; textBaseline TextBaselineMiddle
  fillText ("MacBrickout Edition", cW / 2, 258)
  subLine "Move mouse to control the paddle" 328
  subLine "Destroy all bricks to advance" 354
  subLine "P = Pause   |   SPACE = Start" 380
  subLine "5 levels  |  5 power-up types  |  particles" 406
  promptLine ">>> PRESS SPACE TO PLAY <<<" 462

pauseScreen :: Canvas ()
pauseScreen = do
  darkOverlay
  glowTitle "PAUSED" 160 210 255
  subLine "Press P or SPACE to continue" (cH/2 + 48)

levelOKScreen :: Int -> Int -> Canvas ()
levelOKScreen n score = do
  darkOverlay
  glowTitle ("LEVEL " <> ms n <> " CLEAR!") 80 255 120
  fillStyle (color (RGB 255 225 80))
  font "bold 26px 'Courier New', monospace"
  textAlign TextAlignCenter; textBaseline TextBaselineMiddle
  fillText ("SCORE: " <> ms score, cW / 2, 278)
  let next = if n >= numLevels then "Final level reached!" else "Next: Level " <> ms (n + 1)
  subLine next 348
  promptLine "PRESS SPACE" 430

gameOverScreen :: Int -> Int -> Canvas ()
gameOverScreen score hi = do
  fillStyle (color (RGBA 20 0 0 0.88))
  fillRect (0, 0, cW, cH)
  glowTitle "GAME OVER" 255 80 80
  fillStyle (color (RGB 255 205 80))
  font "bold 26px 'Courier New', monospace"
  textAlign TextAlignCenter; textBaseline TextBaselineMiddle
  fillText ("SCORE: " <> ms score, cW / 2, 278)
  fillStyle (color (RGB 190 190 255))
  font "20px 'Courier New', monospace"
  fillText ("BEST: " <> ms hi, cW / 2, 325)
  promptLine "PRESS SPACE to play again" 432

victoryScreen :: Int -> Int -> Canvas ()
victoryScreen score hi = do
  fillStyle (color (RGBA 0 8 20 0.88))
  fillRect (0, 0, cW, cH)
  glowTitle "YOU WIN!" 255 225 40
  fillStyle (color (RGB 100 255 150))
  font "22px 'Courier New', monospace"
  textAlign TextAlignCenter; textBaseline TextBaselineMiddle
  fillText ("ALL 5 LEVELS CLEARED!", cW / 2, 255)
  fillStyle (color (RGB 255 220 80))
  font "bold 26px 'Courier New', monospace"
  fillText ("FINAL SCORE: " <> ms score, cW / 2, 308)
  fillStyle (color (RGB 190 190 255))
  font "20px 'Courier New', monospace"
  fillText ("BEST: " <> ms hi, cW / 2, 352)
  promptLine "PRESS SPACE to play again" 445

------------------------------------------------------------------------------
-- CSS Stylesheet
------------------------------------------------------------------------------
gameCSS :: StyleSheet
gameCSS = sheet_
  [ selector_ "*"
      [ CSS.margin "0", CSS.padding "0", CSS.boxSizing "border-box" ]
  , selector_ "body"
      [ CSS.backgroundColor (RGB 5 5 15)
      , CSS.display "flex"
      , CSS.justifyContent "center"
      , CSS.alignItems "center"
      , CSS.width (pct 100)
      , CSS.height (vh 100)
      , CSS.overflow "hidden"
      ]
  , selector_ "#game-canvas"
      [ CSS.display "block"
      , CSS.cursor "none"
      ]
  ]

------------------------------------------------------------------------------
-- View
------------------------------------------------------------------------------
viewModel :: () -> Model -> View Model Action
viewModel _ m =
  H.div_ []
    [ canvas
        [ P.id_ "game-canvas"
        , P.width_ (ms (800 :: Int))
        , P.height_ (ms (600 :: Int))
        , onPointerMove (MouseX . fst . offset)
        ]
        (\_ -> pure ())
        (\() -> renderGame m)
    ]

------------------------------------------------------------------------------
-- App
------------------------------------------------------------------------------
gameEvents :: Events
gameEvents = defaultEvents <> M.fromList [("pointermove", BUBBLE)]

app :: App Model Action
app = (component initModel updateModel viewModel)
  { subs   = [animSub Tick, keyboardSub KeyPress]
  , styles = [Sheet gameCSS]
  }

------------------------------------------------------------------------------
-- Entry Point
------------------------------------------------------------------------------
main :: IO ()
#ifdef INTERACTIVE
main = live gameEvents app
#else
main = startApp gameEvents app
#endif

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
------------------------------------------------------------------------------
