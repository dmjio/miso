----------------------------------------------------------------------------
-- | The benchmark frame, written once against 'Draw'.
--
-- The same definition is executed through "Miso.Canvas" (the 'Run'
-- instance, @?mode=run@) and compiled to a JavaScript frame function (the
-- 'JSGen' instance, @?mode=staged@, generated at compile time by a splice
-- in "Main").
module Scene (benchScene) where
----------------------------------------------------------------------------
import Miso.Canvas.Draw
----------------------------------------------------------------------------
-- | Clear, drift the whole layout with the time, draw every entity with
-- its own colour, then the fps counter.
benchScene :: (Draw r, Fractional (r Double)) => r ()
benchScene =
     fillStyleRGB 20 20 40
  >. fillRect 0 0 800 600
  >. save
  >. translate (fmod_ (t * 0.05) 40 - 20) (fmod_ (t * 0.03) 40 - 20)
  >. forEntities (\e ->
          fillStyleRGB (er e) (eg e) (eb e)
       >. fillRect (ex e) (ey e) (esize e) (esize e))
  >. restore
  >. fillStyleRGB 255 255 255
  >. font "16px monospace"
  >. fillText (cat [str "fps: ", showFixed 1 (input Fps), str "  rects: ", showFixed 0 (input Count)]) 10 24
  where
    t = input Time
----------------------------------------------------------------------------
