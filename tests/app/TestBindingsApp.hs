{-
 -
 -
 - Generate a cyclical grpah: A -> B -> C -> A
 -      Let's say A gets clicked, changes A.a,
 -      B is bound to A.a, C is bound to B.a,
 -      C is bound to A.b.
 -          - does binding C and A happen in A or in C?
 -
 - Click gets caught in A's update
 -      - after state is changed, use notify to schedule an Action
 -      - in that update (for the next action, still in A),
 -          assert that A.b has been changed to A.a
 -}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TestBindingsApp where

import Miso
    ( View
    -- , App
    , Effect
    -- , ROOT
    , component
    , text
    , Component
    , toMisoString
    , mount
    )
import qualified Miso.Html as M
import Miso.Lens (Lens (..))
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

type Action = ()

-- type MainComponent = App Int Action
-- 
-- app :: Int -> MainComponent
-- app nnodes = component initialModel update view
--     where
--         initialModel :: Int
--         initialModel = nnodes
-- 
--         update :: a -> Effect ROOT Int Action
--         update = const $ return ()
-- 
--         view :: Int -> View Int Action
--         --view n = M.div_ [] [ text "Hello World" ]
--         view n = M.div_ [] (map (mount . getApp) [0..n])
--             where
--                 getApp :: Int -> AppComponent
--                 getApp = const innerApp
--             -- todo: view should map over the sub-components


data Model = Model
    { valueA :: Int
    , valueB :: Int
    } deriving (Generic, ToJSON, FromJSON, Eq)

valueALens :: Lens Model Int
valueALens = Lens valueA (\x m -> m { valueA = x } )

valueBLens :: Lens Model Int
valueBLens = Lens valueB (\x m -> m { valueB = x } )

type AppAction = ()
type AppModel = Model
type AppComponent a = Component a AppModel AppAction

innerApp :: Int -> AppComponent a
innerApp idx = component initialModel update view
    where
        initialModel :: AppModel
        initialModel = Model 0 0

        update :: AppAction -> Effect a AppModel AppAction
        update = const $ return ()

        view :: AppModel -> View AppModel AppAction
        view m =
            M.div_ []
                ( modelElems m
                ++
                [ mount $ innerApp idx ]
                )

        modelElems :: AppModel -> [ View AppModel AppAction ]
        modelElems m =
            [ M.div_ []
                [ text (toMisoString $ show $ valueA m)
                ]
            , M.div_ []
                [ text (toMisoString $ show $ valueB m)
                ]
            ]
