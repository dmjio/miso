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

{-# LANGUAGE OverloadedStrings #-}
module TestBindingsApp where

import Miso
    ( App
    , View
    , Effect
    , ROOT
    , component
    , text
    , Component
    , toMisoString
    )
import qualified Miso.Html as M
import Miso.Lens (Lens (..))

type Action = ()

type MainComponent = App Int Action

app :: Int -> MainComponent
app nnodes = component initialModel update view
    where
        initialModel :: Int
        initialModel = nnodes

        update :: a -> Effect ROOT Int Action
        update = const $ return ()

        view :: Int -> View Int Action
        view _ = M.div_ [] [ text "Hello World" ]
            -- todo: view should map over the sub-components


data Model = Model
    { valueA :: Int
    , valueB :: Int
    }

valueALens :: Lens Model Int
valueALens = Lens valueA (\x m -> m { valueA = x } )

valueBLens :: Lens Model Int
valueBLens = Lens valueB (\x m -> m { valueB = x } )

type AppAction = ()
type AppModel = Model
type AppComponent = Component MainComponent AppModel AppAction

innerApp :: AppComponent
innerApp = component initialModel update view
    where
        initialModel :: AppModel
        initialModel = Model 0 0

        update :: AppAction -> Effect MainComponent AppModel AppAction
        update = const $ return ()

        view :: AppModel -> View AppModel AppAction
        view m =
            M.div_ []
                [ M.div_ []
                    [ text (toMisoString $ show $ valueA m)
                    ]
                , M.div_ []
                    [ text (toMisoString $ show $ valueB m)
                    ]
                ]
