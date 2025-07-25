{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
{-# LANGUAGE CPP                #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
-- import           Control.Monad
-- import           Language.Javascript.JSaddle ((!), (#), jsg)
-----------------------------------------------------------------------------
import           Miso hiding (width_, height_)
import           Miso.String
import           Miso.Lens
import           Miso.Svg.Property hiding (id_)
import           Miso.Svg.Element hiding (text_)
import qualified Miso.Svg.Element as SVG hiding (view_)
-- import qualified Miso.Html.Element as HTML
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startApp test
  { styles =
    [ Href "https://cdn.jsdelivr.net/npm/basecoat-css@0.2.8/dist/basecoat.cdn.min.css"
    ]
  , scripts =
      [ Src "https://cdn.jsdelivr.net/npm/basecoat-css@0.2.8/dist/js/all.min.js"
      , Src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"
      ]
  }
-----------------------------------------------------------------------------
data Action = Tog
-----------------------------------------------------------------------------
-- | tests component swap, and key case
app :: App Bool Action
app = vcomp
  where
    update_ Tog = _id %= not
    vcomp = component False update_ $ \condition ->
       div_
       []
       [ button_
         [ onClick Tog
         ]
         [ "toggle"
         ]
       , if condition
         then
           p_ [] +> component (0 :: Double) noop $ \m ->
             div_
             []
             [ text $ "hey im: " <> ms m
             ]
         else
           "hey"
       ]
-----------------------------------------------------------------------------
-- alertDialog_ :: Component String () AlertDialog
-----------------------------------------------------------------------------
test :: Component parent Int action
test = undefined -- component 0 noop $ \_ -> p_ [] +> undefined
  -- div_
  -- []
  -- [ alert_
  -- , alertDialog_
  -- , avatar_
  -- , badge_
  -- , button__
  -- , card_
  -- , checkbox_
  -- , combobox_
  -- , tooltip_
  -- , textarea
  -- , popover
  -- ]
-----------------------------------------------------------------------------
alert_ :: View model action
alert_ = div_ [] +> component () noop $ \() ->
  div_
  [ class_ "alert"
  ]
  [ svg_
    [ xmlns_ "http://www.w3.org/2000/svg"
    , width_ "24"
    , height_ "24"
    , viewBox_ "0 0 24 24"
    , fill_ "none"
    , stroke_ "currentColor"
    , strokeWidth_ "2"
    , strokeLinecap_ "round"
    , strokeLinejoin_ "round"
    ]
    [ circle_
      [ cx_ "12"
      , cy_ "12"
      , r_ "10"
      ]
      [ SVG.path_
        [ d_ "m9 12 2 2 4-4"
        ] []
      ]
    ]
  , h2_ [] [ "Success! Your changes have been saved" ]
  , section_ [] [ "This is an alert with icon, title and description." ]
  ]
-----------------------------------------------------------------------------
data AlertDialog
  = FetchParent ()
  | Failed
-----------------------------------------------------------------------------
test_ :: Effect Int model AlertDialog
test_ = parent FetchParent Failed
-----------------------------------------------------------------------------
-- parent
--   :: forall parent model action
--    . (parent -> action)
--   -> action
--   -> Effect parent model action
-----------------------------------------------------------------------------
-- alertDialog_ :: Component Int () AlertDialog
-- alertDialog_ = component () update_ view__
--   where
--     update_
--       :: AlertDialog -> Effect Int model AlertDialog
--     update_ (ShowModal domRef) = do
--       parent FetchParent Failed
--       io_ $ do
--         dialogRef <- domRef ! ("nextSibling" :: MisoString)
--         void $ dialogRef # ("showModal" :: MisoString) $ ()

--     update_ CloseDialog = io_ $ do
--       dialog <- jsg @MisoString "document"
--         # ("getElementById" :: MisoString)
--         $ ["alert-dialog" :: MisoString]
--       void $ dialog # ("close" :: MisoString) $ ()

--     update_ _ = pure ()

--     view__ () =
--       div_
--       []
--       [ button_
--         [ type_ "button"
--         , class_ "btn-outline"
--         , onClickWith ShowModal
--         ]
--         [ "Open alert dialog"
--         ]
--       , dialog_
--           [ id_ "alert-dialog"
--           , class_ "dialog"
--           , aria_ "labelledby" "alert-dialog-title"
--           , aria_ "describedby" "alert-dialog-description"
--           ]
--           [ article_ []
--             [ header_ []
--               [ h2_
--                 [ id_ "alert-dialog-title"
--                 ]
--                 [ "Are you absolutely sure?"
--                 ]
--               , p_
--                 [ id_ "alert-dialog-description"
--                 ]
--                 [ text_
--                   [ "This action cannot be undone. "
--                   , "This will permanently delete your"
--                   , "account and remove your data from our servers."
--                   ]
--                 ]
--               ]
--             , footer_ []
--               [ button_
--                 [ class_ "btn-outline"
--                 , onClick CloseDialog
--                 ] [ "Cancel" ]
--               , button_
--                 [ class_ "btn-primary"
--                 , onClick CloseDialog
--                 ] [ "Continue" ]
--               ]
--             ]
--           ]
--         ]
-- -----------------------------------------------------------------------------
-- avatar_ :: View model action
-- avatar_ =
--   img_
--     [ class_ "size-8 shrink-0 object-cover rounded-full"
--     , alt_ "@hunvreus"
--     , src_ "https://github.com/hunvreus.png"
--     ]
-- -----------------------------------------------------------------------------
-- badge_ :: View model action
-- badge_ = span_ [ class_ "badge" ][ "Badge" ]
-- -----------------------------------------------------------------------------
-- button__ :: View model action
-- button__ = button_ [ class_ "btn" ] [ "Button" ]
-- -----------------------------------------------------------------------------
-- card_ :: View model action
-- card_ =
--   div_
--   [ class_ "card w-full" ]
--   [ header_ []
--     [ h2_ [][ "Login to your account" ]
--     , p_ [][ "Enter your details below to login to your account" ]
--     ]
--   , section_ []
--     [ HTML.form
--       [ class_ "form grid gap-6"
--       ]
--       [ div_
--         [ class_ "grid gap-2" ]
--         [ label_ [ for_ "demo-card-form-email" ][ "Email" ]
--         , input_
--           [ type_ "email"
--           , id_ "demo-card-form-email"
--           ]
--         ]
--       , div_ [ class_ "grid gap-2" ]
--         [ div_ [ class_ "flex items-center gap-2" ]
--           [ label_ [ for_ "demo-card-form-password" ][ "Password" ]
--           , a_
--             [ href_ "#"
--             , class_ "ml-auto inline-block text-sm underline-offset-4 hover:underline"
--             ] [ "Forgot your password?" ]
--           ]
--         , input_
--           [ type_ "password"
--           , id_ "demo-card-form-password"
--           ]
--         ]
--       ]
--     ]
--   , footer_ [ class_ "flex flex-col items-center gap-2" ]
--     [ button_
--       [ type_ "button"
--       , class_ "btn w-full"
--       ] [ "Login" ]
--     , button_
--       [ type_ "button"
--       , class_ "btn-outline w-full"
--       ] [ "Login with Google" ]
--     , p_ [ class_ "mt-4 text-center text-sm" ]
--       [ "Don't have an account?"
--       , a_
--         [ href_ "#"
--         , class_ "underline-offset-4 hover:underline"
--         ] [ "Sign up" ]
--       ]
--     ]
--   ]
-- -----------------------------------------------------------------------------
-- checkbox_ :: View model action
-- checkbox_ =
--   label_
--   [ class_ "label gap-3" ]
--   [ input_
--     [ type_ "checkbox"
--     , class_ "input"
--     ]
--   , "Accept terms and conditions"
--   ]
-- -----------------------------------------------------------------------------
-- combobox_ :: View model action
-- combobox_ = div_
--     [ id_ "select-909078"
--     , class_ "select"
--     ]
--     [ button_
--         [ type_ "button"
--         , class_ "btn-outline justify-between font-normal w-[200px]"
--         , id_ "select-909078-trigger"
--         , textProp "aria-haspopup" "listbox"
--         , textProp "aria-expanded" "false"
--         , textProp "aria-controls" "select-909078-listbox"
--         ]
--         [ span_
--             [ class_ "truncate" ][]
--         , svg_
--             [ xmlns_ "http://www.w3.org/2000/svg"
--             , width_ "24"
--             , height_ "24"
--             , viewBox_ "0 0 24 24"
--             , fill_ "none"
--             , stroke_ "currentColor"
--             , textProp "stroke-width" "2"
--             , textProp "stroke-linecap" "round"
--             , textProp "stroke-linejoin" "round"
--             , class_ "lucide lucide-chevrons-up-down-icon lucide-chevrons-up-down text-muted-foreground opacity-50 shrink-0"
--             ][]
--         ]
--     , div_
--         [ id_ "select-909078-popover"
--         , textProp "aria-hidden" "true"
--         ]
--         [ header_ []
--             [ svg_
--                 [ xmlns_ "http://www.w3.org/2000/svg"
--                 , width_ "24"
--                 , height_ "24"
--                 , viewBox_ "0 0 24 24"
--                 , fill_ "none"
--                 , stroke_ "currentColor"
--                 , textProp "stroke-width" "2"
--                 , textProp "stroke-linecap" "round"
--                 , textProp "stroke-linejoin" "round"
--                 , class_ "lucide lucide-search-icon lucide-search"
--                 ][]
--             , input_
--                 [ type_ "text"
--                 , value_ ""
--                 , placeholder_ "Search framework..."
--                 , autocomplete_ False
--                 , autocorrect_ False
--                 , spellcheck_ False
--                 , aria_ "autocomplete" "list"
--                 , role_ "combobox"
--                 , aria_ "expanded" "false"
--                 , aria_ "controls" "select-909078-listbox"
--                 , aria_ "labelledby" "select-909078-trigger"
--                 ]
--             ]
--         , div_
--             [ role_ "listbox"
--             , id_ "select-909078-listbox"
--             , textProp "aria-orientation" "vertical"
--             , textProp "aria-labelledby" "select-909078-trigger"
--             , textProp "data-empty" "No framework found."
--             ]
--             [ div_
--                 [ role_ "option"
--                 , textProp "data-value" "Next.js"
--                 ][ "Next.js" ]
--             , div_
--                 [ role_ "option"
--                 , textProp "data-value" "SvelteKit"
--                 ][ "SvelteKit" ]
--             , div_
--                 [ role_ "option"
--                 , textProp "data-value" "Nuxt.js"
--                 ][ "Nuxt.js" ]
--             , div_
--                 [ role_ "option"
--                 , textProp "data-value" "Remix"
--                 ][ "Remix" ]
--             , div_
--                 [ role_ "option"
--                 , textProp "data-value" "Astro"
--                 ][ "Astro" ]
--             ]
--         ]
--     , input_
--         [ type_ "hidden"
--         , name_ "select-909078-value"
--         , value_ ""
--         ]
--     ]

-- tooltip_ :: View parent action
-- tooltip_ =
--   button_
--   [ class_ "btn-outline"
--   , textProp "data-tooltip" "Default tooltip"
--   ]
--   [ "Default"
--   ]

-- textarea :: View parent action
-- textarea =
--   textarea_
--   [ class_ "textarea"
--   , placeholder_ "Type your message here"
--   ]
--   []

-- table :: View model action
-- table = div_
--     [ class_ "overflow-x-auto" ]
--     [ table_
--         [ class_ "table" ]
--         [ caption_ [][ "A list of your recent invoices." ]
--         , thead_ []
--             [ tr_ []
--                 [ th_ [][ "Invoice" ]
--                 , th_ [][ "Status" ]
--                 , th_ [][ "Method" ]
--                 , th_ [][ "Amount" ]
--                 ]
--             ]
--         , tbody_ []
--             [ tr_ []
--                 [ td_
--                     [ class_ "font-medium" ][ "INV001" ]
--                 , td_ [][ "Paid" ]
--                 , td_ [][ "Credit Card" ]
--                 , td_
--                     [ class_ "text-right" ][ "$250.00" ]
--                 ]
--             , tr_ []
--                 [ td_
--                     [ class_ "font-medium" ][ "INV002" ]
--                 , td_ [][ "Pending" ]
--                 , td_ [][ "PayPal" ]
--                 , td_
--                     [ class_ "text-right" ][ "$150.00" ]
--                 ]
--             , tr_ []
--                 [ td_
--                     [ class_ "font-medium" ][ "INV003" ]
--                 , td_ [][ "Unpaid" ]
--                 , td_ [][ "Bank Transfer" ]
--                 , td_
--                     [ class_ "text-right" ][ "$350.00" ]
--                 ]
--             , tr_ []
--                 [ td_
--                     [ class_ "font-medium" ][ "INV004" ]
--                 , td_ [][ "Paid" ]
--                 , td_ [][ "Paypal" ]
--                 , td_
--                     [ class_ "text-right" ][ "$450.00" ]
--                 ]
--             , tr_ []
--                 [ td_
--                     [ class_ "font-medium" ][ "INV005" ]
--                 , td_ [][ "Paid" ]
--                 , td_ [][ "Credit Card" ]
--                 , td_
--                     [ class_ "text-right" ][ "$550.00" ]
--                 ]
--             , tr_ []
--                 [ td_
--                     [ class_ "font-medium" ][ "INV006" ]
--                 , td_ [][ "Pending" ]
--                 , td_ [][ "Bank Transfer" ]
--                 , td_
--                     [ class_ "text-right" ][ "$200.00" ]
--                 ]
--             , tr_ []
--                 [ td_
--                     [ class_ "font-medium" ][ "INV007" ]
--                 , td_ [][ "Unpaid" ]
--                 , td_ [][ "Credit Card" ]
--                 , td_
--                     [ class_ "text-right" ][ "$300.00" ]
--                 ]
--             ]
--         , tfoot_ []
--             [ tr_ []
--                 [ td_
--                     [ colspan_ "3" ][ "Total" ]
--                 , td_
--                     [ class_ "text-right" ][ "$2,500.00" ]
--                 ]
--             ]
--         ]
--     ]

-- switch_ :: View model action
-- switch_ = label_
--   [ class_ "label" ]
--   [ input_
--     [ type_ "checkbox"
--     , name_ "switch"
--     , role_ "switch"
--     , class_ "input"
--     ]
--   , "Airplane Mode"
--   ]

-- select :: View model action
-- select = select_
--   [ class_ "select w-[180px]" ]
--   [ optgroup_
--       [ textProp "label" "Fruits"
--       ]
--       [ option_ []  [ "Apple" ]
--       , option_ []  [ "Banana" ]
--       , option_ []  [ "Blueberry" ]
--       , option_ []  [ "Grapes" ]
--       , option_ []  [ "Pineapple" ]
--       ]
--   ]

-- radioGroup :: View model action
-- radioGroup = fieldset_
--     [ class_ "grid gap-3" ]
--     [ label_
--         [ class_ "label" ]
--         [ input_
--             [ type_ "radio"
--             , name_ "radio-group"
--             , value_ "default"
--             , class_ "input"
--             ]
--         , "Default"
--         ]
--     , label_
--         [ class_ "label" ]
--         [ input_
--             [ type_ "radio"
--             , name_ "radio-group"
--             , value_ "comfortable"
--             , class_ "input"
--             , checked_ True
--             ]
--         , "Comfortable"
--         ]
--     , label_
--         [ class_ "label" ]
--         [ input_
--             [ type_ "radio"
--             , name_ "radio-group"
--             , value_ "compact"
--             , class_ "input"
--             ]
--         , "Compact"
--         ]
--     ]

-- popover :: View model action
-- popover = div_
--     [ id_ "demo-popover"
--     , class_ "popover"
--     ]
--     [ button_
--         [ id_ "demo-popover-trigger"
--         , type_ "button"
--         , textProp "aria-expanded" "false"
--         , textProp "aria-controls" "demo-popover-popover"
--         , class_ "btn-outline"
--         ][ "Open popover" ]
--     , div_
--         [ id_ "demo-popover-popover"
--         , textProp "data-popover" ""
--         , textProp "aria-hidden" "true"
--         , class_ "w-80"
--         ]
--         [ div_
--             [ class_ "grid gap-4" ]
--             [ header_
--                 [ class_ "grid gap-1.5" ]
--                 [ h4_
--                     [ class_ "leading-none font-medium" ][ "Dimensions" ]
--                 , p_
--                     [ class_ "text-muted-foreground text-sm" ][ "Set the dimensions for the layer." ]
--                 ]
--             , form
--                 [ class_ "form grid gap-2" ]
--                 [ div_
--                     [ class_ "grid grid-cols-3 items-center gap-4" ]
--                     [ label_
--                         [ for_ "demo-popover-width" ][ "Width" ]
--                     , input_
--                         [ type_ "text"
--                         , id_ "demo-popover-width"
--                         , value_ "100%"
--                         , class_ "col-span-2 h-8"
--                         , textProp "autofocus" ""
--                         ]
--                     , ">"
--                     ]
--                 , div_
--                     [ class_ "grid grid-cols-3 items-center gap-4" ]
--                     [ label_
--                         [ for_ "demo-popover-max-width" ][ "Max. width" ]
--                     , input_
--                         [ type_ "text"
--                         , id_ "demo-popover-max-width"
--                         , value_ "300px"
--                         , class_ "col-span-2 h-8"
--                         ]
--                     , ">"
--                     ]
--                 , div_
--                     [ class_ "grid grid-cols-3 items-center gap-4" ]
--                     [ label_
--                         [ for_ "demo-popover-height" ][ "Height" ]
--                     , input_
--                         [ type_ "text"
--                         , id_ "demo-popover-height"
--                         , value_ "25px"
--                         , class_ "col-span-2 h-8"
--                         ]
--                     , ">"
--                     ]
--                 , div_
--                     [ class_ "grid grid-cols-3 items-center gap-4" ]
--                     [ label_
--                         [ for_ "demo-popover-max-height" ][ "Max. height" ]
--                     , input_
--                         [ type_ "text"
--                         , id_ "demo-popover-max-height"
--                         , value_ "none"
--                         , class_ "col-span-2 h-8"
--                         ]
--                     , ">"
--                     ]
--                 ]
--             ]
--         ]
--     ]
