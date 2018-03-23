{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE CPP                  #-}
module Common where

import           Data.Bool
import qualified Data.Map    as M
import           Data.Monoid
import           Data.Proxy
import           Servant.API
#if MIN_VERSION_servant(0,10,0)
import Servant.Utils.Links
#endif

import           Miso
import           Miso.String

-- | We can pretty much share everything
--
-- model, action, view, router, links, events map
-- decoders are all shareable

-- | Model
data Model = Model
  { uri :: URI
  , navMenuOpen :: Bool
  } deriving (Show, Eq)

-- | Event Actions
data Action
  = Alert
  | ChangeURI URI
  | HandleURI URI
  | ToggleNavMenu
  | NoOp
  deriving (Show, Eq)

-- | Router
type ClientRoutes = Examples
  :<|> Docs
  :<|> Community
  :<|> Home

-- | Handlers
handlers ::
  (Model -> View Action)
    :<|> ((Model -> View Action)
        :<|> ((Model -> View Action) :<|> (Model -> View Action)))
handlers = examples
  :<|> docs
  :<|> community
  :<|> home

-- | Client Routes
type Examples  = "examples" :> View Action
type Docs      = "docs" :> View Action
type Community = "community" :> View Action
type Home      = View Action

-- | Views
community :: Model -> View Action
community = template v
  where
    v = div_ [ class_ $ pack "animated fadeIn" ] [ img_ [
           width_ $ pack "100"
         , class_ $ pack "animated bounceInDown"
         , src_ misoSrc
         ]
         , h1_ [ class_ $ pack "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "community" ]
       , h2_ [ class_ $ pack "subtitle animated pulse" ] [
            a_ [ href_ $ pack "https://haskell-miso-slack.herokuapp.com/"
               , target_ $ pack "_blank"
               ]
            [ text "Slack" ]
          , text " / "
          , a_ [ href_ $ pack "https://www.irccloud.com/invite?channel=%23haskell-miso&hostname=irc.freenode.net&port=6697&ssl=1"
               , target_ $ pack "_blank"
               ] [ text "#haskell-miso" ]
         ]
       ]

docs :: Model -> View Action
docs = template v
  where
    v = div_ [ class_ $ pack "animated fadeIn" ] [ img_ [
           width_ $ pack "100"
         , class_ $ pack "animated bounceInDown"
         , src_ misoSrc
         ]
         , h1_ [ class_ $ pack "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "docs" ]
        , h2_ [ class_ $ pack "subtitle animated pulse" ] [
            a_ [ href_ $ pack "https://haddocks.haskell-miso.org/"
               , target_ $ pack "_blank"
               ]
              [ text "Haddocks" ]
          , text " / "
          , a_ [ href_ $ pack "https://github.com/dmjio/miso/blob/master/README.md"
               , target_ $ pack "_blank"
               ]
            [ text "README" ]
         ]
       ]

misoSrc :: MisoString
misoSrc = pack "https://camo.githubusercontent.com/d6641458f09e24e8fef783de8278886949085960/68747470733a2f2f656d6f6a6970656469612d75732e73332e616d617a6f6e6177732e636f6d2f7468756d62732f3234302f6170706c652f39362f737465616d696e672d626f776c5f31663335632e706e67"

examples :: Model -> View Action
examples = template v
  where
    v =
     div_ [ class_ $ pack "animated fadeIn" ] [ img_ [
           width_ $ pack "100"
         , class_ $ pack "animated bounceInDown"
         , src_ misoSrc
         ]
         , h1_ [ class_ $ pack "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "examples" ]
       , h2_ [ class_ $ pack "subtitle animated pulse" ] [
            a_ [ target_ $ pack "_blank"
               , href_ $ pack "https://todo-mvc.haskell-miso.org/"
               ] [ text "TodoMVC" ]
          , text " / "
          , a_ [ target_ $ pack "_blank"
               , href_ $ pack "https://mario.haskell-miso.org/" ]
            [ text "Mario" ]
          , text " / "
          , a_ [ target_ $ pack "_blank"
               , href_ $ pack "https://flatris.haskell-miso.org/" ]
            [ text "Flatris" ]
         ]
       ]

home :: Model -> View Action
home = template v
  where
    v = div_ [class_ $ pack "animated fadeIn"] [ img_ [
           width_ $ pack "100"
         , class_ $ pack "animated bounceInDown"
         , src_ misoSrc
         ]
         , h1_ [ class_ $ pack "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "miso" ]
       , h2_ [ class_ $ pack "subtitle animated pulse" ] [
         text "A tasty "
         , a_ [ href_ $pack "https://www.haskell.org/"
              , target_ $ pack "_blank"][
             strong_ [] [text . pack $ "Haskell" ]]
         , text $ pack " front-end framework"
         ]
       ]


template :: View Action -> Model -> View Action
template content Model{..} =
  div_ [ ] [
   hero content uri navMenuOpen
  , middle
  , footer
  ]

middle =
  section_ [class_ $ pack  "hero" ] [
    div_ [class_ $ pack  "hero-body"] [
      div_ [class_ $ pack  "container"] [
        nav_ [class_ $ pack  "columns"] [
               a_ [ class_ $ pack  "column has-text-centered"
                   , href_ $ pack "https://medium.com/@localvoid/how-to-win-in-web-framework-benchmarks-8bc31af76ce7"
                   , target_ $ pack "_blank"
                   ] [
                  span_ [class_ $ pack  "icon is-large"] [
                      i_ [class_ $ pack  "fa fa-flash"] [ ]
                      ],
                  p_ [class_ $ pack  "title is-4"] [
                     strong_ [] [ text $ pack "Fast"]
                  ],
                  p_ [class_ $ pack  "subtitle"] [
                        text $ pack "Virtual DOM diffing algorithm"
                      ]
                  ]

              , a_ [ class_ $ pack  "column has-text-centered"
                   , href_ $ pack "/uhoh"
                   ] [
                  span_ [class_ $ pack  "icon is-large"] [
                      i_ [class_ $ pack  "fa fa-refresh"] [ ]
                      ],
                  p_ [class_ $ pack  "title is-4"] [
                     strong_ [] [ text $ pack "Isomorphic"]
                  ],
                  p_ [class_ $ pack  "subtitle"]
                      [ text $ pack "Seamless web experience, try to 404 (click here)" ]
                  ],
                  a_ [ class_ $ pack  "column has-text-centered"
                     , target_ $ pack "_blank"
                     , href_ $ pack "http://chimera.labs.oreilly.com/books/1230000000929/index.html"
                     ] [
                    span_ [class_ $ pack "icon is-large"] [
                       i_ [class_ $ pack "fa fa-gears"] [ ]
                    ], p_ [class_ $ pack "title is-4"] [
                        strong_ [] [ text $ pack "Concurrent" ]
                       ],
                      p_ [class_ $ pack  "subtitle"] [
                        text $ pack "Type-safe and polymorphic, GHC Haskell"
                       ]
                    ],
                  a_ [class_ $ pack  "column has-text-centered"
                     , href_ $  pack "https://github.com/ghcjs/ghcjs/blob/master/doc/foreign-function-interface.md"
                     , target_ $ pack "_blank"
                     ] [
                    span_ [class_ $ pack  "icon is-large"] [
                       i_ [class_ $ pack  "fa fa-code-fork"] [ ]
                    ], p_ [class_ $ pack  "title is-4"] [
                        strong_ [] [ text $ pack "Interoperable" ]
                       ],
                      p_ [class_ $ pack  "subtitle"] [
                        text $ pack "via the GHCJS FFI"
                        ]
                    ]
              ]
          ]
        ]
      ]


cols :: View action
cols = section_[][div_ [ class_ $ pack "container" ] [
  div_ [class_ $ pack "columns" ] [
   div_ [ class_ $ pack "column" ] [
     h1_ [class_ $ pack "title" ] [
       span_ [class_$pack"icon is-large"] [i_[class_$pack"fa fa-flash"][]]
     , text $ pack "Fast"
     ]
   , h2_ [class_ $ pack "subtitle" ] [
       text $ pack "Mutable virtual dom implementation"
      ]
   ]
   , div_ [ class_ $ pack "column" ] [
     text $ pack "Second column"
   ]
   , div_ [ class_ $ pack "column" ] [
      text $ pack "Third column"
   ]
   , div_ [ class_ $ pack "column" ] [
      text $ pack "Fourth column"
    ]
  ]]]

the404 :: Model -> View Action
the404 = template v
  where
    v = div_ [] [ img_ [
           width_ $ pack "100"
         , class_ $ pack "animated bounceOutUp"
         , src_ misoSrc
         ]
         , h1_ [ class_ $ pack "title"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
         ] [ text "404" ]
       , h2_ [ class_ $ pack "subtitle animated pulse" ] [
          text "No soup for you! "
          , a_ [ href_ "/", onPreventClick (ChangeURI goHome) ] [ text " - Go Home" ]
         ]
       ]

-- | Links
goHome, goExamples, goDocs, goCommunity :: URI
( goHome, goExamples, goDocs, goCommunity ) =
#if MIN_VERSION_servant(0,10,0)
    ( linkURI (safeLink routes homeProxy)
    , linkURI (safeLink routes examplesProxy)
    , linkURI (safeLink routes docsProxy)
    , linkURI (safeLink routes communityProxy)
    )
#else
    ( safeLink routes homeProxy
    , safeLink routes examplesProxy
    , safeLink routes docsProxy
    , safeLink routes communityProxy
    )
#endif

homeProxy :: Proxy Home
homeProxy = Proxy
examplesProxy :: Proxy Examples
examplesProxy = Proxy
docsProxy :: Proxy Docs
docsProxy = Proxy
communityProxy :: Proxy Community
communityProxy = Proxy
routes :: Proxy ClientRoutes
routes = Proxy

-- | Github stars
starMiso :: View action
starMiso = a_ [
    class_ (pack "github-button")
  , href_ (pack "https://github.com/dmjio/miso")
  , textProp (pack "data-icon") "octicon-star"
  , textProp (pack "data-size") "large"
  , textProp (pack "data-show-count") "true"
  , textProp (pack "aria-label") "Star dmjio/miso on GitHub"
  ] [ text "Star" ]

forkMiso :: View action
forkMiso = a_ [
    class_ (pack "github-button")
  , href_ (pack "https://github.com/dmjio/miso/fork")
  , textProp (pack "data-icon") "octicon-repo-forked"
  , textProp (pack "data-size") "large"
  , textProp (pack "data-show-count") "true"
  , textProp (pack "aria-label") "Fork dmjio/miso on GitHub"
  ] [ text "Fork" ]

-- | Hero
hero :: View Action -> URI -> Bool -> View Action
hero content uri' navMenuOpen' =
  section_ [ class_ $ pack "hero is-medium is-primary is-bold has-text-centered" ] [
    div_ [ class_ $pack"hero-head" ] [
     header_ [class_$pack"nav"] [
      div_ [class_$pack"container"] [
        div_ [class_$pack"nav-left"][
          a_ [class_$pack"nav-item"][
                 ]
          ],
        span_ [class_$pack"nav-toggle " <> do pack $ bool mempty "is-active" navMenuOpen'
              , onClick ToggleNavMenu
              ] [
          span_[][]
        , span_[][]
        , span_[][]
        ],
         div_ [ class_$pack"nav-right nav-menu " <> do pack $ bool mempty "is-active" navMenuOpen'] [
          a_ [ class_$ pack "nav-item " <> do pack $ bool mempty "is-active" (uriPath uri' == uriPath goHome)
             , href_ "/", onPreventClick (ChangeURI goHome) ] [ text$pack"Home" ],
          a_ [class_$ pack "nav-item " <> do pack $ bool mempty "is-active" (uriPath uri' == uriPath goExamples)
             , href_ "/examples", onPreventClick (ChangeURI goExamples)
             ] [ text$pack"Examples" ],
          a_ [class_$ pack "nav-item " <> do pack $ bool mempty "is-active" (uriPath uri' == uriPath goDocs)
             , href_ "/docs", onPreventClick (ChangeURI goDocs)
             ] [ text$pack"Docs" ],
          a_ [class_$ pack "nav-item " <> do pack $ bool mempty "is-active" (uriPath uri' == uriPath goCommunity)
             , href_ "/community", onPreventClick (ChangeURI goCommunity)
             ] [ text$pack"Community" ]

          ]]]]
    , div_ [ class_ $ pack "hero-body" ] [
     div_ [ class_ $ pack "container" ] [
           content
         ]
     ]
  ]

onPreventClick :: Action -> Attribute Action
onPreventClick action =
  onWithOptions defaultOptions { preventDefault = True }
    "click" emptyDecoder (\() -> action)

-- | Footer
footer :: View action
footer =
  footer_ [ class_ $ pack "footer" ] [
    div_ [ class_ $ pack "container" ] [
      div_ [ class_ $ pack "content has-text-centered" ] [
         p_ [] [
            strong_ [] [ text "Miso" ]
         ,  text " by "
         ,  a_ [ href_ $ pack "https://github.com/dmjio/miso" ]
              [ text "dmjio" ]
         , text ". BSD3"
         , a_ [ href_ $ pack "https://opensource.org/licenses/BSD-3-Clause" ]
              [ text " licensed." ]
         ]
         , p_ [] [ text "The source code for this website is located "
                 , a_ [ href_ $ pack "https://github.com/dmjio/miso/tree/master/examples/haskell-miso.org" ] [  text$pack" here."]
                 ]
         , p_ [] [
           a_ [ class_ $ pack "icon"
              , href_ $ pack "https://github.com/dmjio/miso"
              , target_ (pack "blank")
              ] [span_ [class_$pack"icon is-large"]
                  [i_[class_$pack"fa fa-github"][]]]
         ]
      ]
    ]
  ]



newNav navMenuOpen' =
  div_ [ class_$pack  "container" ] [
    nav_ [class_$pack  "navbar is-transparent"] [
      div_ [class_$pack  "navbar-brand"] [
        a_ [class_$pack  "navbar-item"
           ,href_$pack "https://haskell-miso.org"] [
             text "miso",
          a_ [class_$pack  "navbar-item is-hidden-desktop"
             ,href_$pack "https://github.com/dmjio/miso"
             ,target_$pack "_blank"] [
             span_ [class_$pack  "icon",
                    style_ $ M.singleton (pack "color") (pack "#333")
                   ] [ i_ [class_$pack  "fa fa-github"] [ ]
              ]
           ]
          , a_ [class_$pack  "navbar-item is-hidden-desktop"
               ,href_$pack "https://twitter.com/dmjio"
               ,target_$pack "_blank"] [
               span_ [ class_$pack  "icon"
                     , style_ $ M.singleton (pack "color") (pack "#55acee")
                     ] [
                 i_ [class_$pack  "fa fa-twitter"] [ ]
                 ]
               ]
            , div_ [ class_$pack  "navbar-burger burger " <> do pack $ bool mempty "is-active" navMenuOpen'
                   , textProp (pack "data-target") (pack "navMenuIndex")
                   , onClick ToggleNavMenu] [
                 span_ [] [ ]
                ,span_ [] [ ]
                ,span_ [] [ ]
                ]
            ]
            , div_ [ id_ $pack"navMenuIndex"
                   , class_$pack  "navbar-menu " <> do pack $ bool mempty "is-active" navMenuOpen'] [
                div_ [class_$pack  "navbar-start"] [
                  a_ [class_$pack  "navbar-item is-active"
                     ,href_$pack "https://haskell-miso.org"] [
                    text $ pack "Home"]
                , div_ [class_$pack  "navbar-item has-dropdown is-hoverable"] [
                    a_ [class_$pack  "navbar-link"
                       ,href_$pack "/documentation/overview/start/"] [
                          text $ pack "Docs"]
                    , div_ [class_$pack  "navbar-dropdown is-boxed"] [
                         a_ [class_$pack  "navbar-item "
                             ,href_$pack "/documentation/overview/start/"] [
                            text $ pack "Overview"
                          ]
                        , a_ [class_$pack  "navbar-item "
                             ,href_$pack "http://bulma.io/documentation/modifiers/syntax/"] [
                            text $ pack "Modifiers"
                            ]
                        , a_ [class_$pack  "navbar-item "
                             ,href_$pack "http://bulma.io/documentation/grid/columns/"] [
                            text $ pack "Grid"
                            ]
                        , a_ [ class_$pack  "navbar-item "
                             ,href_$pack "http://bulma.io/documentation/form/general/"] [
                            text $ pack "Form"
                            ]
                        , a_ [class_$pack  "navbar-item "
                             , href_$pack "http://bulma.io/documentation/elements/box/"] [
                            text $ pack "Elements"
                            ]
                        , a_ [class_$pack  "navbar-item "
                             ,href_$pack "http://bulma.io/documentation/components/breadcrumb/"
                             ] [ text $ pack "Components"]
                        , a_ [class_$pack  "navbar-item "
                             ,href_$pack "http://bulma.io/documentation/layout/container/"
                             ] [  text $ pack "Layout"]
                        , hr_ [class_$pack  "navbar-divider"]
                        , div_ [class_$pack  "navbar-item"] [
                              div_ [] [
                                p_ [class_$pack  "has-text-info is-size-6-desktop"] [
                                  strong_ [] [ text $ pack "0.4.4"]]
                                , small_ [] [
                                    a_ [class_$pack  "view-all-versions",href_$pack "/versions"] [
                                      text $ pack "View all versions"
                                    ]
                                  ]
                                ]
                              ]
                            ]
                        ]
                    , div_ [class_$pack  "navbar-item has-dropdown is-hoverable"] [
                        a_ [class_$pack  "navbar-link ", href_$pack "http://bulma.io/blog/"] [
                            text $ pack "Blog"
                            ]
                      , div_ [id_ $pack"blogDropdown"
                             ,class_$pack  "navbar-dropdown is-boxed"
                             ,textProp (pack "data-style_") (pack "width: 18rem;")] [
                          a_ [class_$pack  "navbar-item"
                             ,href_$pack "/2017/07/24/access-previous-bulma-versions/"] [
                             div_ [class_$pack  "navbar-content"] [
                               p_ [] [ small_ [class_$pack  "has-text-info"] [
                                 text $ pack "24 Jul 2017"]
                               ]
                             , p_ [] [ text $ pack "Access previous Bulma versions"]
                             ]
                             ]
                        , a_ [ class_$pack  "navbar-item"
                             ,href_$pack "/2017/03/10/new-field-element/"] [
                            div_ [class_$pack  "navbar-content"] [
                              p_ [] [
                                small_ [class_$pack  "has-text-info"] [
                                  text $ pack "10 Mar 2017"
                                ]
                              ]
                              , p_ [] [
                                  text $ pack "New field element (for better controls)"
                                ]
                            ]
                          ]
                        , a_ [class_$pack  "navbar-item"
                             ,href_$pack "/2016/04/11/metro-ui-css-grid-with-bulma-tiles/"] [
                            div_ [class_$pack  "navbar-content"] [
                              p_ [] [
                                small_ [class_$pack  "has-text-info"] [
                                  text $ pack "11 Apr 2016"
                                ]
                              ]
                              , p_ [] [
                                  text $ pack "Metro UI CSS grid with Bulma tiles"
                                ]
                            ]
                          ]
                        , a_ [class_$pack  "navbar-item",href_$pack "http://bulma.io/blog/"] [
                            text $ pack "More posts"
                            ]
                        , hr_ [class_$pack  "navbar-divider"]
                        , div_ [class_$pack  "navbar-item"] [
                              div_ [class_$pack  "navbar-content"] [
                                div_ [class_$pack  "level is-mobile"] [
                                  div_ [class_$pack  "level-left"] [
                                    div_ [class_$pack  "level-item"] [
                                      strong_ [] [ text $ pack "Stay up to date!"]
                                    ]
                                  ]
                                , div_ [class_$pack  "level-right"] [
                                    div_ [class_$pack  "level-item"] [
                                      a_ [class_$pack  "button is-rss is-small"
                                         ,href_$pack "http://bulma.io/atom.xml"] [
                                        span_ [class_$pack  "icon is-small"] [
                                          i_ [class_$pack  "fa fa-rss"] [ ]
                                        ]
                                      , span_ [] [
                                          text $ pack "Subscribe"]
                                      ]
                                      ]
                                    ]
                                ]
                                ]
                              ]
                            ]
                        ]
                     , div_ [class_$pack  "navbar-item has-dropdown is-hoverable"] [
                         div_ [class_$pack  "navbar-link"] [
                           text $ pack "More"]
                         , div_ [id_ $pack"moreDropdown"
                                ,class_$pack  "navbar-dropdown is-boxed" ] [
                             a_ [class_$pack  "navbar-item ",href_$pack "http://bulma.io/extensions/"] [
                               div_ [class_$pack  "level is-mobile"] [
                                 div_ [class_$pack  "level-left"] [
                                   div_ [class_$pack  "level-item"] [
                                     p_ [] [ strong_ [] [
                                       text $ pack "Extensions"
                                     ]
                                   , br_ []
                                   , small_ [] [
                                          text $ pack "Side projects to enhance Bulma"]
                                      ]
                                     ]
                                   ]
                               , div_ [class_$pack  "level-right"] [
                                   div_ [class_$pack  "level-item"] [
                                      span_ [class_$pack  "icon has-text-info"] [
                                        i_ [class_$pack  "fa fa-plug"] [ ]
                                      ]
                                    ]
                                 ]
                               ]
                             ]
                           ]
                         ]
                      ]
                    , div_ [class_$pack  "navbar-end"] [
                        a_ [class_$pack  "navbar-item"
                           ,href_$pack "https://github.com/dmjio/miso"
                           ,target_$pack "_blank"] [
                             text $ pack "Github"
                           ]
                        , a_ [class_$pack  "navbar-item"
                             ,href_$pack "https://twitter.com/dmjio"
                             ,target_$pack "_blank"] [
                               text $ pack "Twitter"
                             ]
                        , div_ [class_$pack  "navbar-item"] [
                            div_ [class_$pack  "field is-grouped"] [
                               p_ [class_$pack  "control"] [
                                 a_ [ id_ $pack"twitter"
                                    , class_$pack  "button"
                                    , textProp (pack "data-social-network_") (pack "Twitter")
                                    , textProp (pack "data-social-action_") (pack "tweet")
                                    , textProp (pack "data-social-target") (pack "http://bulma.io")
                                    ,target_$pack "_blank"
                                    , href_$pack "https://twitter.com/intent/tweet?text=Miso: a tasty Haskell front-end framework&url=https://haskell-miso.org&via=dmjio"] [
                                     span_ [class_$pack  "icon"] [
                                         i_ [class_$pack  "fa fa-twitter"] [ ]
                                     ]
                                     , span_ [] [ text $ pack "Tweet"] ]
                                 ], p_ [class_$pack  "control"] [
                                    starMiso
                                   ]
                               ]
                            ]
                        ]
                    ]
                  ]
                ]
    ]
