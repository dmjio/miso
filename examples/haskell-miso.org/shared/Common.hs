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
    v = div_ [ class_  "animated fadeIn" ] [ a_ [ href_ "https://github.com/dmjio/miso" ] [ img_ [
           width_  "100"
         , class_  "animated bounceInDown"
         , src_ misoSrc
         , alt_ "miso logo"
         ] ]
         , h1_ [ class_  "title animated pulse"
               , style_ $ M.fromList [ (pack "font-size", pack "82px")
                                     , (pack "font-weight", pack "100")
                                     ]
           ] [ text "community" ]
       , h2_ [ class_  "subtitle animated pulse" ] [
            a_ [ href_  "https://join.slack.com/t/haskell-miso/shared_invite/zt-1w80x404h-AF2Be_BDQixNMadzadnUNg"
               , target_  "_blank"
               ]
            [ text "Slack" ]
          , text " / "
          , a_ [ href_  "https://www.irccloud.com/invite?channel=%23haskell-miso&hostname=irc.libera.chat&port=6697&ssl=1"
               , target_  "_blank"
               ] [ text "#haskell-miso" ]
         ]
       ]

docs :: Model -> View Action
docs = template v
  where
    v = div_ [ class_  "animated fadeIn" ] [ a_ [ href_ "https://github.com/dmjio/miso" ] [ img_ [
           width_  "100"
         , class_  "animated bounceInDown"
         , src_ misoSrc
         , alt_ "miso logo"
         ] ]
         , h1_ [ class_  "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "docs" ]
        , h2_ [ class_  "subtitle animated pulse" ] [
            a_ [ href_  "http://haddocks.haskell-miso.org/"
               , target_  "_blank"
               ]
              [ text "Haddocks" ]
          , text " / "
          , a_ [ href_  "https://github.com/dmjio/miso/blob/master/README.md"
               , target_  "_blank"
               ]
            [ text "README" ]
         ]
       ]

misoSrc :: MisoString
misoSrc = pack "https://em-content.zobj.net/thumbs/240/apple/325/steaming-bowl_1f35c.png"

examples :: Model -> View Action
examples = template v
  where
    v =
      div_ [ class_  "animated fadeIn" ] [ a_ [ href_ "https://github.com/dmjio/miso" ] [ img_ [
           width_  "100"
         , class_  "animated bounceInDown"
         , src_ misoSrc
         , alt_ "miso logo"
         ] ]
         , h1_ [ class_  "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "examples" ]
       , h2_ [ class_  "subtitle animated pulse" ] [
            a_ [ target_  "_blank"
               , href_  "http://todo-mvc.haskell-miso.org/"
               ] [ text "TodoMVC" ]
          , text " / "
          , a_ [ target_  "_blank"
               , href_  "http://mario.haskell-miso.org/" ]
            [ text "Mario" ]
          , text " / "
          , a_ [ target_  "_blank"
               , href_  "http://flatris.haskell-miso.org/" ]
            [ text "Flatris" ]
          , text " / "
          , a_ [ target_  "_blank"
               , href_  "http://2048.haskell-miso.org/" ]
            [ text "2048" ]
         ]
       ]

home :: Model -> View Action
home = template v
  where
    v = div_ [ class_  "animated fadeIn" ] [
        a_ [ href_ "https://github.com/dmjio/miso" ] [
            img_ [ width_ "100"
                 , class_ "animated bounceInDown"
                 , src_ misoSrc
                 , alt_ "miso logo"
                 ]
            ]
        , h1_ [ class_  "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "miso" ]
        , h2_ [ class_ "subtitle animated pulse" ] [
         text "A tasty "
         , a_ [ href_ "https://www.haskell.org/"
              , rel_ "noopener"
              , target_ "_blank"][
             strong_ [] [text "Haskell" ]]
         , text  " front-end framework"
         ]
        ]


template :: View Action -> Model -> View Action
template content Model{..} =
  div_ [ ] [
    a_ [ class_ "github-fork-ribbon left-top fixed"
       , href_ "http://github.com/dmjio/miso"
       , prop "data-ribbon" ("Fork me on GitHub"::MisoString)
       , target_ "blank"
       , rel_ "noopener"
       , title_"Fork me on GitHub"
       ] [text "Fork me on GitHub"]
  , hero content uri navMenuOpen
  , middle
  , footer
  ]

middle =
  section_ [class_ "hero" ] [
    div_ [class_ "hero-body"] [
      div_ [class_ "container"] [
        nav_ [class_ "columns"] [
               a_ [ class_ "column has-text-centered"
                   , href_ "https://rawgit.com/krausest/js-framework-benchmark/master/webdriver-ts-results/table.html"
                   , target_ "_blank"
                   , rel_ "noopener"
                   ] [
                  span_ [class_   "icon is-large"] [
                      i_ [class_   "fa fa-flash"] [ ]
                      ],
                  p_ [class_   "title is-4"] [
                     strong_ [] [ text  "Fast"]
                  ],
                  p_ [class_   "subtitle"] [
                        text  "Virtual DOM diffing algorithm"
                      ]
                  ]

              , a_ [ class_   "column has-text-centered"
                   , href_  "https://en.wikipedia.org/wiki/Isomorphic_JavaScript"
                   , target_ "_blank"
                   , rel_ "noopener"
                   ] [
                  span_ [class_   "icon is-large"] [
                      i_ [class_   "fa fa-refresh"] [ ]
                      ],
                  p_ [class_   "title is-4"] [
                     strong_ [] [ text  "Isomorphic"]
                  ],
                  p_ [class_   "subtitle"]
                      [ text  "Seamless web experience" ]
                  ],
                  a_ [ class_   "column has-text-centered"
                     , target_  "_blank"
                     , href_  "http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html"
                     , rel_ "noopener"
                     ] [
                    span_ [class_  "icon is-large"] [
                       i_ [class_  "fa fa-gears"] [ ]
                    ], p_ [class_  "title is-4"] [
                        strong_ [] [ text  "Concurrent" ]
                       ],
                      p_ [class_   "subtitle"] [
                        text  "Type-safe and polymorphic, GHC Haskell"
                       ]
                    ],
                  a_ [class_ "column has-text-centered"
                     , href_  "https://github.com/ghcjs/ghcjs/blob/master/doc/foreign-function-interface.md"
                     , rel_ "noopener"
                     , target_  "_blank"
                     ] [
                    span_ [class_   "icon is-large"] [
                       i_ [class_   "fa fa-code-fork"] [ ]
                    ], p_ [class_   "title is-4"] [
                        strong_ [] [ text  "Interoperable" ]
                       ],
                      p_ [class_   "subtitle"] [
                        text  "via the GHCJS FFI"
                        ]
                    ]
              ]
          ]
        ]
      ]


cols :: View action
cols = section_[][div_ [ class_  "container" ] [
  div_ [class_  "columns" ] [
   div_ [ class_  "column" ] [
     h1_ [class_  "title" ] [
       span_ [class_"icon is-large"] [i_[class_"fa fa-flash"][]]
     , text  "Fast"
     ]
   , h2_ [class_  "subtitle" ] [
       text  "Mutable virtual dom implementation"
      ]
   ]
   , div_ [ class_  "column" ] [
     text  "Second column"
   ]
   , div_ [ class_  "column" ] [
      text  "Third column"
   ]
   , div_ [ class_  "column" ] [
      text  "Fourth column"
    ]
  ]]]

the404 :: Model -> View Action
the404 = template v
  where
    v = div_ [] [ a_ [ href_ "https://github.com/dmjio/miso" ] [ img_ [
           width_  "100"
         , class_  "animated bounceOutUp"
         , src_ misoSrc
         , alt_ "miso logo"
         ]]
         , h1_ [ class_  "title"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
         ] [ text "404" ]
       , h2_ [ class_  "subtitle animated pulse" ] [
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
  section_ [ class_  "hero is-medium is-primary is-bold has-text-centered" ] [
    div_ [ class_ "hero-head" ] [
      header_ [class_"nav"] [
        div_ [class_"container"] [
          div_ [class_"nav-left"] [
            a_ [class_"nav-item"] []
            ],
          span_ [class_$ "nav-toggle " <> bool mempty "is-active" navMenuOpen'
                , onClick ToggleNavMenu ] [
            span_[][],
            span_[][],
            span_[][]
            ],
          div_ [ class_ $ "nav-right nav-menu " <> do  bool mempty "is-active" navMenuOpen'] [
            div_ [ classList_ [ ("nav-item",True)
                              , ("is-active", uriPath uri' == "/" || uriPath uri' == "")
                 ]] [
              a_ [ href_ "/", onPreventClick (ChangeURI goHome) ]
                 [ text"Home" ]
              ],
            div_ [ classList_ [ ("nav-item",True)
                              , ("is-active", uriPath uri' == ("/" <> uriPath goExamples))
                 ]] [
              a_ [ href_ "/examples", onPreventClick (ChangeURI goExamples)]
                 [ text"Examples" ]
              ],
            div_ [ classList_ [ ("nav-item",True)
                              , ("is-active", uriPath uri' == ("/" <> uriPath goDocs))
                 ]] [
              a_ [ href_ "/docs", onPreventClick (ChangeURI goDocs) ]
                 [ text"Docs" ]
              ],
            div_ [ classList_ [ ("nav-item",True)
                              , ("is-active", uriPath uri' == ("/" <> uriPath goCommunity))
                 ]] [
              a_ [ href_ "/community", onPreventClick (ChangeURI goCommunity) ]
                 [ text"Community" ]
              ]
      ]]]]
    , div_ [ class_  "hero-body" ] [
     div_ [ class_  "container" ] [
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
  footer_ [ class_  "footer" ] [
    div_ [ class_  "container" ] [
      div_ [ class_  "content has-text-centered" ] [
         p_ [] [
            strong_ [] [ text "Miso" ]
         ,  text " by "
         ,  a_ [ href_  "https://github.com/dmjio/miso"
               , style_ $ M.singleton "color" "#363636"
               ]
              [ text "dmjio" ]
         , text ". BSD3"
         , a_ [ href_  "https://opensource.org/licenses/BSD-3-Clause"
              , style_ $ M.singleton "color" "#363636"
              ]
              [ text " licensed." ]
         ]
         , p_ [] [ text "The source code for this website is located "
                 , a_ [ href_  "https://github.com/dmjio/miso/tree/master/examples/haskell-miso.org"
                      , style_ $ M.singleton "color" "#363636"
                      ] [  text" here."]
                 ]
         , p_ [] [ a_ [href_"https://bulma.io"] [ img_
                                                    [ src_ "https://bulma.io/assets/images/made-with-bulma.png"
                                                    , alt_ "Made with Bulma"
                                                    , width_ "128"
                                                    , height_ "24"
                                                    ]
                                                ] ]
         , p_ [] [
           a_ [ href_ "https://github.com/dmjio/miso" ] [ span_ [ class_"icon is-large"]
                  [
                    i_ [ class_"fa fa-github"][ ]
                  ]
                                                        ]
           ]
        ]
      ]
    ]

newNav navMenuOpen' =
  div_ [ class_ "container" ] [
    nav_ [class_ "navbar is-transparent"] [
      div_ [class_ "navbar-brand"] [
        a_ [class_ "navbar-item"
           ,href_"https://haskell-miso.org"] [
             text "miso",
          a_ [class_ "navbar-item is-hidden-desktop"
             ,href_"https://github.com/dmjio/miso"
             ,target_"_blank"
             ,rel_ "noopener"
             ,name_ "miso"
             ] [
             span_ [ class_ "icon"
                   , name_ "github"
                   , style_ $ M.singleton (pack "color") (pack "#333")
                   ] [ i_ [class_ "fa fa-github"] [ ]
              ]
           ]
          , a_ [class_ "navbar-item is-hidden-desktop"
               ,href_"https://twitter.com/dmjio"
               ,rel_ "noopener"
               ,target_"_blank"] [
               span_ [ class_ "icon", name_ "twitter"
                     , style_ $ M.singleton (pack "color") (pack "#55acee")
                     ] [
                 i_ [class_ "fa fa-twitter"] [ ]
                 ]
               ]
            , div_ [ class_ $ "navbar-burger burger " <> bool mempty "is-active" navMenuOpen'
                   , textProp (pack "data-target") (pack "navMenuIndex")
                   , onClick ToggleNavMenu] [
                 span_ [] [ ]
                ,span_ [] [ ]
                ,span_ [] [ ]
                ]
            ]
            , div_ [ id_ "navMenuIndex"
                   , class_ $ "navbar-menu " <> bool mempty "is-active" navMenuOpen'] [
                div_ [class_ "navbar-start"] [
                  a_ [class_ "navbar-item is-active"
                     ,href_"https://haskell-miso.org"] [
                    text  "Home"]
                , div_ [class_ "navbar-item has-dropdown is-hoverable"] [
                    a_ [class_ "navbar-link"
                       ,href_"/documentation/overview/start/"] [
                          text  "Docs"]
                    , div_ [class_ "navbar-dropdown is-boxed"] [
                         a_ [class_ "navbar-item "
                             ,href_"/documentation/overview/start/"] [
                            text  "Overview"
                          ]
                        , a_ [class_ "navbar-item "
                             ,href_"http://bulma.io/documentation/modifiers/syntax/"] [
                            text  "Modifiers"
                            ]
                        , a_ [class_ "navbar-item "
                             ,href_"http://bulma.io/documentation/grid/columns/"] [
                            text  "Grid"
                            ]
                        , a_ [ class_ "navbar-item "
                             ,href_"http://bulma.io/documentation/form/general/"] [
                            text  "Form"
                            ]
                        , a_ [class_ "navbar-item "
                             , href_"http://bulma.io/documentation/elements/box/"] [
                            text  "Elements"
                            ]
                        , a_ [class_ "navbar-item "
                             ,href_"http://bulma.io/documentation/components/breadcrumb/"
                             ] [ text  "Components"]
                        , a_ [class_ "navbar-item "
                             ,href_"http://bulma.io/documentation/layout/container/"
                             ] [  text  "Layout"]
                        , hr_ [class_ "navbar-divider"]
                        , div_ [class_ "navbar-item"] [
                              div_ [] [
                                p_ [class_ "has-text-info is-size-6-desktop"] [
                                  strong_ [] [ text  "0.4.4"]]
                                , small_ [] [
                                    a_ [class_ "view-all-versions",href_"/versions"] [
                                      text  "View all versions"
                                    ]
                                  ]
                                ]
                              ]
                            ]
                        ]
                    , div_ [class_ "navbar-item has-dropdown is-hoverable"] [
                        a_ [class_ "navbar-link ", href_"http://bulma.io/blog/"] [
                            text  "Blog"
                            ]
                      , div_ [id_ "blogDropdown"
                             ,class_ "navbar-dropdown is-boxed"
                             ,textProp (pack "data-style_") (pack "width: 18rem;")] [
                          a_ [class_ "navbar-item"
                             ,href_"/2017/07/24/access-previous-bulma-versions/"] [
                             div_ [class_ "navbar-content"] [
                               p_ [] [ small_ [class_ "has-text-info"] [
                                 text  "24 Jul 2017"]
                               ]
                             , p_ [] [ text  "Access previous Bulma versions"]
                             ]
                             ]
                        , a_ [ class_ "navbar-item"
                             ,href_"/2017/03/10/new-field-element/"] [
                            div_ [class_ "navbar-content"] [
                              p_ [] [
                                small_ [class_ "has-text-info"] [
                                  text  "10 Mar 2017"
                                ]
                              ]
                              , p_ [] [
                                  text  "New field element (for better controls)"
                                ]
                            ]
                          ]
                        , a_ [class_ "navbar-item"
                             ,href_"/2016/04/11/metro-ui-css-grid-with-bulma-tiles/"] [
                            div_ [class_ "navbar-content"] [
                              p_ [] [
                                small_ [class_ "has-text-info"] [
                                  text  "11 Apr 2016"
                                ]
                              ]
                              , p_ [] [
                                  text  "Metro UI CSS grid with Bulma tiles"
                                ]
                            ]
                          ]
                        , a_ [class_ "navbar-item",href_"http://bulma.io/blog/"] [
                            text  "More posts"
                            ]
                        , hr_ [class_ "navbar-divider"]
                        , div_ [class_ "navbar-item"] [
                              div_ [class_ "navbar-content"] [
                                div_ [class_ "level is-mobile"] [
                                  div_ [class_ "level-left"] [
                                    div_ [class_ "level-item"] [
                                      strong_ [] [ text  "Stay up to date!"]
                                    ]
                                  ]
                                , div_ [class_ "level-right"] [
                                    div_ [class_ "level-item"] [
                                      a_ [class_ "button is-rss is-small"
                                         ,href_"http://bulma.io/atom.xml"] [
                                        span_ [class_ "icon is-small"] [
                                          i_ [class_ "fa fa-rss"] [ ]
                                        ]
                                      , span_ [] [
                                          text  "Subscribe"]
                                      ]
                                      ]
                                    ]
                                ]
                                ]
                              ]
                            ]
                        ]
                     , div_ [class_ "navbar-item has-dropdown is-hoverable"] [
                         div_ [class_ "navbar-link"] [
                           text  "More"]
                         , div_ [id_ "moreDropdown"
                                ,class_ "navbar-dropdown is-boxed" ] [
                             a_ [class_ "navbar-item ",href_"http://bulma.io/extensions/"] [
                               div_ [class_ "level is-mobile"] [
                                 div_ [class_ "level-left"] [
                                   div_ [class_ "level-item"] [
                                     p_ [] [ strong_ [] [
                                       text  "Extensions"
                                     ]
                                   , br_ []
                                   , small_ [] [
                                          text  "Side projects to enhance Bulma"]
                                      ]
                                     ]
                                   ]
                               , div_ [class_ "level-right"] [
                                   div_ [class_ "level-item"] [
                                      span_ [class_ "icon has-text-info"] [
                                        i_ [class_ "fa fa-plug"] [ ]
                                      ]
                                    ]
                                 ]
                               ]
                             ]
                           ]
                         ]
                      ]
                    , div_ [class_ "navbar-end"] [
                        a_ [class_ "navbar-item"
                           ,href_"https://github.com/dmjio/miso"
                           ,target_"_blank"] [
                             text  "Github"
                           ]
                        , a_ [class_ "navbar-item"
                             ,href_"https://twitter.com/dmjio"
                             ,target_"_blank"] [
                               text  "Twitter"
                             ]
                        , div_ [class_ "navbar-item"] [
                            div_ [class_ "field is-grouped"] [
                               p_ [class_ "control"] [
                                 a_ [ id_ "twitter"
                                    , class_ "button"
                                    , textProp (pack "data-social-network_") (pack "Twitter")
                                    , textProp (pack "data-social-action_") (pack "tweet")
                                    , textProp (pack "data-social-target") (pack "http://bulma.io")
                                    ,target_"_blank"
                                    , href_"https://twitter.com/intent/tweet?text=Miso: a tasty Haskell front-end framework&url=https://haskell-miso.org&via=dmjio"] [
                                     span_ [class_ "icon"] [
                                         i_ [class_ "fa fa-twitter"] [ ]
                                     ]
                                     , span_ [] [ text "Tweet"] ]
                                 ], p_ [class_ "control"] [
                                    starMiso
                                   ]
                               ]
                            ]
                        ]
                    ]
                  ]
                ]
    ]
