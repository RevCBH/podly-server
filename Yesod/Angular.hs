{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Yesod.Angular
    ( YesodAngular (..)
    , ModuleConfig (..)
    , runAngular
    , runNgModule
    , runNgModuleWidget
    , addCommand
    , addCtrl
    , addCtrlRaw
    , addLib
    , addExtLib
    , setDefaultRoute
    , GAngular
    ) where

import           Control.Applicative        ((<$>))
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe, Maybe(..), catMaybes, maybe)
import           Data.Monoid                (First (..), Monoid (..))
import           Data.Text                  (Text)
import           Text.Hamlet                (HtmlUrl, hamletFile)
import           Text.Julius                (JavascriptUrl, julius, rawJS)
import           Text.Coffee                (coffeeFile)
import           Text.Coffee.Bare           (coffeeBareFile)
import           Yesod.Core                 (GHandler, GWidget, RepHtml,
                                             RepHtml (RepHtml), Route, Yesod,
                                             addScriptEither, addStylesheetEither, defaultLayout,
                                             getUrlRenderParams, getYesod, lift,
                                             lookupGetParam, newIdent,
                                             sendResponse, toContent,
                                             toWidget, whamlet)
import           Yesod.Json                 (jsonToRepJson, parseJsonBody_)
import Language.Haskell.TH.Syntax (Q, Exp (AppE, LitE), Lit (StringL))
import qualified Data.Text as T
import Data.Char (isAlpha)
import Data.Either (Either(..))
import Control.Monad
import GHC.Base

type Layout sub master = GWidget sub master () -> GHandler sub master RepHtml

class Yesod master => YesodAngular master where
    urlAngularJs :: master -> Either (Route master) Text
    urlAngularJs _ = Right "//ajax.googleapis.com/ajax/libs/angularjs/1.0.3/angular.js"

    urlAngularUi :: master -> Either (Route master) Text
    urlAngularUi _ = Right "https://raw.github.com/angular-ui/angular-ui/master/build/angular-ui.js"

    urlAngularUiCss :: master -> Either (Route master) Text
    urlAngularUiCss _ = Right "https://raw.github.com/angular-ui/angular-ui/master/build/angular-ui.css"

    urlJqueryUi :: master -> Either (Route master) Text
    urlJqueryUi _ = Right "//ajax.googleapis.com/ajax/libs/jqueryui/1.9.2/jquery-ui.min.js"

    urlJqueryMaskedInput :: master -> Either (Route master) Text
    urlJqueryMaskedInput _ = Right "http://angular-ui.github.com/lib/maskedinput/jquery.maskedinput.js"

    urlSocialite :: master -> Either (Route master) Text
    urlSocialite _ = Right "https://raw.github.com/dbushell/Socialite/master/socialite.js"

    wrapAngular :: Text -> Layout sub master -> GWidget sub master () -> GHandler sub master RepHtml
    wrapAngular modname layout widget = layout [whamlet|<div ng-app=#{modname}>^{widget}|]

data AngularWriter sub master = AngularWriter
    { awCommands     :: Map Text (GHandler sub master ())
    , awPartials     :: Map Text (HtmlUrl (Route master))
    , awRoutes       :: JavascriptUrl (Route master)
    , awControllers  :: JavascriptUrl (Route master)
    , awLibs         :: Map Text (Maybe (JavascriptUrl (Route master)))
    , awDefaultRoute :: First Text
    }
instance Monoid (AngularWriter sub master) where
    mempty = AngularWriter mempty mempty mempty mempty mempty mempty
    AngularWriter commands1 partials1 routes1 controllers1 libs1 defaultRoute1
        `mappend` AngularWriter commands2 partials2 routes2 controllers2 libs2 defaultRoute2
        = AngularWriter
            (mappend commands1 commands2)
            (mappend partials1 partials2)
            (mappend routes1 routes2)
            (mappend controllers1 controllers2)
            (mappend libs1 libs2)
            (mappend defaultRoute1 defaultRoute2)

type GAngular sub master = WriterT (AngularWriter sub master) (GHandler sub master)

runAngular :: YesodAngular master
           => GAngular sub master ()
           -> GHandler sub master RepHtml
runAngular ga = runNgModule (ModuleConfig Nothing Nothing) ga

data ModuleConfig sub master = ModuleConfig { modName :: Maybe Text, modLayout :: Maybe (Layout sub master) }

runNgModule :: YesodAngular master
            => ModuleConfig sub master
            -> GAngular sub master ()
            -> GHandler sub master RepHtml
runNgModule cfg ga = runNgModuleWidget cfg ([whamlet| |]) ga

runNgModuleWidget ::YesodAngular master
                    => ModuleConfig sub master
                    -> GWidget sub master ()
                    -> GAngular sub master ()
                    -> GHandler sub master RepHtml
runNgModuleWidget cfg modWidget ga = do
    master <- getYesod
    ((), AngularWriter{..}) <- runWriterT ga
    mc <- lookupGetParam "command"
    fromMaybe (return ()) $ mc >>= flip Map.lookup awCommands
    mp <- lookupGetParam "partial"
    case mp >>= flip Map.lookup awPartials of
        Nothing -> return ()
        Just htmlurl -> getUrlRenderParams >>= sendResponse . RepHtml . toContent . htmlurl
    --modname <- do
    --    case modName cfg of
    --        Just name -> return name
    --        Nothing -> newIdent
    modname <- maybe newIdent return (modName cfg)
    let layout = maybe defaultLayout id (modLayout cfg)

    let defaultRoute =
            case awDefaultRoute of
                First (Just x) -> [julius|.otherwise({redirectTo:"#{rawJS x}"})|]
                First Nothing -> mempty

    let renderLibs = mconcat . catMaybes . Map.elems $ awLibs
    let declLibs = let ks = Map.keys awLibs
                       f = \x -> T.concat ["\"", x, "\","]
                   in T.concat (map f ks)

    wrapAngular modname layout $ do
        addScriptEither $ urlJqueryUi master
        addScriptEither $ urlJqueryMaskedInput master
        addScriptEither $ urlAngularJs master
        addScriptEither $ urlAngularUi master
        addStylesheetEither $ urlAngularUiCss master
        addScriptEither $ urlSocialite master

        modWidget

        [whamlet|<div ng-view>|]
        toWidget [julius|
^{renderLibs}
angular
    .module("#{rawJS modname}", [#{rawJS declLibs}])
    .config(["$routeProvider", "$locationProvider", function($routeProvider, $locationProvider) {
        $locationProvider.html5Mode(true);
        $routeProvider ^{awRoutes} ^{defaultRoute} ;
    }]);
^{awControllers}
|]

addCommand :: (FromJSON input, ToJSON output)
           => (input -> GHandler sub master output)
           -> GAngular sub master Text
addCommand f = do
    name <- lift newIdent
    tell mempty { awCommands = Map.singleton name handler }
    return $ "?command=" `mappend` name
  where
    handler = do
        input <- parseJsonBody_
        output <- f input
        repjson <- jsonToRepJson output
        sendResponse repjson

addCtrl :: Text -- ^ route pattern
        -> Text -- ^ template name
        -> Q Exp
addCtrl route name = do
    let name' = T.filter isAlpha name
    [|addCtrlRaw $(liftT name') $(liftT route) $(hamletFile $ fn "hamlet") $(coffeeBareFile $ fn "coffee")|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t
    fn suffix = T.unpack $ T.concat ["angular/", name, ".", suffix]

addCtrlRaw :: Text -- ^ user-friendly name
           -> Text -- ^ route pattern
           -> HtmlUrl (Route master) -- ^ template
           -> JavascriptUrl (Route master) -- ^ controller
           -> GAngular sub master ()
addCtrlRaw name' route template controller = do
    name <- (mappend $ mappend name' "__") <$> lift newIdent
    tell mempty
        { awPartials = Map.singleton name template
        , awRoutes = [julius|.when("#{rawJS route}", {controller:#{rawJS name}, templateUrl:"?partial=#{rawJS name}"})|]
        , awControllers = [julius|var #{rawJS name} = (function (){^{controller}})();|]
        }

setDefaultRoute :: Text -> GAngular sub master ()
setDefaultRoute x = tell mempty { awDefaultRoute = First $ Just x }

addLib :: Text -- ^ lib name
       -> Q Exp
addLib name = do
    --let name' = T.filter isAlpha name
    [|addLibRaw $(liftT name) $(coffeeFile fn)|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t
    fn = T.unpack $ T.concat ["angular/lib/", name, ".coffee"]


addLibRaw :: Text -- ^ lib name
          -> JavascriptUrl (Route master) -- ^ lib
          -> GAngular sub master ()
addLibRaw name lib = do
    tell mempty
        { awLibs = Map.singleton name $ Just [julius|^{lib}|] }

addExtLib :: Text
          -> GAngular sub master ()
addExtLib name = tell mempty { awLibs = Map.singleton name Nothing }