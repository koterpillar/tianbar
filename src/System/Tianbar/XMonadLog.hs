module System.Tianbar.XMonadLog ( dbusLog
                                , dbusLogWithMarkup
                                , dbusLogWithRenderer
                                , tianbarMarkup
                                , WindowSpaceInfo(..)
                                , Renderer
                                , MarkupRenderer
                                ) where

import Data.Maybe

import DBus
import DBus.Client

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String (renderMarkup)

import XMonad hiding (title, workspaces)
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as S
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare

-- | A DBus-based logger with a default pretty-print configuration
dbusLog :: Client -> X ()
dbusLog client = dbusLogWithMarkup client tianbarMarkup

sig :: Signal
sig = signal (fromJust $ parseObjectPath "/org/xmonad/Log")
             (fromJust $ parseInterfaceName "org.xmonad.Log")
             (fromJust $ parseMemberName "Update")

data WindowSpaceInfo = WindowSpaceInfo { wsTag     :: String
                                       , wsCurrent :: Bool
                                       , wsHidden  :: Bool
                                       , wsUrgent  :: Bool
                                       , wsEmpty   :: Bool
                                       }

type Renderer a = String            -- ^ layout description
               -> String            -- ^ window title
               -> [WindowSpaceInfo] -- ^ workspaces
               -> [Window]          -- ^ urgent windows
               -> WindowSet         -- ^ all windows
               -> a

type MarkupRenderer = Renderer Markup

dbusLogWithRenderer :: Client -> Renderer String -> X ()
dbusLogWithRenderer client renderer = do
    winset <- gets windowset
    urgents <- readUrgents
    let ld = description . S.layout . S.workspace . S.current $ winset
    wt <- maybe (return "") (fmap show . getName) . S.peek $ winset

    sort_ <- mkWsSort getWsCompare
    let ws = sort_ $ map S.workspace (S.current winset : S.visible winset)
                     ++ S.hidden winset
    let visibles = map (S.tag . S.workspace) (S.visible winset)
    let wsinfo w = WindowSpaceInfo tag_ current_ hidden_ urgent_ empty_
                   where tag_ = S.tag w
                         current_ = tag_ == S.currentTag winset
                         hidden_  = tag_ `notElem` visibles
                         urgent_  = any (\x -> maybe False (== tag_) (S.findTag x winset)) urgents
                         empty_   = isNothing (S.stack w)

    let html = renderer ld wt (map wsinfo ws) urgents winset
    liftIO $ emit client sig { signalBody = [ toVariant html ] }

dbusLogWithMarkup :: Client -> MarkupRenderer -> X ()
dbusLogWithMarkup client renderer = dbusLogWithRenderer client renderer'
    where renderer' ld wt wksp urgent winset =
              renderMarkup $ renderer ld wt wksp urgent winset

tianbarMarkup :: MarkupRenderer
tianbarMarkup layout title workspaces _ _ = do
    H.span ! A.class_ (toValue "workspaces") $
        mapM_ wsHtml $ workspaces
    H.span ! A.class_ (toValue "layout") $ toMarkup layout
    H.span ! A.class_ (toValue "title") $ toMarkup title
    where
        wsHtml w = H.span ! A.class_ (toValue $ unwords classes) $
            toMarkup $ wsTag w
            where classes =
                    ["workspace"] ++
                    ["current" | wsCurrent w] ++
                    ["hidden"  | wsHidden w] ++
                    ["urgent"  | wsUrgent w] ++
                    ["empty"   | wsEmpty w]
