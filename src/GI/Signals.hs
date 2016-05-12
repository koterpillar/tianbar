{-# LANGUAGE CPP, DataKinds, PatternSynonyms, RankNTypes, ScopedTypeVariables, TypeFamilies #-}


{- |
Copyright  : Will Thompson, Iñaki García Etxebarria and Jonas Platte
License    : LGPL-2.1
Maintainer : Iñaki García Etxebarria (garetxe@gmail.com)
-}

module GI.Signals
    ( 

-- * Exported types
    pattern AccelActivate                   ,
    pattern AccelChanged                    ,
    pattern AccelCleared                    ,
    pattern AccelClosuresChanged            ,
    pattern AccelEdited                     ,
    pattern AcceptPosition                  ,
    pattern ActionActivated                 ,
    pattern ActionsChanged                  ,
    pattern Activate                        ,
    pattern ActivateCurrent                 ,
    pattern ActivateCurrentLink             ,
    pattern ActivateCursorChild             ,
    pattern ActivateCursorItem              ,
    pattern ActivateCursorRow               ,
    pattern ActivateDefault                 ,
    pattern ActivateFocus                   ,
    pattern ActivateItem                    ,
    pattern ActivateLink                    ,
    pattern Add                             ,
    pattern AddEditable                     ,
    pattern AddWidget                       ,
    pattern AdjustBounds                    ,
    pattern AngleChanged                    ,
    pattern ApplicationActivated            ,
    pattern ApplicationSelected             ,
    pattern Apply                           ,
    pattern ApplyAttributes                 ,
    pattern ApplyTag                        ,
    pattern Attach                          ,
    pattern Authenticate                    ,
    pattern Backspace                       ,
    pattern Begin                           ,
    pattern BeginPrint                      ,
    pattern BeginUserAction                 ,
    pattern BringToFront                    ,
    pattern ButtonPressEvent                ,
    pattern ButtonReleaseEvent              ,
    pattern CanActivateAccel                ,
    pattern Cancel                          ,
    pattern CancelPosition                  ,
    pattern Cancelled                       ,
    pattern ChangeCurrentPage               ,
    pattern ChangeValue                     ,
    pattern Changed                         ,
    pattern CheckResize                     ,
    pattern ChildActivated                  ,
    pattern ChildAttached                   ,
    pattern ChildDetached                   ,
    pattern ChildNotify                     ,
    pattern Clicked                         ,
    pattern Close                           ,
    pattern Closed                          ,
    pattern ColorActivated                  ,
    pattern ColorChanged                    ,
    pattern ColorSet                        ,
    pattern ColumnsChanged                  ,
    pattern Commit                          ,
    pattern CompositedChanged               ,
    pattern ConfigureEvent                  ,
    pattern ConfirmOverwrite                ,
    pattern ConnectProxy                    ,
    pattern ContextMenu                     ,
    pattern ContextMenuDismissed            ,
    pattern CopyClipboard                   ,
    pattern CountedMatches                  ,
    pattern Create                          ,
    pattern CreateContext                   ,
    pattern CreateCustomWidget              ,
    pattern CreateMenuProxy                 ,
    pattern CreateWindow                    ,
    pattern CreatedDestination              ,
    pattern CurrentFolderChanged            ,
    pattern CursorChanged                   ,
    pattern CursorOnMatch                   ,
    pattern CustomItemActivated             ,
    pattern CustomWidgetApply               ,
    pattern CutClipboard                    ,
    pattern CycleChildFocus                 ,
    pattern CycleFocus                      ,
    pattern CycleHandleFocus                ,
    pattern DamageEvent                     ,
    pattern DaySelected                     ,
    pattern DaySelectedDoubleClick          ,
    pattern Deactivate                      ,
    pattern DecideDestination               ,
    pattern DecidePolicy                    ,
    pattern DeleteEvent                     ,
    pattern DeleteFromCursor                ,
    pattern DeleteRange                     ,
    pattern DeleteSurrounding               ,
    pattern DeleteText                      ,
    pattern DeletedText                     ,
    pattern Deselect                        ,
    pattern DesktopFolder                   ,
    pattern Destroy                         ,
    pattern DestroyEvent                    ,
    pattern Detach                          ,
    pattern DirectionChanged                ,
    pattern DisconnectProxy                 ,
    pattern Done                            ,
    pattern DownFolder                      ,
    pattern DownloadStarted                 ,
    pattern DragActionAsk                   ,
    pattern DragActionRequested             ,
    pattern DragBegin                       ,
    pattern DragDataDelete                  ,
    pattern DragDataGet                     ,
    pattern DragDataReceived                ,
    pattern DragDrop                        ,
    pattern DragEnd                         ,
    pattern DragFailed                      ,
    pattern DragLeave                       ,
    pattern DragMotion                      ,
    pattern DragPerformDrop                 ,
    pattern DragUpdate                      ,
    pattern Draw                            ,
    pattern DrawPage                        ,
    pattern EdgeOvershot                    ,
    pattern EdgeReached                     ,
    pattern Edited                          ,
    pattern EditingCanceled                 ,
    pattern EditingDone                     ,
    pattern EditingStarted                  ,
    pattern Embedded                        ,
    pattern EnableDebugging                 ,
    pattern End                             ,
    pattern EndPrint                        ,
    pattern EndUserAction                   ,
    pattern Enter                           ,
    pattern EnterFullscreen                 ,
    pattern EnterNotifyEvent                ,
    pattern Escape                          ,
    pattern Event                           ,
    pattern EventAfter                      ,
    pattern ExpandCollapseCursorRow         ,
    pattern ExtendSelection                 ,
    pattern Failed                          ,
    pattern FailedToFindText                ,
    pattern FailedWithTlsErrors             ,
    pattern FaviconChanged                  ,
    pattern FileActivated                   ,
    pattern FileSet                         ,
    pattern Finished                        ,
    pattern Focus                           ,
    pattern FocusChanged                    ,
    pattern FocusHomeOrEnd                  ,
    pattern FocusInEvent                    ,
    pattern FocusOutEvent                   ,
    pattern FocusTab                        ,
    pattern FontActivated                   ,
    pattern FontSet                         ,
    pattern FormatEntryText                 ,
    pattern FormatValue                     ,
    pattern FoundText                       ,
    pattern GetChildPosition                ,
    pattern GotPageSize                     ,
    pattern GrabBrokenEvent                 ,
    pattern GrabFocus                       ,
    pattern GrabNotify                      ,
    pattern GroupChanged                    ,
    pattern Hide                            ,
    pattern HierarchyChanged                ,
    pattern HomeFolder                      ,
    pattern IconPress                       ,
    pattern IconRelease                     ,
    pattern InitializeWebExtensions         ,
    pattern Input                           ,
    pattern InsecureContentDetected         ,
    pattern Insert                          ,
    pattern InsertAtCursor                  ,
    pattern InsertChildAnchor               ,
    pattern InsertPixbuf                    ,
    pattern InsertPrefix                    ,
    pattern InsertText                      ,
    pattern InsertedText                    ,
    pattern ItemActivated                   ,
    pattern KeyPressEvent                   ,
    pattern KeyReleaseEvent                 ,
    pattern KeynavFailed                    ,
    pattern KeysChanged                     ,
    pattern Leave                           ,
    pattern LeaveFullscreen                 ,
    pattern LeaveNotifyEvent                ,
    pattern LoadChanged                     ,
    pattern LoadFailed                      ,
    pattern LoadFailedWithTlsErrors         ,
    pattern LocationPopup                   ,
    pattern LocationPopupOnPaste            ,
    pattern LocationTogglePopup             ,
    pattern Map                             ,
    pattern MapEvent                        ,
    pattern MarkDeleted                     ,
    pattern MarkSet                         ,
    pattern MatchSelected                   ,
    pattern MnemonicActivate                ,
    pattern ModifiedChanged                 ,
    pattern MonthChanged                    ,
    pattern MotionNotifyEvent               ,
    pattern Mount                           ,
    pattern MouseTargetChanged              ,
    pattern Move                            ,
    pattern MoveActive                      ,
    pattern MoveCurrent                     ,
    pattern MoveCursor                      ,
    pattern MoveFocus                       ,
    pattern MoveFocusOut                    ,
    pattern MoveHandle                      ,
    pattern MoveScroll                      ,
    pattern MoveSelected                    ,
    pattern MoveSlider                      ,
    pattern MoveViewport                    ,
    pattern NextMatch                       ,
    pattern NextMonth                       ,
    pattern NextYear                        ,
    pattern NoMatches                       ,
    pattern OffsetChanged                   ,
    pattern OpenLocation                    ,
    pattern OpenWindow                      ,
    pattern OrientationChanged              ,
    pattern Output                          ,
    pattern OwnerChange                     ,
    pattern PageAdded                       ,
    pattern PageRemoved                     ,
    pattern PageReordered                   ,
    pattern Paginate                        ,
    pattern Pan                             ,
    pattern ParentSet                       ,
    pattern ParsingError                    ,
    pattern PasteClipboard                  ,
    pattern PasteDone                       ,
    pattern PermissionRequest               ,
    pattern PlacesShortcut                  ,
    pattern PlugAdded                       ,
    pattern PlugRemoved                     ,
    pattern Popdown                         ,
    pattern PopulatePopup                   ,
    pattern Popup                           ,
    pattern PopupContextMenu                ,
    pattern PopupMenu                       ,
    pattern PostActivate                    ,
    pattern PreActivate                     ,
    pattern PreeditChanged                  ,
    pattern PreeditEnd                      ,
    pattern PreeditStart                    ,
    pattern Prepare                         ,
    pattern Pressed                         ,
    pattern PrevMonth                       ,
    pattern PrevYear                        ,
    pattern Preview                         ,
    pattern PreviousMatch                   ,
    pattern Print                           ,
    pattern PropertyNotifyEvent             ,
    pattern ProximityInEvent                ,
    pattern ProximityOutEvent               ,
    pattern QueryTooltip                    ,
    pattern QuickBookmark                   ,
    pattern Ready                           ,
    pattern ReadyToShow                     ,
    pattern Realize                         ,
    pattern ReceivedData                    ,
    pattern RecentShortcut                  ,
    pattern Released                        ,
    pattern Remove                          ,
    pattern RemoveEditable                  ,
    pattern RemoveTag                       ,
    pattern RemoveWidget                    ,
    pattern Render                          ,
    pattern ReorderTab                      ,
    pattern RequestPageSetup                ,
    pattern Resize                          ,
    pattern ResourceLoadStarted             ,
    pattern Response                        ,
    pattern RetrieveSurrounding             ,
    pattern RowActivated                    ,
    pattern RowChanged                      ,
    pattern RowCollapsed                    ,
    pattern RowDeleted                      ,
    pattern RowExpanded                     ,
    pattern RowHasChildToggled              ,
    pattern RowInserted                     ,
    pattern RowSelected                     ,
    pattern RunAsModal                      ,
    pattern RunColorChooser                 ,
    pattern RunFileChooser                  ,
    pattern ScaleChanged                    ,
    pattern ScreenChanged                   ,
    pattern ScriptDialog                    ,
    pattern ScriptMessageReceived           ,
    pattern ScrollChild                     ,
    pattern ScrollEvent                     ,
    pattern Search                          ,
    pattern SearchChanged                   ,
    pattern SearchShortcut                  ,
    pattern Select                          ,
    pattern SelectAll                       ,
    pattern SelectCursorItem                ,
    pattern SelectCursorParent              ,
    pattern SelectCursorRow                 ,
    pattern SelectPage                      ,
    pattern SelectedChildrenChanged         ,
    pattern SelectedRowsChanged             ,
    pattern SelectionChanged                ,
    pattern SelectionClearEvent             ,
    pattern SelectionDone                   ,
    pattern SelectionGet                    ,
    pattern SelectionNotifyEvent            ,
    pattern SelectionReceived               ,
    pattern SelectionRequestEvent           ,
    pattern SentRequest                     ,
    pattern SequenceStateChanged            ,
    pattern SetAnchor                       ,
    pattern SetFocus                        ,
    pattern SetFocusChild                   ,
    pattern Show                            ,
    pattern ShowConnectToServer             ,
    pattern ShowEnterLocation               ,
    pattern ShowErrorMessage                ,
    pattern ShowHelp                        ,
    pattern ShowHidden                      ,
    pattern ShowMenu                        ,
    pattern ShowNotification                ,
    pattern ShowOtherLocations              ,
    pattern ShowOtherLocationsWithFlags     ,
    pattern SizeAllocate                    ,
    pattern SizeChanged                     ,
    pattern SortColumnChanged               ,
    pattern StartInteractiveSearch          ,
    pattern StateChanged                    ,
    pattern StateFlagsChanged               ,
    pattern StateSet                        ,
    pattern StatusChanged                   ,
    pattern StopSearch                      ,
    pattern Stopped                         ,
    pattern StyleChanged                    ,
    pattern StyleSet                        ,
    pattern StyleUpdated                    ,
    pattern SubmitForm                      ,
    pattern Swipe                           ,
    pattern SwitchPage                      ,
    pattern TagAdded                        ,
    pattern TagChanged                      ,
    pattern TagRemoved                      ,
    pattern TestCollapseRow                 ,
    pattern TestExpandRow                   ,
    pattern TextPopped                      ,
    pattern TextPushed                      ,
    pattern ToggleCursorChild               ,
    pattern ToggleCursorItem                ,
    pattern ToggleCursorRow                 ,
    pattern ToggleCursorVisible             ,
    pattern ToggleHandleFocus               ,
    pattern ToggleOverwrite                 ,
    pattern ToggleSizeAllocate              ,
    pattern ToggleSizeRequest               ,
    pattern Toggled                         ,
    pattern ToolbarReconfigured             ,
    pattern TouchEvent                      ,
    pattern Unmap                           ,
    pattern UnmapEvent                      ,
    pattern Unmount                         ,
    pattern Unrealize                       ,
    pattern UnselectAll                     ,
    pattern UpFolder                        ,
    pattern Update                          ,
    pattern UpdateCustomWidget              ,
    pattern UpdatePreview                   ,
    pattern ValueChanged                    ,
    pattern VisibilityNotifyEvent           ,
    pattern WebProcessCrashed               ,
    pattern WindowAdded                     ,
    pattern WindowRemoved                   ,
    pattern WindowStateEvent                ,
    pattern Wrapped                         ,


    ) where





import Data.GI.Base.Signals (SignalProxy(..))
import Data.GI.Base.Overloading (ResolveSignal)

#if MIN_VERSION_base(4,8,0)
pattern AccelActivate :: SignalProxy object (ResolveSignal "accelActivate" object)
pattern AccelActivate = SignalProxy
#else
pattern AccelActivate = SignalProxy :: forall info object. info ~ ResolveSignal "accelActivate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern AccelChanged :: SignalProxy object (ResolveSignal "accelChanged" object)
pattern AccelChanged = SignalProxy
#else
pattern AccelChanged = SignalProxy :: forall info object. info ~ ResolveSignal "accelChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern AccelCleared :: SignalProxy object (ResolveSignal "accelCleared" object)
pattern AccelCleared = SignalProxy
#else
pattern AccelCleared = SignalProxy :: forall info object. info ~ ResolveSignal "accelCleared" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern AccelClosuresChanged :: SignalProxy object (ResolveSignal "accelClosuresChanged" object)
pattern AccelClosuresChanged = SignalProxy
#else
pattern AccelClosuresChanged = SignalProxy :: forall info object. info ~ ResolveSignal "accelClosuresChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern AccelEdited :: SignalProxy object (ResolveSignal "accelEdited" object)
pattern AccelEdited = SignalProxy
#else
pattern AccelEdited = SignalProxy :: forall info object. info ~ ResolveSignal "accelEdited" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern AcceptPosition :: SignalProxy object (ResolveSignal "acceptPosition" object)
pattern AcceptPosition = SignalProxy
#else
pattern AcceptPosition = SignalProxy :: forall info object. info ~ ResolveSignal "acceptPosition" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActionActivated :: SignalProxy object (ResolveSignal "actionActivated" object)
pattern ActionActivated = SignalProxy
#else
pattern ActionActivated = SignalProxy :: forall info object. info ~ ResolveSignal "actionActivated" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActionsChanged :: SignalProxy object (ResolveSignal "actionsChanged" object)
pattern ActionsChanged = SignalProxy
#else
pattern ActionsChanged = SignalProxy :: forall info object. info ~ ResolveSignal "actionsChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Activate :: SignalProxy object (ResolveSignal "activate" object)
pattern Activate = SignalProxy
#else
pattern Activate = SignalProxy :: forall info object. info ~ ResolveSignal "activate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActivateCurrent :: SignalProxy object (ResolveSignal "activateCurrent" object)
pattern ActivateCurrent = SignalProxy
#else
pattern ActivateCurrent = SignalProxy :: forall info object. info ~ ResolveSignal "activateCurrent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActivateCurrentLink :: SignalProxy object (ResolveSignal "activateCurrentLink" object)
pattern ActivateCurrentLink = SignalProxy
#else
pattern ActivateCurrentLink = SignalProxy :: forall info object. info ~ ResolveSignal "activateCurrentLink" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActivateCursorChild :: SignalProxy object (ResolveSignal "activateCursorChild" object)
pattern ActivateCursorChild = SignalProxy
#else
pattern ActivateCursorChild = SignalProxy :: forall info object. info ~ ResolveSignal "activateCursorChild" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActivateCursorItem :: SignalProxy object (ResolveSignal "activateCursorItem" object)
pattern ActivateCursorItem = SignalProxy
#else
pattern ActivateCursorItem = SignalProxy :: forall info object. info ~ ResolveSignal "activateCursorItem" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActivateCursorRow :: SignalProxy object (ResolveSignal "activateCursorRow" object)
pattern ActivateCursorRow = SignalProxy
#else
pattern ActivateCursorRow = SignalProxy :: forall info object. info ~ ResolveSignal "activateCursorRow" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActivateDefault :: SignalProxy object (ResolveSignal "activateDefault" object)
pattern ActivateDefault = SignalProxy
#else
pattern ActivateDefault = SignalProxy :: forall info object. info ~ ResolveSignal "activateDefault" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActivateFocus :: SignalProxy object (ResolveSignal "activateFocus" object)
pattern ActivateFocus = SignalProxy
#else
pattern ActivateFocus = SignalProxy :: forall info object. info ~ ResolveSignal "activateFocus" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActivateItem :: SignalProxy object (ResolveSignal "activateItem" object)
pattern ActivateItem = SignalProxy
#else
pattern ActivateItem = SignalProxy :: forall info object. info ~ ResolveSignal "activateItem" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ActivateLink :: SignalProxy object (ResolveSignal "activateLink" object)
pattern ActivateLink = SignalProxy
#else
pattern ActivateLink = SignalProxy :: forall info object. info ~ ResolveSignal "activateLink" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Add :: SignalProxy object (ResolveSignal "add" object)
pattern Add = SignalProxy
#else
pattern Add = SignalProxy :: forall info object. info ~ ResolveSignal "add" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern AddEditable :: SignalProxy object (ResolveSignal "addEditable" object)
pattern AddEditable = SignalProxy
#else
pattern AddEditable = SignalProxy :: forall info object. info ~ ResolveSignal "addEditable" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern AddWidget :: SignalProxy object (ResolveSignal "addWidget" object)
pattern AddWidget = SignalProxy
#else
pattern AddWidget = SignalProxy :: forall info object. info ~ ResolveSignal "addWidget" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern AdjustBounds :: SignalProxy object (ResolveSignal "adjustBounds" object)
pattern AdjustBounds = SignalProxy
#else
pattern AdjustBounds = SignalProxy :: forall info object. info ~ ResolveSignal "adjustBounds" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern AngleChanged :: SignalProxy object (ResolveSignal "angleChanged" object)
pattern AngleChanged = SignalProxy
#else
pattern AngleChanged = SignalProxy :: forall info object. info ~ ResolveSignal "angleChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ApplicationActivated :: SignalProxy object (ResolveSignal "applicationActivated" object)
pattern ApplicationActivated = SignalProxy
#else
pattern ApplicationActivated = SignalProxy :: forall info object. info ~ ResolveSignal "applicationActivated" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ApplicationSelected :: SignalProxy object (ResolveSignal "applicationSelected" object)
pattern ApplicationSelected = SignalProxy
#else
pattern ApplicationSelected = SignalProxy :: forall info object. info ~ ResolveSignal "applicationSelected" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Apply :: SignalProxy object (ResolveSignal "apply" object)
pattern Apply = SignalProxy
#else
pattern Apply = SignalProxy :: forall info object. info ~ ResolveSignal "apply" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ApplyAttributes :: SignalProxy object (ResolveSignal "applyAttributes" object)
pattern ApplyAttributes = SignalProxy
#else
pattern ApplyAttributes = SignalProxy :: forall info object. info ~ ResolveSignal "applyAttributes" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ApplyTag :: SignalProxy object (ResolveSignal "applyTag" object)
pattern ApplyTag = SignalProxy
#else
pattern ApplyTag = SignalProxy :: forall info object. info ~ ResolveSignal "applyTag" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Attach :: SignalProxy object (ResolveSignal "attach" object)
pattern Attach = SignalProxy
#else
pattern Attach = SignalProxy :: forall info object. info ~ ResolveSignal "attach" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Authenticate :: SignalProxy object (ResolveSignal "authenticate" object)
pattern Authenticate = SignalProxy
#else
pattern Authenticate = SignalProxy :: forall info object. info ~ ResolveSignal "authenticate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Backspace :: SignalProxy object (ResolveSignal "backspace" object)
pattern Backspace = SignalProxy
#else
pattern Backspace = SignalProxy :: forall info object. info ~ ResolveSignal "backspace" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Begin :: SignalProxy object (ResolveSignal "begin" object)
pattern Begin = SignalProxy
#else
pattern Begin = SignalProxy :: forall info object. info ~ ResolveSignal "begin" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern BeginPrint :: SignalProxy object (ResolveSignal "beginPrint" object)
pattern BeginPrint = SignalProxy
#else
pattern BeginPrint = SignalProxy :: forall info object. info ~ ResolveSignal "beginPrint" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern BeginUserAction :: SignalProxy object (ResolveSignal "beginUserAction" object)
pattern BeginUserAction = SignalProxy
#else
pattern BeginUserAction = SignalProxy :: forall info object. info ~ ResolveSignal "beginUserAction" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern BringToFront :: SignalProxy object (ResolveSignal "bringToFront" object)
pattern BringToFront = SignalProxy
#else
pattern BringToFront = SignalProxy :: forall info object. info ~ ResolveSignal "bringToFront" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ButtonPressEvent :: SignalProxy object (ResolveSignal "buttonPressEvent" object)
pattern ButtonPressEvent = SignalProxy
#else
pattern ButtonPressEvent = SignalProxy :: forall info object. info ~ ResolveSignal "buttonPressEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ButtonReleaseEvent :: SignalProxy object (ResolveSignal "buttonReleaseEvent" object)
pattern ButtonReleaseEvent = SignalProxy
#else
pattern ButtonReleaseEvent = SignalProxy :: forall info object. info ~ ResolveSignal "buttonReleaseEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CanActivateAccel :: SignalProxy object (ResolveSignal "canActivateAccel" object)
pattern CanActivateAccel = SignalProxy
#else
pattern CanActivateAccel = SignalProxy :: forall info object. info ~ ResolveSignal "canActivateAccel" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Cancel :: SignalProxy object (ResolveSignal "cancel" object)
pattern Cancel = SignalProxy
#else
pattern Cancel = SignalProxy :: forall info object. info ~ ResolveSignal "cancel" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CancelPosition :: SignalProxy object (ResolveSignal "cancelPosition" object)
pattern CancelPosition = SignalProxy
#else
pattern CancelPosition = SignalProxy :: forall info object. info ~ ResolveSignal "cancelPosition" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Cancelled :: SignalProxy object (ResolveSignal "cancelled" object)
pattern Cancelled = SignalProxy
#else
pattern Cancelled = SignalProxy :: forall info object. info ~ ResolveSignal "cancelled" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ChangeCurrentPage :: SignalProxy object (ResolveSignal "changeCurrentPage" object)
pattern ChangeCurrentPage = SignalProxy
#else
pattern ChangeCurrentPage = SignalProxy :: forall info object. info ~ ResolveSignal "changeCurrentPage" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ChangeValue :: SignalProxy object (ResolveSignal "changeValue" object)
pattern ChangeValue = SignalProxy
#else
pattern ChangeValue = SignalProxy :: forall info object. info ~ ResolveSignal "changeValue" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Changed :: SignalProxy object (ResolveSignal "changed" object)
pattern Changed = SignalProxy
#else
pattern Changed = SignalProxy :: forall info object. info ~ ResolveSignal "changed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CheckResize :: SignalProxy object (ResolveSignal "checkResize" object)
pattern CheckResize = SignalProxy
#else
pattern CheckResize = SignalProxy :: forall info object. info ~ ResolveSignal "checkResize" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ChildActivated :: SignalProxy object (ResolveSignal "childActivated" object)
pattern ChildActivated = SignalProxy
#else
pattern ChildActivated = SignalProxy :: forall info object. info ~ ResolveSignal "childActivated" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ChildAttached :: SignalProxy object (ResolveSignal "childAttached" object)
pattern ChildAttached = SignalProxy
#else
pattern ChildAttached = SignalProxy :: forall info object. info ~ ResolveSignal "childAttached" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ChildDetached :: SignalProxy object (ResolveSignal "childDetached" object)
pattern ChildDetached = SignalProxy
#else
pattern ChildDetached = SignalProxy :: forall info object. info ~ ResolveSignal "childDetached" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ChildNotify :: SignalProxy object (ResolveSignal "childNotify" object)
pattern ChildNotify = SignalProxy
#else
pattern ChildNotify = SignalProxy :: forall info object. info ~ ResolveSignal "childNotify" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Clicked :: SignalProxy object (ResolveSignal "clicked" object)
pattern Clicked = SignalProxy
#else
pattern Clicked = SignalProxy :: forall info object. info ~ ResolveSignal "clicked" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Close :: SignalProxy object (ResolveSignal "close" object)
pattern Close = SignalProxy
#else
pattern Close = SignalProxy :: forall info object. info ~ ResolveSignal "close" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Closed :: SignalProxy object (ResolveSignal "closed" object)
pattern Closed = SignalProxy
#else
pattern Closed = SignalProxy :: forall info object. info ~ ResolveSignal "closed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ColorActivated :: SignalProxy object (ResolveSignal "colorActivated" object)
pattern ColorActivated = SignalProxy
#else
pattern ColorActivated = SignalProxy :: forall info object. info ~ ResolveSignal "colorActivated" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ColorChanged :: SignalProxy object (ResolveSignal "colorChanged" object)
pattern ColorChanged = SignalProxy
#else
pattern ColorChanged = SignalProxy :: forall info object. info ~ ResolveSignal "colorChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ColorSet :: SignalProxy object (ResolveSignal "colorSet" object)
pattern ColorSet = SignalProxy
#else
pattern ColorSet = SignalProxy :: forall info object. info ~ ResolveSignal "colorSet" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ColumnsChanged :: SignalProxy object (ResolveSignal "columnsChanged" object)
pattern ColumnsChanged = SignalProxy
#else
pattern ColumnsChanged = SignalProxy :: forall info object. info ~ ResolveSignal "columnsChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Commit :: SignalProxy object (ResolveSignal "commit" object)
pattern Commit = SignalProxy
#else
pattern Commit = SignalProxy :: forall info object. info ~ ResolveSignal "commit" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CompositedChanged :: SignalProxy object (ResolveSignal "compositedChanged" object)
pattern CompositedChanged = SignalProxy
#else
pattern CompositedChanged = SignalProxy :: forall info object. info ~ ResolveSignal "compositedChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ConfigureEvent :: SignalProxy object (ResolveSignal "configureEvent" object)
pattern ConfigureEvent = SignalProxy
#else
pattern ConfigureEvent = SignalProxy :: forall info object. info ~ ResolveSignal "configureEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ConfirmOverwrite :: SignalProxy object (ResolveSignal "confirmOverwrite" object)
pattern ConfirmOverwrite = SignalProxy
#else
pattern ConfirmOverwrite = SignalProxy :: forall info object. info ~ ResolveSignal "confirmOverwrite" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ConnectProxy :: SignalProxy object (ResolveSignal "connectProxy" object)
pattern ConnectProxy = SignalProxy
#else
pattern ConnectProxy = SignalProxy :: forall info object. info ~ ResolveSignal "connectProxy" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ContextMenu :: SignalProxy object (ResolveSignal "contextMenu" object)
pattern ContextMenu = SignalProxy
#else
pattern ContextMenu = SignalProxy :: forall info object. info ~ ResolveSignal "contextMenu" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ContextMenuDismissed :: SignalProxy object (ResolveSignal "contextMenuDismissed" object)
pattern ContextMenuDismissed = SignalProxy
#else
pattern ContextMenuDismissed = SignalProxy :: forall info object. info ~ ResolveSignal "contextMenuDismissed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CopyClipboard :: SignalProxy object (ResolveSignal "copyClipboard" object)
pattern CopyClipboard = SignalProxy
#else
pattern CopyClipboard = SignalProxy :: forall info object. info ~ ResolveSignal "copyClipboard" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CountedMatches :: SignalProxy object (ResolveSignal "countedMatches" object)
pattern CountedMatches = SignalProxy
#else
pattern CountedMatches = SignalProxy :: forall info object. info ~ ResolveSignal "countedMatches" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Create :: SignalProxy object (ResolveSignal "create" object)
pattern Create = SignalProxy
#else
pattern Create = SignalProxy :: forall info object. info ~ ResolveSignal "create" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CreateContext :: SignalProxy object (ResolveSignal "createContext" object)
pattern CreateContext = SignalProxy
#else
pattern CreateContext = SignalProxy :: forall info object. info ~ ResolveSignal "createContext" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CreateCustomWidget :: SignalProxy object (ResolveSignal "createCustomWidget" object)
pattern CreateCustomWidget = SignalProxy
#else
pattern CreateCustomWidget = SignalProxy :: forall info object. info ~ ResolveSignal "createCustomWidget" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CreateMenuProxy :: SignalProxy object (ResolveSignal "createMenuProxy" object)
pattern CreateMenuProxy = SignalProxy
#else
pattern CreateMenuProxy = SignalProxy :: forall info object. info ~ ResolveSignal "createMenuProxy" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CreateWindow :: SignalProxy object (ResolveSignal "createWindow" object)
pattern CreateWindow = SignalProxy
#else
pattern CreateWindow = SignalProxy :: forall info object. info ~ ResolveSignal "createWindow" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CreatedDestination :: SignalProxy object (ResolveSignal "createdDestination" object)
pattern CreatedDestination = SignalProxy
#else
pattern CreatedDestination = SignalProxy :: forall info object. info ~ ResolveSignal "createdDestination" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CurrentFolderChanged :: SignalProxy object (ResolveSignal "currentFolderChanged" object)
pattern CurrentFolderChanged = SignalProxy
#else
pattern CurrentFolderChanged = SignalProxy :: forall info object. info ~ ResolveSignal "currentFolderChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CursorChanged :: SignalProxy object (ResolveSignal "cursorChanged" object)
pattern CursorChanged = SignalProxy
#else
pattern CursorChanged = SignalProxy :: forall info object. info ~ ResolveSignal "cursorChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CursorOnMatch :: SignalProxy object (ResolveSignal "cursorOnMatch" object)
pattern CursorOnMatch = SignalProxy
#else
pattern CursorOnMatch = SignalProxy :: forall info object. info ~ ResolveSignal "cursorOnMatch" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CustomItemActivated :: SignalProxy object (ResolveSignal "customItemActivated" object)
pattern CustomItemActivated = SignalProxy
#else
pattern CustomItemActivated = SignalProxy :: forall info object. info ~ ResolveSignal "customItemActivated" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CustomWidgetApply :: SignalProxy object (ResolveSignal "customWidgetApply" object)
pattern CustomWidgetApply = SignalProxy
#else
pattern CustomWidgetApply = SignalProxy :: forall info object. info ~ ResolveSignal "customWidgetApply" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CutClipboard :: SignalProxy object (ResolveSignal "cutClipboard" object)
pattern CutClipboard = SignalProxy
#else
pattern CutClipboard = SignalProxy :: forall info object. info ~ ResolveSignal "cutClipboard" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CycleChildFocus :: SignalProxy object (ResolveSignal "cycleChildFocus" object)
pattern CycleChildFocus = SignalProxy
#else
pattern CycleChildFocus = SignalProxy :: forall info object. info ~ ResolveSignal "cycleChildFocus" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CycleFocus :: SignalProxy object (ResolveSignal "cycleFocus" object)
pattern CycleFocus = SignalProxy
#else
pattern CycleFocus = SignalProxy :: forall info object. info ~ ResolveSignal "cycleFocus" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern CycleHandleFocus :: SignalProxy object (ResolveSignal "cycleHandleFocus" object)
pattern CycleHandleFocus = SignalProxy
#else
pattern CycleHandleFocus = SignalProxy :: forall info object. info ~ ResolveSignal "cycleHandleFocus" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DamageEvent :: SignalProxy object (ResolveSignal "damageEvent" object)
pattern DamageEvent = SignalProxy
#else
pattern DamageEvent = SignalProxy :: forall info object. info ~ ResolveSignal "damageEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DaySelected :: SignalProxy object (ResolveSignal "daySelected" object)
pattern DaySelected = SignalProxy
#else
pattern DaySelected = SignalProxy :: forall info object. info ~ ResolveSignal "daySelected" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DaySelectedDoubleClick :: SignalProxy object (ResolveSignal "daySelectedDoubleClick" object)
pattern DaySelectedDoubleClick = SignalProxy
#else
pattern DaySelectedDoubleClick = SignalProxy :: forall info object. info ~ ResolveSignal "daySelectedDoubleClick" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Deactivate :: SignalProxy object (ResolveSignal "deactivate" object)
pattern Deactivate = SignalProxy
#else
pattern Deactivate = SignalProxy :: forall info object. info ~ ResolveSignal "deactivate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DecideDestination :: SignalProxy object (ResolveSignal "decideDestination" object)
pattern DecideDestination = SignalProxy
#else
pattern DecideDestination = SignalProxy :: forall info object. info ~ ResolveSignal "decideDestination" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DecidePolicy :: SignalProxy object (ResolveSignal "decidePolicy" object)
pattern DecidePolicy = SignalProxy
#else
pattern DecidePolicy = SignalProxy :: forall info object. info ~ ResolveSignal "decidePolicy" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DeleteEvent :: SignalProxy object (ResolveSignal "deleteEvent" object)
pattern DeleteEvent = SignalProxy
#else
pattern DeleteEvent = SignalProxy :: forall info object. info ~ ResolveSignal "deleteEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DeleteFromCursor :: SignalProxy object (ResolveSignal "deleteFromCursor" object)
pattern DeleteFromCursor = SignalProxy
#else
pattern DeleteFromCursor = SignalProxy :: forall info object. info ~ ResolveSignal "deleteFromCursor" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DeleteRange :: SignalProxy object (ResolveSignal "deleteRange" object)
pattern DeleteRange = SignalProxy
#else
pattern DeleteRange = SignalProxy :: forall info object. info ~ ResolveSignal "deleteRange" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DeleteSurrounding :: SignalProxy object (ResolveSignal "deleteSurrounding" object)
pattern DeleteSurrounding = SignalProxy
#else
pattern DeleteSurrounding = SignalProxy :: forall info object. info ~ ResolveSignal "deleteSurrounding" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DeleteText :: SignalProxy object (ResolveSignal "deleteText" object)
pattern DeleteText = SignalProxy
#else
pattern DeleteText = SignalProxy :: forall info object. info ~ ResolveSignal "deleteText" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DeletedText :: SignalProxy object (ResolveSignal "deletedText" object)
pattern DeletedText = SignalProxy
#else
pattern DeletedText = SignalProxy :: forall info object. info ~ ResolveSignal "deletedText" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Deselect :: SignalProxy object (ResolveSignal "deselect" object)
pattern Deselect = SignalProxy
#else
pattern Deselect = SignalProxy :: forall info object. info ~ ResolveSignal "deselect" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DesktopFolder :: SignalProxy object (ResolveSignal "desktopFolder" object)
pattern DesktopFolder = SignalProxy
#else
pattern DesktopFolder = SignalProxy :: forall info object. info ~ ResolveSignal "desktopFolder" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Destroy :: SignalProxy object (ResolveSignal "destroy" object)
pattern Destroy = SignalProxy
#else
pattern Destroy = SignalProxy :: forall info object. info ~ ResolveSignal "destroy" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DestroyEvent :: SignalProxy object (ResolveSignal "destroyEvent" object)
pattern DestroyEvent = SignalProxy
#else
pattern DestroyEvent = SignalProxy :: forall info object. info ~ ResolveSignal "destroyEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Detach :: SignalProxy object (ResolveSignal "detach" object)
pattern Detach = SignalProxy
#else
pattern Detach = SignalProxy :: forall info object. info ~ ResolveSignal "detach" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DirectionChanged :: SignalProxy object (ResolveSignal "directionChanged" object)
pattern DirectionChanged = SignalProxy
#else
pattern DirectionChanged = SignalProxy :: forall info object. info ~ ResolveSignal "directionChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DisconnectProxy :: SignalProxy object (ResolveSignal "disconnectProxy" object)
pattern DisconnectProxy = SignalProxy
#else
pattern DisconnectProxy = SignalProxy :: forall info object. info ~ ResolveSignal "disconnectProxy" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Done :: SignalProxy object (ResolveSignal "done" object)
pattern Done = SignalProxy
#else
pattern Done = SignalProxy :: forall info object. info ~ ResolveSignal "done" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DownFolder :: SignalProxy object (ResolveSignal "downFolder" object)
pattern DownFolder = SignalProxy
#else
pattern DownFolder = SignalProxy :: forall info object. info ~ ResolveSignal "downFolder" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DownloadStarted :: SignalProxy object (ResolveSignal "downloadStarted" object)
pattern DownloadStarted = SignalProxy
#else
pattern DownloadStarted = SignalProxy :: forall info object. info ~ ResolveSignal "downloadStarted" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragActionAsk :: SignalProxy object (ResolveSignal "dragActionAsk" object)
pattern DragActionAsk = SignalProxy
#else
pattern DragActionAsk = SignalProxy :: forall info object. info ~ ResolveSignal "dragActionAsk" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragActionRequested :: SignalProxy object (ResolveSignal "dragActionRequested" object)
pattern DragActionRequested = SignalProxy
#else
pattern DragActionRequested = SignalProxy :: forall info object. info ~ ResolveSignal "dragActionRequested" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragBegin :: SignalProxy object (ResolveSignal "dragBegin" object)
pattern DragBegin = SignalProxy
#else
pattern DragBegin = SignalProxy :: forall info object. info ~ ResolveSignal "dragBegin" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragDataDelete :: SignalProxy object (ResolveSignal "dragDataDelete" object)
pattern DragDataDelete = SignalProxy
#else
pattern DragDataDelete = SignalProxy :: forall info object. info ~ ResolveSignal "dragDataDelete" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragDataGet :: SignalProxy object (ResolveSignal "dragDataGet" object)
pattern DragDataGet = SignalProxy
#else
pattern DragDataGet = SignalProxy :: forall info object. info ~ ResolveSignal "dragDataGet" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragDataReceived :: SignalProxy object (ResolveSignal "dragDataReceived" object)
pattern DragDataReceived = SignalProxy
#else
pattern DragDataReceived = SignalProxy :: forall info object. info ~ ResolveSignal "dragDataReceived" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragDrop :: SignalProxy object (ResolveSignal "dragDrop" object)
pattern DragDrop = SignalProxy
#else
pattern DragDrop = SignalProxy :: forall info object. info ~ ResolveSignal "dragDrop" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragEnd :: SignalProxy object (ResolveSignal "dragEnd" object)
pattern DragEnd = SignalProxy
#else
pattern DragEnd = SignalProxy :: forall info object. info ~ ResolveSignal "dragEnd" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragFailed :: SignalProxy object (ResolveSignal "dragFailed" object)
pattern DragFailed = SignalProxy
#else
pattern DragFailed = SignalProxy :: forall info object. info ~ ResolveSignal "dragFailed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragLeave :: SignalProxy object (ResolveSignal "dragLeave" object)
pattern DragLeave = SignalProxy
#else
pattern DragLeave = SignalProxy :: forall info object. info ~ ResolveSignal "dragLeave" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragMotion :: SignalProxy object (ResolveSignal "dragMotion" object)
pattern DragMotion = SignalProxy
#else
pattern DragMotion = SignalProxy :: forall info object. info ~ ResolveSignal "dragMotion" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragPerformDrop :: SignalProxy object (ResolveSignal "dragPerformDrop" object)
pattern DragPerformDrop = SignalProxy
#else
pattern DragPerformDrop = SignalProxy :: forall info object. info ~ ResolveSignal "dragPerformDrop" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DragUpdate :: SignalProxy object (ResolveSignal "dragUpdate" object)
pattern DragUpdate = SignalProxy
#else
pattern DragUpdate = SignalProxy :: forall info object. info ~ ResolveSignal "dragUpdate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Draw :: SignalProxy object (ResolveSignal "draw" object)
pattern Draw = SignalProxy
#else
pattern Draw = SignalProxy :: forall info object. info ~ ResolveSignal "draw" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern DrawPage :: SignalProxy object (ResolveSignal "drawPage" object)
pattern DrawPage = SignalProxy
#else
pattern DrawPage = SignalProxy :: forall info object. info ~ ResolveSignal "drawPage" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EdgeOvershot :: SignalProxy object (ResolveSignal "edgeOvershot" object)
pattern EdgeOvershot = SignalProxy
#else
pattern EdgeOvershot = SignalProxy :: forall info object. info ~ ResolveSignal "edgeOvershot" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EdgeReached :: SignalProxy object (ResolveSignal "edgeReached" object)
pattern EdgeReached = SignalProxy
#else
pattern EdgeReached = SignalProxy :: forall info object. info ~ ResolveSignal "edgeReached" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Edited :: SignalProxy object (ResolveSignal "edited" object)
pattern Edited = SignalProxy
#else
pattern Edited = SignalProxy :: forall info object. info ~ ResolveSignal "edited" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EditingCanceled :: SignalProxy object (ResolveSignal "editingCanceled" object)
pattern EditingCanceled = SignalProxy
#else
pattern EditingCanceled = SignalProxy :: forall info object. info ~ ResolveSignal "editingCanceled" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EditingDone :: SignalProxy object (ResolveSignal "editingDone" object)
pattern EditingDone = SignalProxy
#else
pattern EditingDone = SignalProxy :: forall info object. info ~ ResolveSignal "editingDone" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EditingStarted :: SignalProxy object (ResolveSignal "editingStarted" object)
pattern EditingStarted = SignalProxy
#else
pattern EditingStarted = SignalProxy :: forall info object. info ~ ResolveSignal "editingStarted" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Embedded :: SignalProxy object (ResolveSignal "embedded" object)
pattern Embedded = SignalProxy
#else
pattern Embedded = SignalProxy :: forall info object. info ~ ResolveSignal "embedded" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EnableDebugging :: SignalProxy object (ResolveSignal "enableDebugging" object)
pattern EnableDebugging = SignalProxy
#else
pattern EnableDebugging = SignalProxy :: forall info object. info ~ ResolveSignal "enableDebugging" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern End :: SignalProxy object (ResolveSignal "end" object)
pattern End = SignalProxy
#else
pattern End = SignalProxy :: forall info object. info ~ ResolveSignal "end" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EndPrint :: SignalProxy object (ResolveSignal "endPrint" object)
pattern EndPrint = SignalProxy
#else
pattern EndPrint = SignalProxy :: forall info object. info ~ ResolveSignal "endPrint" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EndUserAction :: SignalProxy object (ResolveSignal "endUserAction" object)
pattern EndUserAction = SignalProxy
#else
pattern EndUserAction = SignalProxy :: forall info object. info ~ ResolveSignal "endUserAction" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Enter :: SignalProxy object (ResolveSignal "enter" object)
pattern Enter = SignalProxy
#else
pattern Enter = SignalProxy :: forall info object. info ~ ResolveSignal "enter" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EnterFullscreen :: SignalProxy object (ResolveSignal "enterFullscreen" object)
pattern EnterFullscreen = SignalProxy
#else
pattern EnterFullscreen = SignalProxy :: forall info object. info ~ ResolveSignal "enterFullscreen" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EnterNotifyEvent :: SignalProxy object (ResolveSignal "enterNotifyEvent" object)
pattern EnterNotifyEvent = SignalProxy
#else
pattern EnterNotifyEvent = SignalProxy :: forall info object. info ~ ResolveSignal "enterNotifyEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Escape :: SignalProxy object (ResolveSignal "escape" object)
pattern Escape = SignalProxy
#else
pattern Escape = SignalProxy :: forall info object. info ~ ResolveSignal "escape" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Event :: SignalProxy object (ResolveSignal "event" object)
pattern Event = SignalProxy
#else
pattern Event = SignalProxy :: forall info object. info ~ ResolveSignal "event" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern EventAfter :: SignalProxy object (ResolveSignal "eventAfter" object)
pattern EventAfter = SignalProxy
#else
pattern EventAfter = SignalProxy :: forall info object. info ~ ResolveSignal "eventAfter" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ExpandCollapseCursorRow :: SignalProxy object (ResolveSignal "expandCollapseCursorRow" object)
pattern ExpandCollapseCursorRow = SignalProxy
#else
pattern ExpandCollapseCursorRow = SignalProxy :: forall info object. info ~ ResolveSignal "expandCollapseCursorRow" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ExtendSelection :: SignalProxy object (ResolveSignal "extendSelection" object)
pattern ExtendSelection = SignalProxy
#else
pattern ExtendSelection = SignalProxy :: forall info object. info ~ ResolveSignal "extendSelection" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Failed :: SignalProxy object (ResolveSignal "failed" object)
pattern Failed = SignalProxy
#else
pattern Failed = SignalProxy :: forall info object. info ~ ResolveSignal "failed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FailedToFindText :: SignalProxy object (ResolveSignal "failedToFindText" object)
pattern FailedToFindText = SignalProxy
#else
pattern FailedToFindText = SignalProxy :: forall info object. info ~ ResolveSignal "failedToFindText" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FailedWithTlsErrors :: SignalProxy object (ResolveSignal "failedWithTlsErrors" object)
pattern FailedWithTlsErrors = SignalProxy
#else
pattern FailedWithTlsErrors = SignalProxy :: forall info object. info ~ ResolveSignal "failedWithTlsErrors" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FaviconChanged :: SignalProxy object (ResolveSignal "faviconChanged" object)
pattern FaviconChanged = SignalProxy
#else
pattern FaviconChanged = SignalProxy :: forall info object. info ~ ResolveSignal "faviconChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FileActivated :: SignalProxy object (ResolveSignal "fileActivated" object)
pattern FileActivated = SignalProxy
#else
pattern FileActivated = SignalProxy :: forall info object. info ~ ResolveSignal "fileActivated" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FileSet :: SignalProxy object (ResolveSignal "fileSet" object)
pattern FileSet = SignalProxy
#else
pattern FileSet = SignalProxy :: forall info object. info ~ ResolveSignal "fileSet" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Finished :: SignalProxy object (ResolveSignal "finished" object)
pattern Finished = SignalProxy
#else
pattern Finished = SignalProxy :: forall info object. info ~ ResolveSignal "finished" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Focus :: SignalProxy object (ResolveSignal "focus" object)
pattern Focus = SignalProxy
#else
pattern Focus = SignalProxy :: forall info object. info ~ ResolveSignal "focus" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FocusChanged :: SignalProxy object (ResolveSignal "focusChanged" object)
pattern FocusChanged = SignalProxy
#else
pattern FocusChanged = SignalProxy :: forall info object. info ~ ResolveSignal "focusChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FocusHomeOrEnd :: SignalProxy object (ResolveSignal "focusHomeOrEnd" object)
pattern FocusHomeOrEnd = SignalProxy
#else
pattern FocusHomeOrEnd = SignalProxy :: forall info object. info ~ ResolveSignal "focusHomeOrEnd" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FocusInEvent :: SignalProxy object (ResolveSignal "focusInEvent" object)
pattern FocusInEvent = SignalProxy
#else
pattern FocusInEvent = SignalProxy :: forall info object. info ~ ResolveSignal "focusInEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FocusOutEvent :: SignalProxy object (ResolveSignal "focusOutEvent" object)
pattern FocusOutEvent = SignalProxy
#else
pattern FocusOutEvent = SignalProxy :: forall info object. info ~ ResolveSignal "focusOutEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FocusTab :: SignalProxy object (ResolveSignal "focusTab" object)
pattern FocusTab = SignalProxy
#else
pattern FocusTab = SignalProxy :: forall info object. info ~ ResolveSignal "focusTab" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FontActivated :: SignalProxy object (ResolveSignal "fontActivated" object)
pattern FontActivated = SignalProxy
#else
pattern FontActivated = SignalProxy :: forall info object. info ~ ResolveSignal "fontActivated" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FontSet :: SignalProxy object (ResolveSignal "fontSet" object)
pattern FontSet = SignalProxy
#else
pattern FontSet = SignalProxy :: forall info object. info ~ ResolveSignal "fontSet" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FormatEntryText :: SignalProxy object (ResolveSignal "formatEntryText" object)
pattern FormatEntryText = SignalProxy
#else
pattern FormatEntryText = SignalProxy :: forall info object. info ~ ResolveSignal "formatEntryText" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FormatValue :: SignalProxy object (ResolveSignal "formatValue" object)
pattern FormatValue = SignalProxy
#else
pattern FormatValue = SignalProxy :: forall info object. info ~ ResolveSignal "formatValue" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern FoundText :: SignalProxy object (ResolveSignal "foundText" object)
pattern FoundText = SignalProxy
#else
pattern FoundText = SignalProxy :: forall info object. info ~ ResolveSignal "foundText" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern GetChildPosition :: SignalProxy object (ResolveSignal "getChildPosition" object)
pattern GetChildPosition = SignalProxy
#else
pattern GetChildPosition = SignalProxy :: forall info object. info ~ ResolveSignal "getChildPosition" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern GotPageSize :: SignalProxy object (ResolveSignal "gotPageSize" object)
pattern GotPageSize = SignalProxy
#else
pattern GotPageSize = SignalProxy :: forall info object. info ~ ResolveSignal "gotPageSize" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern GrabBrokenEvent :: SignalProxy object (ResolveSignal "grabBrokenEvent" object)
pattern GrabBrokenEvent = SignalProxy
#else
pattern GrabBrokenEvent = SignalProxy :: forall info object. info ~ ResolveSignal "grabBrokenEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern GrabFocus :: SignalProxy object (ResolveSignal "grabFocus" object)
pattern GrabFocus = SignalProxy
#else
pattern GrabFocus = SignalProxy :: forall info object. info ~ ResolveSignal "grabFocus" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern GrabNotify :: SignalProxy object (ResolveSignal "grabNotify" object)
pattern GrabNotify = SignalProxy
#else
pattern GrabNotify = SignalProxy :: forall info object. info ~ ResolveSignal "grabNotify" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern GroupChanged :: SignalProxy object (ResolveSignal "groupChanged" object)
pattern GroupChanged = SignalProxy
#else
pattern GroupChanged = SignalProxy :: forall info object. info ~ ResolveSignal "groupChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Hide :: SignalProxy object (ResolveSignal "hide" object)
pattern Hide = SignalProxy
#else
pattern Hide = SignalProxy :: forall info object. info ~ ResolveSignal "hide" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern HierarchyChanged :: SignalProxy object (ResolveSignal "hierarchyChanged" object)
pattern HierarchyChanged = SignalProxy
#else
pattern HierarchyChanged = SignalProxy :: forall info object. info ~ ResolveSignal "hierarchyChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern HomeFolder :: SignalProxy object (ResolveSignal "homeFolder" object)
pattern HomeFolder = SignalProxy
#else
pattern HomeFolder = SignalProxy :: forall info object. info ~ ResolveSignal "homeFolder" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern IconPress :: SignalProxy object (ResolveSignal "iconPress" object)
pattern IconPress = SignalProxy
#else
pattern IconPress = SignalProxy :: forall info object. info ~ ResolveSignal "iconPress" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern IconRelease :: SignalProxy object (ResolveSignal "iconRelease" object)
pattern IconRelease = SignalProxy
#else
pattern IconRelease = SignalProxy :: forall info object. info ~ ResolveSignal "iconRelease" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern InitializeWebExtensions :: SignalProxy object (ResolveSignal "initializeWebExtensions" object)
pattern InitializeWebExtensions = SignalProxy
#else
pattern InitializeWebExtensions = SignalProxy :: forall info object. info ~ ResolveSignal "initializeWebExtensions" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Input :: SignalProxy object (ResolveSignal "input" object)
pattern Input = SignalProxy
#else
pattern Input = SignalProxy :: forall info object. info ~ ResolveSignal "input" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern InsecureContentDetected :: SignalProxy object (ResolveSignal "insecureContentDetected" object)
pattern InsecureContentDetected = SignalProxy
#else
pattern InsecureContentDetected = SignalProxy :: forall info object. info ~ ResolveSignal "insecureContentDetected" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Insert :: SignalProxy object (ResolveSignal "insert" object)
pattern Insert = SignalProxy
#else
pattern Insert = SignalProxy :: forall info object. info ~ ResolveSignal "insert" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern InsertAtCursor :: SignalProxy object (ResolveSignal "insertAtCursor" object)
pattern InsertAtCursor = SignalProxy
#else
pattern InsertAtCursor = SignalProxy :: forall info object. info ~ ResolveSignal "insertAtCursor" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern InsertChildAnchor :: SignalProxy object (ResolveSignal "insertChildAnchor" object)
pattern InsertChildAnchor = SignalProxy
#else
pattern InsertChildAnchor = SignalProxy :: forall info object. info ~ ResolveSignal "insertChildAnchor" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern InsertPixbuf :: SignalProxy object (ResolveSignal "insertPixbuf" object)
pattern InsertPixbuf = SignalProxy
#else
pattern InsertPixbuf = SignalProxy :: forall info object. info ~ ResolveSignal "insertPixbuf" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern InsertPrefix :: SignalProxy object (ResolveSignal "insertPrefix" object)
pattern InsertPrefix = SignalProxy
#else
pattern InsertPrefix = SignalProxy :: forall info object. info ~ ResolveSignal "insertPrefix" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern InsertText :: SignalProxy object (ResolveSignal "insertText" object)
pattern InsertText = SignalProxy
#else
pattern InsertText = SignalProxy :: forall info object. info ~ ResolveSignal "insertText" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern InsertedText :: SignalProxy object (ResolveSignal "insertedText" object)
pattern InsertedText = SignalProxy
#else
pattern InsertedText = SignalProxy :: forall info object. info ~ ResolveSignal "insertedText" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ItemActivated :: SignalProxy object (ResolveSignal "itemActivated" object)
pattern ItemActivated = SignalProxy
#else
pattern ItemActivated = SignalProxy :: forall info object. info ~ ResolveSignal "itemActivated" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern KeyPressEvent :: SignalProxy object (ResolveSignal "keyPressEvent" object)
pattern KeyPressEvent = SignalProxy
#else
pattern KeyPressEvent = SignalProxy :: forall info object. info ~ ResolveSignal "keyPressEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern KeyReleaseEvent :: SignalProxy object (ResolveSignal "keyReleaseEvent" object)
pattern KeyReleaseEvent = SignalProxy
#else
pattern KeyReleaseEvent = SignalProxy :: forall info object. info ~ ResolveSignal "keyReleaseEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern KeynavFailed :: SignalProxy object (ResolveSignal "keynavFailed" object)
pattern KeynavFailed = SignalProxy
#else
pattern KeynavFailed = SignalProxy :: forall info object. info ~ ResolveSignal "keynavFailed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern KeysChanged :: SignalProxy object (ResolveSignal "keysChanged" object)
pattern KeysChanged = SignalProxy
#else
pattern KeysChanged = SignalProxy :: forall info object. info ~ ResolveSignal "keysChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Leave :: SignalProxy object (ResolveSignal "leave" object)
pattern Leave = SignalProxy
#else
pattern Leave = SignalProxy :: forall info object. info ~ ResolveSignal "leave" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern LeaveFullscreen :: SignalProxy object (ResolveSignal "leaveFullscreen" object)
pattern LeaveFullscreen = SignalProxy
#else
pattern LeaveFullscreen = SignalProxy :: forall info object. info ~ ResolveSignal "leaveFullscreen" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern LeaveNotifyEvent :: SignalProxy object (ResolveSignal "leaveNotifyEvent" object)
pattern LeaveNotifyEvent = SignalProxy
#else
pattern LeaveNotifyEvent = SignalProxy :: forall info object. info ~ ResolveSignal "leaveNotifyEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern LoadChanged :: SignalProxy object (ResolveSignal "loadChanged" object)
pattern LoadChanged = SignalProxy
#else
pattern LoadChanged = SignalProxy :: forall info object. info ~ ResolveSignal "loadChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern LoadFailed :: SignalProxy object (ResolveSignal "loadFailed" object)
pattern LoadFailed = SignalProxy
#else
pattern LoadFailed = SignalProxy :: forall info object. info ~ ResolveSignal "loadFailed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern LoadFailedWithTlsErrors :: SignalProxy object (ResolveSignal "loadFailedWithTlsErrors" object)
pattern LoadFailedWithTlsErrors = SignalProxy
#else
pattern LoadFailedWithTlsErrors = SignalProxy :: forall info object. info ~ ResolveSignal "loadFailedWithTlsErrors" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern LocationPopup :: SignalProxy object (ResolveSignal "locationPopup" object)
pattern LocationPopup = SignalProxy
#else
pattern LocationPopup = SignalProxy :: forall info object. info ~ ResolveSignal "locationPopup" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern LocationPopupOnPaste :: SignalProxy object (ResolveSignal "locationPopupOnPaste" object)
pattern LocationPopupOnPaste = SignalProxy
#else
pattern LocationPopupOnPaste = SignalProxy :: forall info object. info ~ ResolveSignal "locationPopupOnPaste" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern LocationTogglePopup :: SignalProxy object (ResolveSignal "locationTogglePopup" object)
pattern LocationTogglePopup = SignalProxy
#else
pattern LocationTogglePopup = SignalProxy :: forall info object. info ~ ResolveSignal "locationTogglePopup" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Map :: SignalProxy object (ResolveSignal "map" object)
pattern Map = SignalProxy
#else
pattern Map = SignalProxy :: forall info object. info ~ ResolveSignal "map" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MapEvent :: SignalProxy object (ResolveSignal "mapEvent" object)
pattern MapEvent = SignalProxy
#else
pattern MapEvent = SignalProxy :: forall info object. info ~ ResolveSignal "mapEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MarkDeleted :: SignalProxy object (ResolveSignal "markDeleted" object)
pattern MarkDeleted = SignalProxy
#else
pattern MarkDeleted = SignalProxy :: forall info object. info ~ ResolveSignal "markDeleted" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MarkSet :: SignalProxy object (ResolveSignal "markSet" object)
pattern MarkSet = SignalProxy
#else
pattern MarkSet = SignalProxy :: forall info object. info ~ ResolveSignal "markSet" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MatchSelected :: SignalProxy object (ResolveSignal "matchSelected" object)
pattern MatchSelected = SignalProxy
#else
pattern MatchSelected = SignalProxy :: forall info object. info ~ ResolveSignal "matchSelected" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MnemonicActivate :: SignalProxy object (ResolveSignal "mnemonicActivate" object)
pattern MnemonicActivate = SignalProxy
#else
pattern MnemonicActivate = SignalProxy :: forall info object. info ~ ResolveSignal "mnemonicActivate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ModifiedChanged :: SignalProxy object (ResolveSignal "modifiedChanged" object)
pattern ModifiedChanged = SignalProxy
#else
pattern ModifiedChanged = SignalProxy :: forall info object. info ~ ResolveSignal "modifiedChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MonthChanged :: SignalProxy object (ResolveSignal "monthChanged" object)
pattern MonthChanged = SignalProxy
#else
pattern MonthChanged = SignalProxy :: forall info object. info ~ ResolveSignal "monthChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MotionNotifyEvent :: SignalProxy object (ResolveSignal "motionNotifyEvent" object)
pattern MotionNotifyEvent = SignalProxy
#else
pattern MotionNotifyEvent = SignalProxy :: forall info object. info ~ ResolveSignal "motionNotifyEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Mount :: SignalProxy object (ResolveSignal "mount" object)
pattern Mount = SignalProxy
#else
pattern Mount = SignalProxy :: forall info object. info ~ ResolveSignal "mount" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MouseTargetChanged :: SignalProxy object (ResolveSignal "mouseTargetChanged" object)
pattern MouseTargetChanged = SignalProxy
#else
pattern MouseTargetChanged = SignalProxy :: forall info object. info ~ ResolveSignal "mouseTargetChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Move :: SignalProxy object (ResolveSignal "move" object)
pattern Move = SignalProxy
#else
pattern Move = SignalProxy :: forall info object. info ~ ResolveSignal "move" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MoveActive :: SignalProxy object (ResolveSignal "moveActive" object)
pattern MoveActive = SignalProxy
#else
pattern MoveActive = SignalProxy :: forall info object. info ~ ResolveSignal "moveActive" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MoveCurrent :: SignalProxy object (ResolveSignal "moveCurrent" object)
pattern MoveCurrent = SignalProxy
#else
pattern MoveCurrent = SignalProxy :: forall info object. info ~ ResolveSignal "moveCurrent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MoveCursor :: SignalProxy object (ResolveSignal "moveCursor" object)
pattern MoveCursor = SignalProxy
#else
pattern MoveCursor = SignalProxy :: forall info object. info ~ ResolveSignal "moveCursor" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MoveFocus :: SignalProxy object (ResolveSignal "moveFocus" object)
pattern MoveFocus = SignalProxy
#else
pattern MoveFocus = SignalProxy :: forall info object. info ~ ResolveSignal "moveFocus" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MoveFocusOut :: SignalProxy object (ResolveSignal "moveFocusOut" object)
pattern MoveFocusOut = SignalProxy
#else
pattern MoveFocusOut = SignalProxy :: forall info object. info ~ ResolveSignal "moveFocusOut" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MoveHandle :: SignalProxy object (ResolveSignal "moveHandle" object)
pattern MoveHandle = SignalProxy
#else
pattern MoveHandle = SignalProxy :: forall info object. info ~ ResolveSignal "moveHandle" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MoveScroll :: SignalProxy object (ResolveSignal "moveScroll" object)
pattern MoveScroll = SignalProxy
#else
pattern MoveScroll = SignalProxy :: forall info object. info ~ ResolveSignal "moveScroll" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MoveSelected :: SignalProxy object (ResolveSignal "moveSelected" object)
pattern MoveSelected = SignalProxy
#else
pattern MoveSelected = SignalProxy :: forall info object. info ~ ResolveSignal "moveSelected" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MoveSlider :: SignalProxy object (ResolveSignal "moveSlider" object)
pattern MoveSlider = SignalProxy
#else
pattern MoveSlider = SignalProxy :: forall info object. info ~ ResolveSignal "moveSlider" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern MoveViewport :: SignalProxy object (ResolveSignal "moveViewport" object)
pattern MoveViewport = SignalProxy
#else
pattern MoveViewport = SignalProxy :: forall info object. info ~ ResolveSignal "moveViewport" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern NextMatch :: SignalProxy object (ResolveSignal "nextMatch" object)
pattern NextMatch = SignalProxy
#else
pattern NextMatch = SignalProxy :: forall info object. info ~ ResolveSignal "nextMatch" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern NextMonth :: SignalProxy object (ResolveSignal "nextMonth" object)
pattern NextMonth = SignalProxy
#else
pattern NextMonth = SignalProxy :: forall info object. info ~ ResolveSignal "nextMonth" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern NextYear :: SignalProxy object (ResolveSignal "nextYear" object)
pattern NextYear = SignalProxy
#else
pattern NextYear = SignalProxy :: forall info object. info ~ ResolveSignal "nextYear" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern NoMatches :: SignalProxy object (ResolveSignal "noMatches" object)
pattern NoMatches = SignalProxy
#else
pattern NoMatches = SignalProxy :: forall info object. info ~ ResolveSignal "noMatches" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern OffsetChanged :: SignalProxy object (ResolveSignal "offsetChanged" object)
pattern OffsetChanged = SignalProxy
#else
pattern OffsetChanged = SignalProxy :: forall info object. info ~ ResolveSignal "offsetChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern OpenLocation :: SignalProxy object (ResolveSignal "openLocation" object)
pattern OpenLocation = SignalProxy
#else
pattern OpenLocation = SignalProxy :: forall info object. info ~ ResolveSignal "openLocation" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern OpenWindow :: SignalProxy object (ResolveSignal "openWindow" object)
pattern OpenWindow = SignalProxy
#else
pattern OpenWindow = SignalProxy :: forall info object. info ~ ResolveSignal "openWindow" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern OrientationChanged :: SignalProxy object (ResolveSignal "orientationChanged" object)
pattern OrientationChanged = SignalProxy
#else
pattern OrientationChanged = SignalProxy :: forall info object. info ~ ResolveSignal "orientationChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Output :: SignalProxy object (ResolveSignal "output" object)
pattern Output = SignalProxy
#else
pattern Output = SignalProxy :: forall info object. info ~ ResolveSignal "output" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern OwnerChange :: SignalProxy object (ResolveSignal "ownerChange" object)
pattern OwnerChange = SignalProxy
#else
pattern OwnerChange = SignalProxy :: forall info object. info ~ ResolveSignal "ownerChange" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PageAdded :: SignalProxy object (ResolveSignal "pageAdded" object)
pattern PageAdded = SignalProxy
#else
pattern PageAdded = SignalProxy :: forall info object. info ~ ResolveSignal "pageAdded" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PageRemoved :: SignalProxy object (ResolveSignal "pageRemoved" object)
pattern PageRemoved = SignalProxy
#else
pattern PageRemoved = SignalProxy :: forall info object. info ~ ResolveSignal "pageRemoved" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PageReordered :: SignalProxy object (ResolveSignal "pageReordered" object)
pattern PageReordered = SignalProxy
#else
pattern PageReordered = SignalProxy :: forall info object. info ~ ResolveSignal "pageReordered" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Paginate :: SignalProxy object (ResolveSignal "paginate" object)
pattern Paginate = SignalProxy
#else
pattern Paginate = SignalProxy :: forall info object. info ~ ResolveSignal "paginate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Pan :: SignalProxy object (ResolveSignal "pan" object)
pattern Pan = SignalProxy
#else
pattern Pan = SignalProxy :: forall info object. info ~ ResolveSignal "pan" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ParentSet :: SignalProxy object (ResolveSignal "parentSet" object)
pattern ParentSet = SignalProxy
#else
pattern ParentSet = SignalProxy :: forall info object. info ~ ResolveSignal "parentSet" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ParsingError :: SignalProxy object (ResolveSignal "parsingError" object)
pattern ParsingError = SignalProxy
#else
pattern ParsingError = SignalProxy :: forall info object. info ~ ResolveSignal "parsingError" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PasteClipboard :: SignalProxy object (ResolveSignal "pasteClipboard" object)
pattern PasteClipboard = SignalProxy
#else
pattern PasteClipboard = SignalProxy :: forall info object. info ~ ResolveSignal "pasteClipboard" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PasteDone :: SignalProxy object (ResolveSignal "pasteDone" object)
pattern PasteDone = SignalProxy
#else
pattern PasteDone = SignalProxy :: forall info object. info ~ ResolveSignal "pasteDone" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PermissionRequest :: SignalProxy object (ResolveSignal "permissionRequest" object)
pattern PermissionRequest = SignalProxy
#else
pattern PermissionRequest = SignalProxy :: forall info object. info ~ ResolveSignal "permissionRequest" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PlacesShortcut :: SignalProxy object (ResolveSignal "placesShortcut" object)
pattern PlacesShortcut = SignalProxy
#else
pattern PlacesShortcut = SignalProxy :: forall info object. info ~ ResolveSignal "placesShortcut" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PlugAdded :: SignalProxy object (ResolveSignal "plugAdded" object)
pattern PlugAdded = SignalProxy
#else
pattern PlugAdded = SignalProxy :: forall info object. info ~ ResolveSignal "plugAdded" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PlugRemoved :: SignalProxy object (ResolveSignal "plugRemoved" object)
pattern PlugRemoved = SignalProxy
#else
pattern PlugRemoved = SignalProxy :: forall info object. info ~ ResolveSignal "plugRemoved" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Popdown :: SignalProxy object (ResolveSignal "popdown" object)
pattern Popdown = SignalProxy
#else
pattern Popdown = SignalProxy :: forall info object. info ~ ResolveSignal "popdown" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PopulatePopup :: SignalProxy object (ResolveSignal "populatePopup" object)
pattern PopulatePopup = SignalProxy
#else
pattern PopulatePopup = SignalProxy :: forall info object. info ~ ResolveSignal "populatePopup" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Popup :: SignalProxy object (ResolveSignal "popup" object)
pattern Popup = SignalProxy
#else
pattern Popup = SignalProxy :: forall info object. info ~ ResolveSignal "popup" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PopupContextMenu :: SignalProxy object (ResolveSignal "popupContextMenu" object)
pattern PopupContextMenu = SignalProxy
#else
pattern PopupContextMenu = SignalProxy :: forall info object. info ~ ResolveSignal "popupContextMenu" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PopupMenu :: SignalProxy object (ResolveSignal "popupMenu" object)
pattern PopupMenu = SignalProxy
#else
pattern PopupMenu = SignalProxy :: forall info object. info ~ ResolveSignal "popupMenu" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PostActivate :: SignalProxy object (ResolveSignal "postActivate" object)
pattern PostActivate = SignalProxy
#else
pattern PostActivate = SignalProxy :: forall info object. info ~ ResolveSignal "postActivate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PreActivate :: SignalProxy object (ResolveSignal "preActivate" object)
pattern PreActivate = SignalProxy
#else
pattern PreActivate = SignalProxy :: forall info object. info ~ ResolveSignal "preActivate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PreeditChanged :: SignalProxy object (ResolveSignal "preeditChanged" object)
pattern PreeditChanged = SignalProxy
#else
pattern PreeditChanged = SignalProxy :: forall info object. info ~ ResolveSignal "preeditChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PreeditEnd :: SignalProxy object (ResolveSignal "preeditEnd" object)
pattern PreeditEnd = SignalProxy
#else
pattern PreeditEnd = SignalProxy :: forall info object. info ~ ResolveSignal "preeditEnd" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PreeditStart :: SignalProxy object (ResolveSignal "preeditStart" object)
pattern PreeditStart = SignalProxy
#else
pattern PreeditStart = SignalProxy :: forall info object. info ~ ResolveSignal "preeditStart" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Prepare :: SignalProxy object (ResolveSignal "prepare" object)
pattern Prepare = SignalProxy
#else
pattern Prepare = SignalProxy :: forall info object. info ~ ResolveSignal "prepare" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Pressed :: SignalProxy object (ResolveSignal "pressed" object)
pattern Pressed = SignalProxy
#else
pattern Pressed = SignalProxy :: forall info object. info ~ ResolveSignal "pressed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PrevMonth :: SignalProxy object (ResolveSignal "prevMonth" object)
pattern PrevMonth = SignalProxy
#else
pattern PrevMonth = SignalProxy :: forall info object. info ~ ResolveSignal "prevMonth" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PrevYear :: SignalProxy object (ResolveSignal "prevYear" object)
pattern PrevYear = SignalProxy
#else
pattern PrevYear = SignalProxy :: forall info object. info ~ ResolveSignal "prevYear" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Preview :: SignalProxy object (ResolveSignal "preview" object)
pattern Preview = SignalProxy
#else
pattern Preview = SignalProxy :: forall info object. info ~ ResolveSignal "preview" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PreviousMatch :: SignalProxy object (ResolveSignal "previousMatch" object)
pattern PreviousMatch = SignalProxy
#else
pattern PreviousMatch = SignalProxy :: forall info object. info ~ ResolveSignal "previousMatch" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Print :: SignalProxy object (ResolveSignal "print" object)
pattern Print = SignalProxy
#else
pattern Print = SignalProxy :: forall info object. info ~ ResolveSignal "print" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern PropertyNotifyEvent :: SignalProxy object (ResolveSignal "propertyNotifyEvent" object)
pattern PropertyNotifyEvent = SignalProxy
#else
pattern PropertyNotifyEvent = SignalProxy :: forall info object. info ~ ResolveSignal "propertyNotifyEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ProximityInEvent :: SignalProxy object (ResolveSignal "proximityInEvent" object)
pattern ProximityInEvent = SignalProxy
#else
pattern ProximityInEvent = SignalProxy :: forall info object. info ~ ResolveSignal "proximityInEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ProximityOutEvent :: SignalProxy object (ResolveSignal "proximityOutEvent" object)
pattern ProximityOutEvent = SignalProxy
#else
pattern ProximityOutEvent = SignalProxy :: forall info object. info ~ ResolveSignal "proximityOutEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern QueryTooltip :: SignalProxy object (ResolveSignal "queryTooltip" object)
pattern QueryTooltip = SignalProxy
#else
pattern QueryTooltip = SignalProxy :: forall info object. info ~ ResolveSignal "queryTooltip" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern QuickBookmark :: SignalProxy object (ResolveSignal "quickBookmark" object)
pattern QuickBookmark = SignalProxy
#else
pattern QuickBookmark = SignalProxy :: forall info object. info ~ ResolveSignal "quickBookmark" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Ready :: SignalProxy object (ResolveSignal "ready" object)
pattern Ready = SignalProxy
#else
pattern Ready = SignalProxy :: forall info object. info ~ ResolveSignal "ready" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ReadyToShow :: SignalProxy object (ResolveSignal "readyToShow" object)
pattern ReadyToShow = SignalProxy
#else
pattern ReadyToShow = SignalProxy :: forall info object. info ~ ResolveSignal "readyToShow" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Realize :: SignalProxy object (ResolveSignal "realize" object)
pattern Realize = SignalProxy
#else
pattern Realize = SignalProxy :: forall info object. info ~ ResolveSignal "realize" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ReceivedData :: SignalProxy object (ResolveSignal "receivedData" object)
pattern ReceivedData = SignalProxy
#else
pattern ReceivedData = SignalProxy :: forall info object. info ~ ResolveSignal "receivedData" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RecentShortcut :: SignalProxy object (ResolveSignal "recentShortcut" object)
pattern RecentShortcut = SignalProxy
#else
pattern RecentShortcut = SignalProxy :: forall info object. info ~ ResolveSignal "recentShortcut" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Released :: SignalProxy object (ResolveSignal "released" object)
pattern Released = SignalProxy
#else
pattern Released = SignalProxy :: forall info object. info ~ ResolveSignal "released" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Remove :: SignalProxy object (ResolveSignal "remove" object)
pattern Remove = SignalProxy
#else
pattern Remove = SignalProxy :: forall info object. info ~ ResolveSignal "remove" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RemoveEditable :: SignalProxy object (ResolveSignal "removeEditable" object)
pattern RemoveEditable = SignalProxy
#else
pattern RemoveEditable = SignalProxy :: forall info object. info ~ ResolveSignal "removeEditable" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RemoveTag :: SignalProxy object (ResolveSignal "removeTag" object)
pattern RemoveTag = SignalProxy
#else
pattern RemoveTag = SignalProxy :: forall info object. info ~ ResolveSignal "removeTag" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RemoveWidget :: SignalProxy object (ResolveSignal "removeWidget" object)
pattern RemoveWidget = SignalProxy
#else
pattern RemoveWidget = SignalProxy :: forall info object. info ~ ResolveSignal "removeWidget" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Render :: SignalProxy object (ResolveSignal "render" object)
pattern Render = SignalProxy
#else
pattern Render = SignalProxy :: forall info object. info ~ ResolveSignal "render" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ReorderTab :: SignalProxy object (ResolveSignal "reorderTab" object)
pattern ReorderTab = SignalProxy
#else
pattern ReorderTab = SignalProxy :: forall info object. info ~ ResolveSignal "reorderTab" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RequestPageSetup :: SignalProxy object (ResolveSignal "requestPageSetup" object)
pattern RequestPageSetup = SignalProxy
#else
pattern RequestPageSetup = SignalProxy :: forall info object. info ~ ResolveSignal "requestPageSetup" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Resize :: SignalProxy object (ResolveSignal "resize" object)
pattern Resize = SignalProxy
#else
pattern Resize = SignalProxy :: forall info object. info ~ ResolveSignal "resize" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ResourceLoadStarted :: SignalProxy object (ResolveSignal "resourceLoadStarted" object)
pattern ResourceLoadStarted = SignalProxy
#else
pattern ResourceLoadStarted = SignalProxy :: forall info object. info ~ ResolveSignal "resourceLoadStarted" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Response :: SignalProxy object (ResolveSignal "response" object)
pattern Response = SignalProxy
#else
pattern Response = SignalProxy :: forall info object. info ~ ResolveSignal "response" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RetrieveSurrounding :: SignalProxy object (ResolveSignal "retrieveSurrounding" object)
pattern RetrieveSurrounding = SignalProxy
#else
pattern RetrieveSurrounding = SignalProxy :: forall info object. info ~ ResolveSignal "retrieveSurrounding" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RowActivated :: SignalProxy object (ResolveSignal "rowActivated" object)
pattern RowActivated = SignalProxy
#else
pattern RowActivated = SignalProxy :: forall info object. info ~ ResolveSignal "rowActivated" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RowChanged :: SignalProxy object (ResolveSignal "rowChanged" object)
pattern RowChanged = SignalProxy
#else
pattern RowChanged = SignalProxy :: forall info object. info ~ ResolveSignal "rowChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RowCollapsed :: SignalProxy object (ResolveSignal "rowCollapsed" object)
pattern RowCollapsed = SignalProxy
#else
pattern RowCollapsed = SignalProxy :: forall info object. info ~ ResolveSignal "rowCollapsed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RowDeleted :: SignalProxy object (ResolveSignal "rowDeleted" object)
pattern RowDeleted = SignalProxy
#else
pattern RowDeleted = SignalProxy :: forall info object. info ~ ResolveSignal "rowDeleted" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RowExpanded :: SignalProxy object (ResolveSignal "rowExpanded" object)
pattern RowExpanded = SignalProxy
#else
pattern RowExpanded = SignalProxy :: forall info object. info ~ ResolveSignal "rowExpanded" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RowHasChildToggled :: SignalProxy object (ResolveSignal "rowHasChildToggled" object)
pattern RowHasChildToggled = SignalProxy
#else
pattern RowHasChildToggled = SignalProxy :: forall info object. info ~ ResolveSignal "rowHasChildToggled" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RowInserted :: SignalProxy object (ResolveSignal "rowInserted" object)
pattern RowInserted = SignalProxy
#else
pattern RowInserted = SignalProxy :: forall info object. info ~ ResolveSignal "rowInserted" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RowSelected :: SignalProxy object (ResolveSignal "rowSelected" object)
pattern RowSelected = SignalProxy
#else
pattern RowSelected = SignalProxy :: forall info object. info ~ ResolveSignal "rowSelected" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RunAsModal :: SignalProxy object (ResolveSignal "runAsModal" object)
pattern RunAsModal = SignalProxy
#else
pattern RunAsModal = SignalProxy :: forall info object. info ~ ResolveSignal "runAsModal" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RunColorChooser :: SignalProxy object (ResolveSignal "runColorChooser" object)
pattern RunColorChooser = SignalProxy
#else
pattern RunColorChooser = SignalProxy :: forall info object. info ~ ResolveSignal "runColorChooser" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern RunFileChooser :: SignalProxy object (ResolveSignal "runFileChooser" object)
pattern RunFileChooser = SignalProxy
#else
pattern RunFileChooser = SignalProxy :: forall info object. info ~ ResolveSignal "runFileChooser" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ScaleChanged :: SignalProxy object (ResolveSignal "scaleChanged" object)
pattern ScaleChanged = SignalProxy
#else
pattern ScaleChanged = SignalProxy :: forall info object. info ~ ResolveSignal "scaleChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ScreenChanged :: SignalProxy object (ResolveSignal "screenChanged" object)
pattern ScreenChanged = SignalProxy
#else
pattern ScreenChanged = SignalProxy :: forall info object. info ~ ResolveSignal "screenChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ScriptDialog :: SignalProxy object (ResolveSignal "scriptDialog" object)
pattern ScriptDialog = SignalProxy
#else
pattern ScriptDialog = SignalProxy :: forall info object. info ~ ResolveSignal "scriptDialog" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ScriptMessageReceived :: SignalProxy object (ResolveSignal "scriptMessageReceived" object)
pattern ScriptMessageReceived = SignalProxy
#else
pattern ScriptMessageReceived = SignalProxy :: forall info object. info ~ ResolveSignal "scriptMessageReceived" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ScrollChild :: SignalProxy object (ResolveSignal "scrollChild" object)
pattern ScrollChild = SignalProxy
#else
pattern ScrollChild = SignalProxy :: forall info object. info ~ ResolveSignal "scrollChild" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ScrollEvent :: SignalProxy object (ResolveSignal "scrollEvent" object)
pattern ScrollEvent = SignalProxy
#else
pattern ScrollEvent = SignalProxy :: forall info object. info ~ ResolveSignal "scrollEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Search :: SignalProxy object (ResolveSignal "search" object)
pattern Search = SignalProxy
#else
pattern Search = SignalProxy :: forall info object. info ~ ResolveSignal "search" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SearchChanged :: SignalProxy object (ResolveSignal "searchChanged" object)
pattern SearchChanged = SignalProxy
#else
pattern SearchChanged = SignalProxy :: forall info object. info ~ ResolveSignal "searchChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SearchShortcut :: SignalProxy object (ResolveSignal "searchShortcut" object)
pattern SearchShortcut = SignalProxy
#else
pattern SearchShortcut = SignalProxy :: forall info object. info ~ ResolveSignal "searchShortcut" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Select :: SignalProxy object (ResolveSignal "select" object)
pattern Select = SignalProxy
#else
pattern Select = SignalProxy :: forall info object. info ~ ResolveSignal "select" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectAll :: SignalProxy object (ResolveSignal "selectAll" object)
pattern SelectAll = SignalProxy
#else
pattern SelectAll = SignalProxy :: forall info object. info ~ ResolveSignal "selectAll" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectCursorItem :: SignalProxy object (ResolveSignal "selectCursorItem" object)
pattern SelectCursorItem = SignalProxy
#else
pattern SelectCursorItem = SignalProxy :: forall info object. info ~ ResolveSignal "selectCursorItem" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectCursorParent :: SignalProxy object (ResolveSignal "selectCursorParent" object)
pattern SelectCursorParent = SignalProxy
#else
pattern SelectCursorParent = SignalProxy :: forall info object. info ~ ResolveSignal "selectCursorParent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectCursorRow :: SignalProxy object (ResolveSignal "selectCursorRow" object)
pattern SelectCursorRow = SignalProxy
#else
pattern SelectCursorRow = SignalProxy :: forall info object. info ~ ResolveSignal "selectCursorRow" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectPage :: SignalProxy object (ResolveSignal "selectPage" object)
pattern SelectPage = SignalProxy
#else
pattern SelectPage = SignalProxy :: forall info object. info ~ ResolveSignal "selectPage" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectedChildrenChanged :: SignalProxy object (ResolveSignal "selectedChildrenChanged" object)
pattern SelectedChildrenChanged = SignalProxy
#else
pattern SelectedChildrenChanged = SignalProxy :: forall info object. info ~ ResolveSignal "selectedChildrenChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectedRowsChanged :: SignalProxy object (ResolveSignal "selectedRowsChanged" object)
pattern SelectedRowsChanged = SignalProxy
#else
pattern SelectedRowsChanged = SignalProxy :: forall info object. info ~ ResolveSignal "selectedRowsChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectionChanged :: SignalProxy object (ResolveSignal "selectionChanged" object)
pattern SelectionChanged = SignalProxy
#else
pattern SelectionChanged = SignalProxy :: forall info object. info ~ ResolveSignal "selectionChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectionClearEvent :: SignalProxy object (ResolveSignal "selectionClearEvent" object)
pattern SelectionClearEvent = SignalProxy
#else
pattern SelectionClearEvent = SignalProxy :: forall info object. info ~ ResolveSignal "selectionClearEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectionDone :: SignalProxy object (ResolveSignal "selectionDone" object)
pattern SelectionDone = SignalProxy
#else
pattern SelectionDone = SignalProxy :: forall info object. info ~ ResolveSignal "selectionDone" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectionGet :: SignalProxy object (ResolveSignal "selectionGet" object)
pattern SelectionGet = SignalProxy
#else
pattern SelectionGet = SignalProxy :: forall info object. info ~ ResolveSignal "selectionGet" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectionNotifyEvent :: SignalProxy object (ResolveSignal "selectionNotifyEvent" object)
pattern SelectionNotifyEvent = SignalProxy
#else
pattern SelectionNotifyEvent = SignalProxy :: forall info object. info ~ ResolveSignal "selectionNotifyEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectionReceived :: SignalProxy object (ResolveSignal "selectionReceived" object)
pattern SelectionReceived = SignalProxy
#else
pattern SelectionReceived = SignalProxy :: forall info object. info ~ ResolveSignal "selectionReceived" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SelectionRequestEvent :: SignalProxy object (ResolveSignal "selectionRequestEvent" object)
pattern SelectionRequestEvent = SignalProxy
#else
pattern SelectionRequestEvent = SignalProxy :: forall info object. info ~ ResolveSignal "selectionRequestEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SentRequest :: SignalProxy object (ResolveSignal "sentRequest" object)
pattern SentRequest = SignalProxy
#else
pattern SentRequest = SignalProxy :: forall info object. info ~ ResolveSignal "sentRequest" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SequenceStateChanged :: SignalProxy object (ResolveSignal "sequenceStateChanged" object)
pattern SequenceStateChanged = SignalProxy
#else
pattern SequenceStateChanged = SignalProxy :: forall info object. info ~ ResolveSignal "sequenceStateChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SetAnchor :: SignalProxy object (ResolveSignal "setAnchor" object)
pattern SetAnchor = SignalProxy
#else
pattern SetAnchor = SignalProxy :: forall info object. info ~ ResolveSignal "setAnchor" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SetFocus :: SignalProxy object (ResolveSignal "setFocus" object)
pattern SetFocus = SignalProxy
#else
pattern SetFocus = SignalProxy :: forall info object. info ~ ResolveSignal "setFocus" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SetFocusChild :: SignalProxy object (ResolveSignal "setFocusChild" object)
pattern SetFocusChild = SignalProxy
#else
pattern SetFocusChild = SignalProxy :: forall info object. info ~ ResolveSignal "setFocusChild" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Show :: SignalProxy object (ResolveSignal "show" object)
pattern Show = SignalProxy
#else
pattern Show = SignalProxy :: forall info object. info ~ ResolveSignal "show" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ShowConnectToServer :: SignalProxy object (ResolveSignal "showConnectToServer" object)
pattern ShowConnectToServer = SignalProxy
#else
pattern ShowConnectToServer = SignalProxy :: forall info object. info ~ ResolveSignal "showConnectToServer" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ShowEnterLocation :: SignalProxy object (ResolveSignal "showEnterLocation" object)
pattern ShowEnterLocation = SignalProxy
#else
pattern ShowEnterLocation = SignalProxy :: forall info object. info ~ ResolveSignal "showEnterLocation" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ShowErrorMessage :: SignalProxy object (ResolveSignal "showErrorMessage" object)
pattern ShowErrorMessage = SignalProxy
#else
pattern ShowErrorMessage = SignalProxy :: forall info object. info ~ ResolveSignal "showErrorMessage" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ShowHelp :: SignalProxy object (ResolveSignal "showHelp" object)
pattern ShowHelp = SignalProxy
#else
pattern ShowHelp = SignalProxy :: forall info object. info ~ ResolveSignal "showHelp" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ShowHidden :: SignalProxy object (ResolveSignal "showHidden" object)
pattern ShowHidden = SignalProxy
#else
pattern ShowHidden = SignalProxy :: forall info object. info ~ ResolveSignal "showHidden" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ShowMenu :: SignalProxy object (ResolveSignal "showMenu" object)
pattern ShowMenu = SignalProxy
#else
pattern ShowMenu = SignalProxy :: forall info object. info ~ ResolveSignal "showMenu" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ShowNotification :: SignalProxy object (ResolveSignal "showNotification" object)
pattern ShowNotification = SignalProxy
#else
pattern ShowNotification = SignalProxy :: forall info object. info ~ ResolveSignal "showNotification" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ShowOtherLocations :: SignalProxy object (ResolveSignal "showOtherLocations" object)
pattern ShowOtherLocations = SignalProxy
#else
pattern ShowOtherLocations = SignalProxy :: forall info object. info ~ ResolveSignal "showOtherLocations" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ShowOtherLocationsWithFlags :: SignalProxy object (ResolveSignal "showOtherLocationsWithFlags" object)
pattern ShowOtherLocationsWithFlags = SignalProxy
#else
pattern ShowOtherLocationsWithFlags = SignalProxy :: forall info object. info ~ ResolveSignal "showOtherLocationsWithFlags" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SizeAllocate :: SignalProxy object (ResolveSignal "sizeAllocate" object)
pattern SizeAllocate = SignalProxy
#else
pattern SizeAllocate = SignalProxy :: forall info object. info ~ ResolveSignal "sizeAllocate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SizeChanged :: SignalProxy object (ResolveSignal "sizeChanged" object)
pattern SizeChanged = SignalProxy
#else
pattern SizeChanged = SignalProxy :: forall info object. info ~ ResolveSignal "sizeChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SortColumnChanged :: SignalProxy object (ResolveSignal "sortColumnChanged" object)
pattern SortColumnChanged = SignalProxy
#else
pattern SortColumnChanged = SignalProxy :: forall info object. info ~ ResolveSignal "sortColumnChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern StartInteractiveSearch :: SignalProxy object (ResolveSignal "startInteractiveSearch" object)
pattern StartInteractiveSearch = SignalProxy
#else
pattern StartInteractiveSearch = SignalProxy :: forall info object. info ~ ResolveSignal "startInteractiveSearch" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern StateChanged :: SignalProxy object (ResolveSignal "stateChanged" object)
pattern StateChanged = SignalProxy
#else
pattern StateChanged = SignalProxy :: forall info object. info ~ ResolveSignal "stateChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern StateFlagsChanged :: SignalProxy object (ResolveSignal "stateFlagsChanged" object)
pattern StateFlagsChanged = SignalProxy
#else
pattern StateFlagsChanged = SignalProxy :: forall info object. info ~ ResolveSignal "stateFlagsChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern StateSet :: SignalProxy object (ResolveSignal "stateSet" object)
pattern StateSet = SignalProxy
#else
pattern StateSet = SignalProxy :: forall info object. info ~ ResolveSignal "stateSet" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern StatusChanged :: SignalProxy object (ResolveSignal "statusChanged" object)
pattern StatusChanged = SignalProxy
#else
pattern StatusChanged = SignalProxy :: forall info object. info ~ ResolveSignal "statusChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern StopSearch :: SignalProxy object (ResolveSignal "stopSearch" object)
pattern StopSearch = SignalProxy
#else
pattern StopSearch = SignalProxy :: forall info object. info ~ ResolveSignal "stopSearch" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Stopped :: SignalProxy object (ResolveSignal "stopped" object)
pattern Stopped = SignalProxy
#else
pattern Stopped = SignalProxy :: forall info object. info ~ ResolveSignal "stopped" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern StyleChanged :: SignalProxy object (ResolveSignal "styleChanged" object)
pattern StyleChanged = SignalProxy
#else
pattern StyleChanged = SignalProxy :: forall info object. info ~ ResolveSignal "styleChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern StyleSet :: SignalProxy object (ResolveSignal "styleSet" object)
pattern StyleSet = SignalProxy
#else
pattern StyleSet = SignalProxy :: forall info object. info ~ ResolveSignal "styleSet" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern StyleUpdated :: SignalProxy object (ResolveSignal "styleUpdated" object)
pattern StyleUpdated = SignalProxy
#else
pattern StyleUpdated = SignalProxy :: forall info object. info ~ ResolveSignal "styleUpdated" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SubmitForm :: SignalProxy object (ResolveSignal "submitForm" object)
pattern SubmitForm = SignalProxy
#else
pattern SubmitForm = SignalProxy :: forall info object. info ~ ResolveSignal "submitForm" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Swipe :: SignalProxy object (ResolveSignal "swipe" object)
pattern Swipe = SignalProxy
#else
pattern Swipe = SignalProxy :: forall info object. info ~ ResolveSignal "swipe" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern SwitchPage :: SignalProxy object (ResolveSignal "switchPage" object)
pattern SwitchPage = SignalProxy
#else
pattern SwitchPage = SignalProxy :: forall info object. info ~ ResolveSignal "switchPage" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern TagAdded :: SignalProxy object (ResolveSignal "tagAdded" object)
pattern TagAdded = SignalProxy
#else
pattern TagAdded = SignalProxy :: forall info object. info ~ ResolveSignal "tagAdded" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern TagChanged :: SignalProxy object (ResolveSignal "tagChanged" object)
pattern TagChanged = SignalProxy
#else
pattern TagChanged = SignalProxy :: forall info object. info ~ ResolveSignal "tagChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern TagRemoved :: SignalProxy object (ResolveSignal "tagRemoved" object)
pattern TagRemoved = SignalProxy
#else
pattern TagRemoved = SignalProxy :: forall info object. info ~ ResolveSignal "tagRemoved" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern TestCollapseRow :: SignalProxy object (ResolveSignal "testCollapseRow" object)
pattern TestCollapseRow = SignalProxy
#else
pattern TestCollapseRow = SignalProxy :: forall info object. info ~ ResolveSignal "testCollapseRow" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern TestExpandRow :: SignalProxy object (ResolveSignal "testExpandRow" object)
pattern TestExpandRow = SignalProxy
#else
pattern TestExpandRow = SignalProxy :: forall info object. info ~ ResolveSignal "testExpandRow" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern TextPopped :: SignalProxy object (ResolveSignal "textPopped" object)
pattern TextPopped = SignalProxy
#else
pattern TextPopped = SignalProxy :: forall info object. info ~ ResolveSignal "textPopped" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern TextPushed :: SignalProxy object (ResolveSignal "textPushed" object)
pattern TextPushed = SignalProxy
#else
pattern TextPushed = SignalProxy :: forall info object. info ~ ResolveSignal "textPushed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ToggleCursorChild :: SignalProxy object (ResolveSignal "toggleCursorChild" object)
pattern ToggleCursorChild = SignalProxy
#else
pattern ToggleCursorChild = SignalProxy :: forall info object. info ~ ResolveSignal "toggleCursorChild" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ToggleCursorItem :: SignalProxy object (ResolveSignal "toggleCursorItem" object)
pattern ToggleCursorItem = SignalProxy
#else
pattern ToggleCursorItem = SignalProxy :: forall info object. info ~ ResolveSignal "toggleCursorItem" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ToggleCursorRow :: SignalProxy object (ResolveSignal "toggleCursorRow" object)
pattern ToggleCursorRow = SignalProxy
#else
pattern ToggleCursorRow = SignalProxy :: forall info object. info ~ ResolveSignal "toggleCursorRow" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ToggleCursorVisible :: SignalProxy object (ResolveSignal "toggleCursorVisible" object)
pattern ToggleCursorVisible = SignalProxy
#else
pattern ToggleCursorVisible = SignalProxy :: forall info object. info ~ ResolveSignal "toggleCursorVisible" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ToggleHandleFocus :: SignalProxy object (ResolveSignal "toggleHandleFocus" object)
pattern ToggleHandleFocus = SignalProxy
#else
pattern ToggleHandleFocus = SignalProxy :: forall info object. info ~ ResolveSignal "toggleHandleFocus" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ToggleOverwrite :: SignalProxy object (ResolveSignal "toggleOverwrite" object)
pattern ToggleOverwrite = SignalProxy
#else
pattern ToggleOverwrite = SignalProxy :: forall info object. info ~ ResolveSignal "toggleOverwrite" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ToggleSizeAllocate :: SignalProxy object (ResolveSignal "toggleSizeAllocate" object)
pattern ToggleSizeAllocate = SignalProxy
#else
pattern ToggleSizeAllocate = SignalProxy :: forall info object. info ~ ResolveSignal "toggleSizeAllocate" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ToggleSizeRequest :: SignalProxy object (ResolveSignal "toggleSizeRequest" object)
pattern ToggleSizeRequest = SignalProxy
#else
pattern ToggleSizeRequest = SignalProxy :: forall info object. info ~ ResolveSignal "toggleSizeRequest" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Toggled :: SignalProxy object (ResolveSignal "toggled" object)
pattern Toggled = SignalProxy
#else
pattern Toggled = SignalProxy :: forall info object. info ~ ResolveSignal "toggled" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ToolbarReconfigured :: SignalProxy object (ResolveSignal "toolbarReconfigured" object)
pattern ToolbarReconfigured = SignalProxy
#else
pattern ToolbarReconfigured = SignalProxy :: forall info object. info ~ ResolveSignal "toolbarReconfigured" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern TouchEvent :: SignalProxy object (ResolveSignal "touchEvent" object)
pattern TouchEvent = SignalProxy
#else
pattern TouchEvent = SignalProxy :: forall info object. info ~ ResolveSignal "touchEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Unmap :: SignalProxy object (ResolveSignal "unmap" object)
pattern Unmap = SignalProxy
#else
pattern Unmap = SignalProxy :: forall info object. info ~ ResolveSignal "unmap" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern UnmapEvent :: SignalProxy object (ResolveSignal "unmapEvent" object)
pattern UnmapEvent = SignalProxy
#else
pattern UnmapEvent = SignalProxy :: forall info object. info ~ ResolveSignal "unmapEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Unmount :: SignalProxy object (ResolveSignal "unmount" object)
pattern Unmount = SignalProxy
#else
pattern Unmount = SignalProxy :: forall info object. info ~ ResolveSignal "unmount" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Unrealize :: SignalProxy object (ResolveSignal "unrealize" object)
pattern Unrealize = SignalProxy
#else
pattern Unrealize = SignalProxy :: forall info object. info ~ ResolveSignal "unrealize" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern UnselectAll :: SignalProxy object (ResolveSignal "unselectAll" object)
pattern UnselectAll = SignalProxy
#else
pattern UnselectAll = SignalProxy :: forall info object. info ~ ResolveSignal "unselectAll" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern UpFolder :: SignalProxy object (ResolveSignal "upFolder" object)
pattern UpFolder = SignalProxy
#else
pattern UpFolder = SignalProxy :: forall info object. info ~ ResolveSignal "upFolder" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Update :: SignalProxy object (ResolveSignal "update" object)
pattern Update = SignalProxy
#else
pattern Update = SignalProxy :: forall info object. info ~ ResolveSignal "update" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern UpdateCustomWidget :: SignalProxy object (ResolveSignal "updateCustomWidget" object)
pattern UpdateCustomWidget = SignalProxy
#else
pattern UpdateCustomWidget = SignalProxy :: forall info object. info ~ ResolveSignal "updateCustomWidget" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern UpdatePreview :: SignalProxy object (ResolveSignal "updatePreview" object)
pattern UpdatePreview = SignalProxy
#else
pattern UpdatePreview = SignalProxy :: forall info object. info ~ ResolveSignal "updatePreview" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern ValueChanged :: SignalProxy object (ResolveSignal "valueChanged" object)
pattern ValueChanged = SignalProxy
#else
pattern ValueChanged = SignalProxy :: forall info object. info ~ ResolveSignal "valueChanged" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern VisibilityNotifyEvent :: SignalProxy object (ResolveSignal "visibilityNotifyEvent" object)
pattern VisibilityNotifyEvent = SignalProxy
#else
pattern VisibilityNotifyEvent = SignalProxy :: forall info object. info ~ ResolveSignal "visibilityNotifyEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern WebProcessCrashed :: SignalProxy object (ResolveSignal "webProcessCrashed" object)
pattern WebProcessCrashed = SignalProxy
#else
pattern WebProcessCrashed = SignalProxy :: forall info object. info ~ ResolveSignal "webProcessCrashed" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern WindowAdded :: SignalProxy object (ResolveSignal "windowAdded" object)
pattern WindowAdded = SignalProxy
#else
pattern WindowAdded = SignalProxy :: forall info object. info ~ ResolveSignal "windowAdded" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern WindowRemoved :: SignalProxy object (ResolveSignal "windowRemoved" object)
pattern WindowRemoved = SignalProxy
#else
pattern WindowRemoved = SignalProxy :: forall info object. info ~ ResolveSignal "windowRemoved" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern WindowStateEvent :: SignalProxy object (ResolveSignal "windowStateEvent" object)
pattern WindowStateEvent = SignalProxy
#else
pattern WindowStateEvent = SignalProxy :: forall info object. info ~ ResolveSignal "windowStateEvent" object => SignalProxy object info
#endif

#if MIN_VERSION_base(4,8,0)
pattern Wrapped :: SignalProxy object (ResolveSignal "wrapped" object)
pattern Wrapped = SignalProxy
#else
pattern Wrapped = SignalProxy :: forall info object. info ~ ResolveSignal "wrapped" object => SignalProxy object info
#endif


