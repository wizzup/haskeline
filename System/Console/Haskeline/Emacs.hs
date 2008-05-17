module System.Console.Haskeline.Emacs where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Command.Completion
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.LineState
import System.Console.Haskeline.HaskLineT
import System.Console.Haskeline.Monads

import Data.Char

type HaskLineCmd s t = forall m . MonadIO m => Command (HaskLineCmdT m) s t

emacsCommands :: MonadIO m => KeyMap (HaskLineCmdT m) InsertMode
emacsCommands = runCommand $ choiceCmd [simpleActions, controlActions]

simpleActions, controlActions :: HaskLineCmd InsertMode InsertMode
simpleActions = choiceCmd 
            [ KeyChar '\n' +> finish
            , KeyLeft +> change goLeft
            , KeyRight +> change goRight
            , Backspace +> change deletePrev
            , DeleteForward +> change deleteNext 
            , acceptChar insertChar
            , KeyChar '\t' +> completionCmd
            , KeyUp +> historyBack
            , KeyDown +> historyForward
            ] 
            
controlActions = choiceCmd
            [ controlKey 'a' +> change moveToStart 
            , controlKey 'e' +> change moveToEnd
            , controlKey 'b' +> change goLeft
            , controlKey 'c' +> change goRight
            , controlKey 'd' +> deleteCharOrEOF
            , controlKey 'l' +> clearScreenCmd
            , KeyMeta 'f' +> change (skipRight isAlphaNum
                                     . skipRight (not . isAlphaNum))
            , KeyMeta 'b' +> change (skipLeft isAlphaNum
                                     . skipLeft (not . isAlphaNum))
            ]

deleteCharOrEOF :: Key -> HaskLineCmd InsertMode InsertMode
deleteCharOrEOF k = acceptKeyM k $ \s -> if s == emptyIM
                        then Nothing
                        else Just $ return (Change (deleteNext s), justDelete)
    where
        justDelete = try (change deleteNext k >|> justDelete)