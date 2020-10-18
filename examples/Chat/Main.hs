{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class       (liftIO)
import           Concur.Core.Types            (Widget)
import           Concur.Replica               (HTML, runDefault)
import           Concur.Replica.DOM           (button, input, li, text, ul)
import           Concur.Replica.DOM.Events    (BaseEvent (target), onClick, onInput, targetValue)
import           Concur.Replica.DOM.Props     (value)
import           Data.Text                    (Text)
import           Control.Concurrent.STM       (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import           Control.Concurrent.STM.TChan (TChan, dupTChan, newTChanIO, readTChan, writeTChan)

-- A simple chat app demonstrating communication between clients using STM.
-- The chatWidget is waiting for either the user to send a message or
-- another client to alert a new message. When a client sends a message
-- the chatWidget will update the messageHistory and then alert the other
-- clients by writing to messageAlert.

type Message = Text

data EditorAction
    = Typing Message -- The message the user is currently typing
    | Send Message -- The message the user sends to other users

data ChatAction
    = NewMessageToPost Message -- Message from one client to the others
    | NewMessagePosted


chatWidget :: TVar [Message] -> TChan () -> Widget HTML Message
chatWidget msgsTVar messageAlert = do
    messageHistory <- liftIO . atomically . readTVar $ msgsTVar
    -- Render messageList and messageEditor and wait for user to send
    let newMessageToPost = NewMessageToPost <$> (messageEditor "" <|> messagesList messageHistory)
    -- Wait for a message from other user
    let newMessagePosted = NewMessagePosted <$ (liftIO . atomically . readTChan $ messageAlert)
    -- Wait for whatever chatAction to happen first
    chatAction <- newMessageToPost <|> newMessagePosted
    case chatAction of
        NewMessagePosted -> chatWidget msgsTVar messageAlert
        NewMessageToPost newMessage -> do
            liftIO . atomically $ do
                -- Add this message to the messageHistory
                modifyTVar' msgsTVar (newMessage :)
                -- Notify all clients about the updates
                writeTChan messageAlert ()
            chatWidget msgsTVar messageAlert

messageEditor :: Message -> Widget HTML Message
messageEditor typing = do
    let textInput = Typing . targetValue . target <$> input [value $ typing, onInput]
    let submitButton = Send typing <$ button [onClick] [text "Send"]
    -- Wait for whatever editorAction to happen first
    editorAction <- textInput <|> submitButton
    case editorAction of
        Typing tm -> messageEditor tm
        Send msg -> pure msg

messagesList :: [Message] -> Widget HTML Message
messagesList messageHistory = ul [] $ messageItem <$> messageHistory

messageItem :: Message -> Widget HTML Message
messageItem message = li [] [text message]


main :: IO ()
main = do
    -- Channel to alert other clients that the messageHistory is updated
    messageAlert <- newTChanIO
    messageHistory <- newTVarIO []
    runDefault 8080 "Chat" $ \_ -> chatWidget messageHistory
        =<< (liftIO . atomically $ dupTChan messageAlert)
