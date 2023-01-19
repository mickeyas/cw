module Main (main) where

-- | Libraries to do with 'threading and concurrency' and 'random' values.
import Lib
import Control.Concurrent
import System.Random
import Data.List

type UserName = String
type Messages = String
type MsgCount = Integer

--type AllMessage = [Message]

--AllMessage :: [(String, String)]

--data [] a = [] | a : [a]

-- | The user type class is defined so that each user has a username.
data User = User {
      username :: UserName
   } deriving (Show)

-- | The 'Message' type class to store messages that the users will send to each other.
data Message = Message {
      message :: Messages
} deriving (Show)

--type AllMessage [(sender, receiver, msg)] = AllMessage {sender :: UserName, receiver :: UserName, msg :: Messages}
--allMessage :: (UserName, UserName, Messages)
--allMessage = [(User, Message)]

--allMessage = AllMessage { [(sender, receiver, msg)] }

-- | Initialising 10 users.
tom = User {username =      "Tom    "}
jake = User {username =     "Jake   "}
sully = User {username =    "Sully  "}
tony = User {username =     "Tony   "}
mike = User {username =     "Mike   "}
kelly = User {username =    "Kelly  "}
rachael = User {username =  "Rachael"}
alice = User {username =    "Alice  "}
fred = User {username =     "Fred   "}
anthony = User {username =  "Anthony"}

-- | Initialising 10 random messages
a = Message {message = "Greetings!"}
b = Message {message = "Pub?"}
c = Message {message = "Pub first, then cinema?"}
d = Message {message = "I'm staying in tonight"}
e = Message {message = "Don't Stay in tonight"}
f = Message {message = "Okay"}
g = Message {message = "Sorry, I have things to do right now. Can we talk later?"}
h = Message {message = "Let's get a bite to eat"}
i = Message {message = "I am in a hurry, let me know what is happening"}
j = Message {message = "Today will be fun!"}

{- | In this function Int values are being mapped to the users in preparation for selection. The use of 'mod' funtion
     is to ensure there are only whole numbers between and including 0 and 9 can be selected. -}
mapIntToPerson :: Int -> User
mapIntToPerson n = case r of
      0 -> tom
      1 -> jake
      2 -> sully
      3 -> tony
      4 -> mike
      5 -> kelly
      6 -> rachael
      7 -> alice
      8 -> fred
      9 -> anthony
    where r = mod n 10 


-- | In this function Int values are being mapped to the messages in preparation for selection.
mapIntToMessage :: Int -> Message
mapIntToMessage n = case r of
      0 -> a
      1 -> b
      2 -> c
      3 -> d
      4 -> e
      5 -> f
      6 -> g
      7 -> h
      8 -> i
      9 -> j
    where r = mod n 10

-- | Int values are mapped to greater integers, this is because the greater values can represent time in milliseconds.
mapIntToRandInterval :: Int -> Int
mapIntToRandInterval n = case r of
      0 -> 500000
      1 -> 1000000
      2 -> 2000000
      3 -> 3000000
      4 -> 4000000
      5 -> 5000000
    where r = mod n 6

{- The function randUser returns the value of the user mapped to the randomly selected integer value. In this function
   we see the use of randomIO to generate the random integer. -}
randUsr :: IO User
randUsr = do
    n <- randomIO :: IO Int
    let person = mapIntToPerson n
    return person

-- | Here we are able to return a message selected at random as the 'mapIntToMessage' function is called here.
randMessage :: IO Message
randMessage = do
    n <- randomIO :: IO Int
    let messages = mapIntToMessage n
    return messages

-- | As in the afore mentioned function, the randInterval returns an interval mapped to the randomly generated integer value. 
randInterval :: IO Int
randInterval = do
    n <- randomIO :: IO Int
    let interval = mapIntToRandInterval n
    return interval

{- | The sndMessage function takes in as parameter; the username of the sender, and MVar and an empty MVar. As well as a 
     randomly selected user, random message and the sender of the user, the threadId is stored as a variable within this method.
     'putStrLn' is used to print the threadId, the sender, the receiver and the message to screen. A method putVar is used to
     take the values of the sender, receiver and the message and put them in the thread named 'chats'. A delay is called after
     which the sendMessage funtion is called again with the same parameters. 
    -}
sndMessage :: User -> MVar () -> MVar (User, User, Message) -> IO ()
sndMessage user io chats = do
    threadId <- myThreadId
    ru <- randUsr
    msg <- randMessage
    interval <- randInterval
    threadDelay interval
    putStrLn $ show threadId ++ " From: " ++ show user ++ " To: " ++ show(ru) ++ " " ++ show(msg)

    --putMVar chats (user, ru, msg)
    
    --x <- takeMVar chats

    --putStrLn $ show x
    sndMessage user io chats

{- This is the main function. Here, newEmptyMVar are declared for each user and the 'forkIO' function is used to create new threads.
   Within each new empty MVar, the new threads are used to run the sndMessage function to simulate a social networking. -}
main :: IO ()
main = do
    io <- newMVar ()
    person <- randUsr
    chats <- newEmptyMVar
    --allMessage :: AllMessage

    threadTom <- newEmptyMVar
    threadJake <- newEmptyMVar
    threadSully <- newEmptyMVar
    threadTony <- newEmptyMVar
    threadMike <- newEmptyMVar
    threadKelly <- newEmptyMVar
    threadRachael <- newEmptyMVar
    threadAlice <- newEmptyMVar
    threadFred <- newEmptyMVar
    threadAnthony <- newEmptyMVar

    threadTom <- forkIO (sndMessage tom io chats)
    threadJake <- forkIO (sndMessage jake io chats)
    threadSully <- forkIO (sndMessage sully io chats)
    threadTony <- forkIO (sndMessage tony io chats)
    threadMike <- forkIO (sndMessage mike io chats)
    threadKelly <- forkIO (sndMessage kelly io chats)
    threadRachael <- forkIO (sndMessage rachael io chats)
    threadAlice <- forkIO (sndMessage alice io chats)
    threadFred <- forkIO (sndMessage fred io chats)
    threadAnthony <- forkIO (sndMessage anthony io chats)

    threadDelay 20000000 --I need this until the messages can be counted

    putStrLn $ "<<<<<<<<--------Last Line-------->>>>>>>>"
    