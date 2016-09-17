module Homework_2 where
import Log

-- Exercise 1 solution

parseMessage :: String -> LogMessage
parseMessage msg = case (msgTypeString) of
						"I" -> LogMessage Info infoWarningTimeStamp infoWarningMsg
						"W" -> LogMessage Warning infoWarningTimeStamp infoWarningMsg
						"E" -> LogMessage (Error errorSeverity) errorTimeStamp errorWarningMsg
						_   -> Unknown msg
 where
   computeTimeStamp remInfo = (read . unwords . take 1) remInfo
   computeMsg remInfo = (unwords . drop 1) remInfo
   
   msgTypeString:remInfoWarnMsgInfo = words msg
   
   infoWarningTimeStamp = computeTimeStamp remInfoWarnMsgInfo
   infoWarningMsg = computeMsg remInfoWarnMsgInfo
   
   errorSeverity = infoWarningTimeStamp
   remErrorInfo = drop 1 remInfoWarnMsgInfo
   errorTimeStamp = computeTimeStamp remErrorInfo
   errorWarningMsg = computeMsg remErrorInfo
   
parse :: String -> [LogMessage]
parse logFile = map parseMessage (lines logFile)

-- Exercise 2 solution

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ timeStamp _) (Node left rt@(LogMessage _ rtTimeStamp _) right)
	| timeStamp < rtTimeStamp = Node (insert lm left) rt right
	| otherwise = Node left rt (insert lm right)
	
-- Exercise 3 solution

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- Exercise 4 solution

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left rt right) = (inOrder left) ++ [rt] ++ (inOrder right)

-- Exercise 5 solution

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map extractMessage (filter isErrorLogMessage sortedMessages)
  where
	isErrorLogMessage (LogMessage (Error severity) _ _) = severity >= 50
	isErrorLogMessage _ = False

	extractMessage (Unknown msg) = msg
	extractMessage (LogMessage _ _ msg) = msg

	sortedMessages = inOrder (build msgs)