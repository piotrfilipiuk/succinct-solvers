{-# OPTIONS_GHC -w #-}
module WhileParser where
import Prelude hiding (EQ)
import WhileLang
import WhileLexer

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

action_0 (7) = happyShift action_3
action_0 (9) = happyShift action_4
action_0 (12) = happyShift action_5
action_0 (29) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 _ = happyFail

action_1 (7) = happyShift action_3
action_1 (9) = happyShift action_4
action_1 (12) = happyShift action_5
action_1 (29) = happyShift action_6
action_1 (4) = happyGoto action_2
action_1 _ = happyFail

action_2 (24) = happyShift action_8
action_2 _ = happyFail

action_3 (16) = happyShift action_18
action_3 _ = happyFail

action_4 (7) = happyShift action_11
action_4 (8) = happyShift action_12
action_4 (17) = happyShift action_13
action_4 (18) = happyShift action_14
action_4 (21) = happyShift action_15
action_4 (22) = happyShift action_16
action_4 (5) = happyGoto action_17
action_4 (6) = happyGoto action_10
action_4 _ = happyFail

action_5 (7) = happyShift action_11
action_5 (8) = happyShift action_12
action_5 (17) = happyShift action_13
action_5 (18) = happyShift action_14
action_5 (21) = happyShift action_15
action_5 (22) = happyShift action_16
action_5 (5) = happyGoto action_9
action_5 (6) = happyGoto action_10
action_5 _ = happyFail

action_6 _ = happyReduce_2

action_7 (24) = happyShift action_8
action_7 (30) = happyAccept
action_7 _ = happyFail

action_8 (7) = happyShift action_3
action_8 (9) = happyShift action_4
action_8 (12) = happyShift action_5
action_8 (29) = happyShift action_6
action_8 (4) = happyGoto action_32
action_8 _ = happyFail

action_9 (14) = happyShift action_31
action_9 _ = happyFail

action_10 (19) = happyShift action_25
action_10 (20) = happyShift action_26
action_10 (25) = happyShift action_27
action_10 (26) = happyShift action_28
action_10 (27) = happyShift action_29
action_10 (28) = happyShift action_30
action_10 _ = happyFail

action_11 _ = happyReduce_13

action_12 _ = happyReduce_14

action_13 _ = happyReduce_10

action_14 _ = happyReduce_11

action_15 (7) = happyShift action_11
action_15 (8) = happyShift action_12
action_15 (17) = happyShift action_13
action_15 (18) = happyShift action_14
action_15 (21) = happyShift action_15
action_15 (22) = happyShift action_16
action_15 (5) = happyGoto action_24
action_15 (6) = happyGoto action_10
action_15 _ = happyFail

action_16 (7) = happyShift action_11
action_16 (8) = happyShift action_12
action_16 (17) = happyShift action_13
action_16 (18) = happyShift action_14
action_16 (21) = happyShift action_15
action_16 (22) = happyShift action_16
action_16 (5) = happyGoto action_22
action_16 (6) = happyGoto action_23
action_16 _ = happyFail

action_17 (10) = happyShift action_21
action_17 _ = happyFail

action_18 (7) = happyShift action_11
action_18 (8) = happyShift action_12
action_18 (22) = happyShift action_20
action_18 (6) = happyGoto action_19
action_18 _ = happyFail

action_19 (25) = happyShift action_27
action_19 (26) = happyShift action_28
action_19 (27) = happyShift action_29
action_19 (28) = happyShift action_30
action_19 _ = happyReduce_3

action_20 (7) = happyShift action_11
action_20 (8) = happyShift action_12
action_20 (22) = happyShift action_20
action_20 (6) = happyGoto action_43
action_20 _ = happyFail

action_21 (7) = happyShift action_3
action_21 (9) = happyShift action_4
action_21 (12) = happyShift action_5
action_21 (29) = happyShift action_6
action_21 (4) = happyGoto action_42
action_21 _ = happyFail

action_22 (23) = happyShift action_41
action_22 _ = happyFail

action_23 (19) = happyShift action_25
action_23 (20) = happyShift action_26
action_23 (23) = happyShift action_40
action_23 (25) = happyShift action_27
action_23 (26) = happyShift action_28
action_23 (27) = happyShift action_29
action_23 (28) = happyShift action_30
action_23 _ = happyFail

action_24 _ = happyReduce_7

action_25 (7) = happyShift action_11
action_25 (8) = happyShift action_12
action_25 (22) = happyShift action_20
action_25 (6) = happyGoto action_39
action_25 _ = happyFail

action_26 (7) = happyShift action_11
action_26 (8) = happyShift action_12
action_26 (22) = happyShift action_20
action_26 (6) = happyGoto action_38
action_26 _ = happyFail

action_27 (7) = happyShift action_11
action_27 (8) = happyShift action_12
action_27 (22) = happyShift action_20
action_27 (6) = happyGoto action_37
action_27 _ = happyFail

action_28 (7) = happyShift action_11
action_28 (8) = happyShift action_12
action_28 (22) = happyShift action_20
action_28 (6) = happyGoto action_36
action_28 _ = happyFail

action_29 (7) = happyShift action_11
action_29 (8) = happyShift action_12
action_29 (22) = happyShift action_20
action_29 (6) = happyGoto action_35
action_29 _ = happyFail

action_30 (7) = happyShift action_11
action_30 (8) = happyShift action_12
action_30 (22) = happyShift action_20
action_30 (6) = happyGoto action_34
action_30 _ = happyFail

action_31 (7) = happyShift action_3
action_31 (9) = happyShift action_4
action_31 (12) = happyShift action_5
action_31 (29) = happyShift action_6
action_31 (4) = happyGoto action_33
action_31 _ = happyFail

action_32 (24) = happyShift action_8
action_32 _ = happyReduce_1

action_33 (15) = happyShift action_45
action_33 (24) = happyShift action_8
action_33 _ = happyFail

action_34 _ = happyReduce_18

action_35 _ = happyReduce_17

action_36 (27) = happyShift action_29
action_36 (28) = happyShift action_30
action_36 _ = happyReduce_16

action_37 (27) = happyShift action_29
action_37 (28) = happyShift action_30
action_37 _ = happyReduce_15

action_38 (25) = happyShift action_27
action_38 (26) = happyShift action_28
action_38 (27) = happyShift action_29
action_38 (28) = happyShift action_30
action_38 _ = happyReduce_9

action_39 (25) = happyShift action_27
action_39 (26) = happyShift action_28
action_39 (27) = happyShift action_29
action_39 (28) = happyShift action_30
action_39 _ = happyReduce_8

action_40 _ = happyReduce_12

action_41 _ = happyReduce_6

action_42 (11) = happyShift action_44
action_42 (24) = happyShift action_8
action_42 _ = happyFail

action_43 (23) = happyShift action_40
action_43 (25) = happyShift action_27
action_43 (26) = happyShift action_28
action_43 (27) = happyShift action_29
action_43 (28) = happyShift action_30
action_43 _ = happyFail

action_44 _ = happyReduce_5

action_45 (7) = happyShift action_3
action_45 (9) = happyShift action_4
action_45 (12) = happyShift action_5
action_45 (29) = happyShift action_6
action_45 (4) = happyGoto action_46
action_45 _ = happyFail

action_46 (13) = happyShift action_47
action_46 (24) = happyShift action_8
action_46 _ = happyFail

action_47 _ = happyReduce_4

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Semi happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (Skip
	)

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn4
		 (Asgn happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 7 4 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 5 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Not happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Neq happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn5
		 (B True
	)

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn5
		 (B False
	)

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn6
		 (V happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  6 happyReduction_14
happyReduction_14 (HappyTerminal (NUM happy_var_1))
	 =  HappyAbsSyn6
		 (I happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Add happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  6 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  6 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Div happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 30 30 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	ID happy_dollar_dollar -> cont 7;
	NUM happy_dollar_dollar -> cont 8;
	WHILE -> cont 9;
	DO -> cont 10;
	OD -> cont 11;
	IF -> cont 12;
	FI -> cont 13;
	THEN -> cont 14;
	ELSE -> cont 15;
	ASGN -> cont 16;
	TRUE -> cont 17;
	FALSE -> cont 18;
	EQ -> cont 19;
	NEQ -> cont 20;
	NOT -> cont 21;
	LPAREN -> cont 22;
	RPAREN -> cont 23;
	SEMI -> cont 24;
	PLUS -> cont 25;
	MINUS -> cont 26;
	MUL -> cont 27;
	DIV -> cont 28;
	SKIP -> cont 29;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

program tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 311 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
