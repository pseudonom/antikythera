{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Arrow ((&&&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Maybe
import GHC.Natural
import Protolude

newtype Numeraire = MkNumeraire
  { runNumeraire :: Rational
  } deriving (Eq, Fractional, Hashable, Num, Ord, Show)

type Valuation info decision = info -> decision -> Numeraire

type UtilityFunction individual info decision = DecisionRule individual info decision -> TransferFunction individual info -> Infos individual info -> info -> individual -> Numeraire

type Infos individual info = individual -> info

type DecisionRule individual info decision = Infos individual info -> decision

type TransferFunction individual info = Infos individual info -> (individual -> Numeraire)

type SocialChoiceFunction individual info decision = (DecisionRule individual info decision, TransferFunction individual info)

efficient :: DecisionRule individuals info decision -> Bool
efficient decisionRule = undefined

feasible :: TransferFunction individual info -> Bool
feasible transferFunction = undefined

balanced :: TransferFunction individual info -> Bool
balanced transferFunction = undefined

dominant :: info -> Bool
dominant info = undefined

dictatorial :: DecisionRule individual info decision -> Bool
dictatorial decisionRule = undefined

quasilinear :: Valuation info decision
            -> UtilityFunction individual info decision
quasilinear valuation decisionRule transferFunction announcedInfos trueInfo individual =
  valuation trueInfo (decisionRule announcedInfos) +
  transferFunction announcedInfos individual

grovesTransferFunction
  :: (Eq individual, Hashable individual)
  => HashMap individual (Valuation info decision)
  -> DecisionRule individual info decision
  -> (HashMap individual (Valuation info decision) -> TransferFunction individual info)
  -> TransferFunction individual info
grovesTransferFunction valuations decisionRule transferFunction infos individual =
  transferFunction (individual `HashMap.delete` valuations) infos individual +
  basePayments HashMap.! individual
  where
    basePayments =
      HashMap.mapWithKey
        (\i _ ->
           evaluateDecision
             (HashMap.elems $ i `HashMap.delete` valuations')
             decision)
        valuations'
    valuations' =
      HashMap.mapWithKey
        (\individual' valuation -> valuation (infos individual'))
        valuations
    decision = decisionRule infos

clarkePivot
  :: (Eq decision, Hashable decision)
  => HashSet decision
  -> HashMap individual (Valuation info decision)
  -> TransferFunction individual info
clarkePivot decisions valuations infos _ = -socialUtility
  where
    (_, socialUtility) = efficientDecisionRule' decisions valuations infos

clarkeMechanism
  :: (Eq decision, Hashable decision, Eq individual, Hashable individual)
  => HashSet decision
  -> HashMap individual (Valuation info decision)
  -> SocialChoiceFunction individual info decision
clarkeMechanism decisions valuations =
  ( decisionRule
  , grovesTransferFunction valuations decisionRule (clarkePivot decisions))
  where
    decisionRule = efficientDecisionRule decisions valuations

runSocialChoiceFunction
  :: SocialChoiceFunction individual info decision
  -> Infos individual info
  -> (decision, individual -> Numeraire)
runSocialChoiceFunction (decisionRule, transferFunction) infos =
  (decisionRule infos, transferFunction infos)

evaluateAtEach :: (a -> b) -> HashSet a -> HashMap a b
evaluateAtEach f set = HashMap.mapWithKey (\key _ -> f key) $ HashSet.toMap set

publicProject :: Natural
              -> Numeraire
              -> SocialChoiceFunction Natural Numeraire Bool
publicProject numParticipants cost =
  clarkeMechanism (HashSet.fromList [True, False]) valuations
  where
    valuations =
      HashMap.fromList $ (, valuation) <$> enumFromTo 1 numParticipants
    valuation info decision =
      if decision
        then info - cost / fromIntegral (HashMap.size valuations)
        else MkNumeraire 0

runPublicProject :: Numeraire
                 -> [Numeraire]
                 -> (Bool, HashMap Natural Numeraire)
runPublicProject cost infos' =
  second (`evaluateAtEach` keySet infos) $
  runSocialChoiceFunction
    (publicProject (fromIntegral $ HashMap.size infos) cost)
    (infos HashMap.!)
  where
    infos =
      HashMap.fromList $
      zip (enumFromTo 1 (fromIntegral $ length infos')) infos'

newtype Project = MkProject
  { runProject :: Text
  } deriving (Eq, Hashable, IsString, Show)

newtype Employee = MkEmployee
  { runEmployee :: Text
  } deriving (Eq, Hashable, IsString, Show)

plsPlanning
  :: HashSet (HashSet Project)
  -> HashSet Employee
  -> SocialChoiceFunction Employee (HashSet Project -> Numeraire) (HashSet Project)
plsPlanning projects employees = clarkeMechanism projects valuations
  where
    valuations = HashMap.map (const valuation) $ HashSet.toMap employees
    valuation info decision = info decision

runPlsPlanning
  :: HashMap Employee (HashMap (HashSet Project) Numeraire)
  -> (HashSet Project, HashMap Employee Numeraire)
runPlsPlanning infos' =
  second (`evaluateAtEach` employees) $
  runSocialChoiceFunction (plsPlanning projects employees) infos
  where
    projects = keySet . fromJust . head $ toList infos'
    infos employee projects' = infos' HashMap.! employee HashMap.! projects'
    employees = keySet infos'

preferences :: HashMap Employee (HashMap (HashSet Project) Numeraire)
preferences =
  [ ( "jerry"
    , [ (["auth", "denorm", "proxy"], 70)
      , (["auth", "denorm", "entitlements"], 40)
      , (["auth", "denorm"], 40)
      , (["auth", "proxy"], 40)
      , (["auth", "entitlements"], 10)
      , (["denorm", "proxy"], 50)
      , (["denorm", "entitlements"], 20)
      , (["auth"], 10)
      , (["denorm"], 20)
      , (["proxy"], 30)
      , (["entitlements"], 0)
      ])
  , ( "marc"
    , [ (["auth", "denorm", "proxy"], 0)
      , (["auth", "denorm", "entitlements"], 80)
      , (["auth", "denorm"], 0)
      , (["auth", "proxy"], 0)
      , (["auth", "entitlements"], 80)
      , (["denorm", "proxy"], 0)
      , (["denorm", "entitlements"], 80)
      , (["auth"], 0)
      , (["denorm"], 0)
      , (["proxy"], 0)
      , (["entitlements"], 80)
      ])
  , ( "eric"
    , [ (["auth", "denorm", "proxy"], -40)
      , (["auth", "denorm", "entitlements"], -40)
      , (["auth", "denorm"], -40)
      , (["auth", "proxy"], -40)
      , (["auth", "entitlements"], -40)
      , (["denorm", "proxy"], 0)
      , (["denorm", "entitlements"], 0)
      , (["auth"], -40)
      , (["denorm"], 0)
      , (["proxy"], 0)
      , (["entitlements"], 0)
      ])
  ]

keySet :: HashMap k v -> HashSet k
keySet = HashSet.fromMap . HashMap.map (const ())

efficientDecisionRule
  :: (Eq decision, Hashable decision)
  => HashSet decision
  -> HashMap individual (Valuation info decision)
  -> DecisionRule individual info decision
efficientDecisionRule decisions valuations infos =
  fst $ efficientDecisionRule' decisions valuations infos

efficientDecisionRule'
  :: (Eq decision, Hashable decision)
  => HashSet decision
  -> HashMap individual (Valuation info decision)
  -> Infos individual info
  -> (decision, Numeraire)
efficientDecisionRule' decisions valuations infos =
  maximumBy (compare `on` snd) $
  HashSet.map
    (identity &&& evaluateDecision (HashMap.elems valuations'))
    decisions
  where
    valuations' =
      HashMap.mapWithKey
        (\individual valuation -> valuation (infos individual))
        valuations

evaluateDecision :: [decision -> Numeraire] -> decision -> Numeraire
evaluateDecision valuations decision = sum $ ($ decision) <$> valuations
