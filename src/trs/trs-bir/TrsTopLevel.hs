-- | Which top-level modules trs can execute.
--
-- The compiler's own rule (SimExpand.simCheckBluesimTop) is a C++
-- model's: it can drive neither an always_enabled interface method nor
-- a top-level argument, because nothing in the generated model supplies
-- either.  trs supplies both, so its rule is different, and it lives
-- here rather than as a branch inside the compiler.
module TrsTopLevel(checkTrsTop) where

import Control.Monad(when, msum)
import Data.List(intercalate)

import ABin(ABinModInfo(..))
import ASyntax
import Error(ErrorHandle, ErrMsg(..), bsError)
import Id(getIdBaseString)
import Position(noPosition)
import Pragma(PProp(..), isAlwaysEn, isEnWhenRdy)
import PreIds(idDefaultClock, idDefaultReset)
import VModInfo(isParam, isPort)

-- | Refuse a top-level module trs has no way to run.
--
-- Batch mode fires an always_enabled method on every cycle of its
-- clock, at its schedule position, with EN held true, so that pragma is
-- accepted.  enabled_when_ready is not: nothing computes a runtime EN
-- to follow the RDY.
--
-- Bit-typed arguments and parameters are accepted because the link and
-- run steps bind them to constants (+NAME=value); any other type is
-- refused.  A module with bindable arguments must not also take input
-- clocks or resets beyond the defaults -- a binding is a constant, not
-- a waveform, so those are refused rather than left silently never
-- ticking.
checkTrsTop :: ErrorHandle -> ABinModInfo -> IO ()
checkTrsTop errh topModInfo = do
    when (hasEnWhenRdyMethod topModInfo) $
        bsError errh [(noPosition, EBSimEnablePragma)]

    let (bad_args, bad_params) = getNonBitArgsAndParams topModInfo
    when ((not (null bad_args)) || (not (null bad_params))) $
        bsError errh
            [(noPosition,
              EBSimTopLevelArgOrParam False (bad_args ++ bad_params))]

    let (top_args, top_params) = getArgsAndParams topModInfo
        extra_ins = getExtraClockAndResetInputs topModInfo
    when ((not (null (top_args ++ top_params))) && (not (null extra_ins))) $
        bsError errh
            [(noPosition,
              EGeneric ("The trs backend binds top-level module " ++
                        "arguments and parameters to constants; it does " ++
                        "not support additional input clocks or resets on " ++
                        "such a top-level module: " ++
                        intercalate ", " extra_ins))]

-- | An action method carrying enabled_when_ready and not always_enabled.
hasEnWhenRdyMethod :: ABinModInfo -> Bool
hasEnWhenRdyMethod modInfo =
    let
        pps = abmi_pps modInfo
        apkg = abmi_apkg modInfo
        ifcs = apkg_interface apkg

        getActionIfcs (AIAction { aif_name = i }) = [i]
        getActionIfcs (AIActionValue { aif_name = i }) = [i]
        getActionIfcs _ = []
        action_ifcs = concatMap getActionIfcs ifcs

        isEnPragma (PPalwaysEnabled {}) = True
        isEnPragma (PPenabledWhenReady {}) = True
        isEnPragma _ = False
        en_pps = filter isEnPragma pps

        isEwrOnly i = (isEnWhenRdy en_pps i) && (not (isAlwaysEn en_pps i))
    in
        (not (null en_pps)) && (any isEwrOnly action_ifcs)

-- | Every top-level argument and parameter, by name.
getArgsAndParams :: ABinModInfo -> ([String],[String])
getArgsAndParams modInfo =
    let inputs = getAPackageInputs (abmi_apkg modInfo)
        params = filter (isParam . snd) inputs
        ports  = filter (isPort . snd) inputs
        params' = [ getIdBaseString i | (AAI_Port (i,_),_) <- params ]
        ports'  = [ getIdBaseString i | (AAI_Port (i,_),_) <- ports ]
    in (ports',params')

-- | Those of them a constant binding cannot supply: a binding carries
-- bits, so anything not Bit#(n) with n >= 1 is out of reach.
getNonBitArgsAndParams :: ABinModInfo -> ([String],[String])
getNonBitArgsAndParams modInfo =
    let inputs = getAPackageInputs (abmi_apkg modInfo)
        isBindable (AAI_Port (_, ATBit n)) = n >= 1
        isBindable _ = False
        params = filter (isParam . snd) inputs
        ports  = filter (isPort . snd) inputs
        params' = [ getIdBaseString i
                  | (p@(AAI_Port (i,_)),_) <- params, not (isBindable p) ]
        ports'  = [ getIdBaseString i
                  | (p@(AAI_Port (i,_)),_) <- ports, not (isBindable p) ]
    in (ports',params')

-- | Input clocks and resets beyond the module's own defaults.
getExtraClockAndResetInputs :: ABinModInfo -> [String]
getExtraClockAndResetInputs modInfo =
    let pps = abmi_pps modInfo
        def_clk = case msum $ [ lookup idDefaultClock xs
                              | (PPclock_osc xs) <- pps ] ++ [Just "CLK"] of
                    Just s -> s
                    Nothing -> "CLK"
        -- the fallback is the port convention (RST_N), unlike
        -- ssys_default_rst's legacy "RSTN" string, which never
        -- matches a real port name
        def_rst = case msum $ [ lookup idDefaultReset xs
                              | (PPreset_port xs) <- pps ] ++ [Just "RST_N"] of
                    Just s -> s
                    Nothing -> "RST_N"
        inputs = map fst (getAPackageInputs (abmi_apkg modInfo))
        clks = [ getIdBaseString osc
               | (AAI_Clock osc _) <- inputs
               , getIdBaseString osc /= def_clk ]
        rsts = [ getIdBaseString r
               | (AAI_Reset r) <- inputs
               , getIdBaseString r /= def_rst ]
    in clks ++ rsts
