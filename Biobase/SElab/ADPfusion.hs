
-- | Provides streaming facilities for ADPfusion. These have some additional
-- machinery embedded via newtypes.
--
-- TODO need a type @(State :. Subword)@ for cmsearch-like behaviour -- how
-- about @(Z:.State:.Subword)@ ?
--
-- TODO need a type @(Z:.State:.State)@ for CMcompare-like behaviour

module Biobase.SElab.ADPfusion where

