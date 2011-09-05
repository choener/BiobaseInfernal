
-- | Transforms the internal representation of a CM back into a version that
-- can be used by Infernal.
--
-- Note that models are transformed into 'ByteString' as-is, the exporter does
-- not make sure that probabilities add to one, that we write out probabilities
-- instead of scores, and so on.
--
-- TODO some of the notes above will become less problematic once we use
-- newtypes, as a probability-CM will not be accepted by the exporter by then.

module Biobase.Infernal.Export where
