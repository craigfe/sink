open Import

type 'a t = 'a array
[@@implements Semigroup.S, Foldable.S, Typeable.S, Higher.BRANDED]
