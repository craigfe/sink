open Import

type 'a t = 'a lazy_t
[@@implements Functor.S, Monad.S, Typeable.S, Higher.BRANDED]

external force : 'a t -> 'a = "%lazy_force"

val force_val : 'a t -> 'a

val from_fun : (unit -> 'a) -> 'a t

val is_val : _ t -> bool
