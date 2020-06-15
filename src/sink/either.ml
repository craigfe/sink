open Import

type ('l, 'r) t = ('l, 'r) Sink_kernel.either = Left of 'l | Right of 'r
[@@deriving branded]
