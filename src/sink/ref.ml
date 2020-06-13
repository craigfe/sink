open Import

type 'a t = 'a ref [@@deriving branded]
