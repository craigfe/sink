open! Import

type 'a t = 'a ref [@@implements Branded.S]
