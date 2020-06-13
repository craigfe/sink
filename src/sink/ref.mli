open Import

type 'a t = 'a ref [@@implements Higher.BRANDED]
