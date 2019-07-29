type uint64 = Stdint.Uint64.t
open Stdint.Uint64

type t = uint64
type t' = t

let n = Prims.of_int 64
let v (x:t) : Prims.int = Prims.parse_int (to_string x)

let zero = zero
let one  = one
let ones = pred zero

(* Reexport add, plus aliases *)
let add           = add
let add_underspec = add
let add_mod       = add

(* Reexport sub, plus aliases *)
let sub           = sub
let sub_underspec = sub
let sub_mod       = sub

(* Reexport mul, plus aliases *)
let mul           = mul
let mul_underspec = mul
let mul_mod       = mul

(* Just reexport these *)
let div           = div
let rem           = rem
let logand        = logand
let logxor        = logxor
let logor         = logor
let lognot        = lognot
let to_string     = to_string
let to_string_hex = to_string_hex
let of_string     = of_string
let to_int        = to_int
let uint_to_t u   = of_string (Z.to_string u)
let __uint_to_t   = uint_to_t
let shift_right   = shift_right
let shift_left    = shift_left

(* Leverage Uint128 for this one *)
let mul_div a b =
  let a = Stdint.Uint128.of_uint64 a in
  let b = Stdint.Uint128.of_uint64 b in
  let r = Stdint.Uint128.mul a b in
  let r = Stdint.Uint128.shift_right r 64 in
  of_uint128 r

(* Comparison operators *)
let eq  (a:t) (b:t) : bool = a = b
let gt  (a:t) (b:t) : bool = a > b
let gte (a:t) (b:t) : bool = a >= b
let lt  (a:t) (b:t) : bool = a < b
let lte (a:t) (b:t) : bool = a <= b

(* NOT Constant time operators *)
let eq_mask  (a:t) (b:t) : t = if a  = b then ones else zero
let gte_mask (a:t) (b:t) : t = if a >= b then ones else zero

(* Infix notations *)
let op_Plus_Hat                 = add
let op_Plus_Question_Hat        = add_underspec
let op_Plus_Percent_Hat         = add_mod
let op_Subtraction_Hat          = sub
let op_Subtraction_Question_Hat = sub_underspec
let op_Subtraction_Percent_Hat  = sub_mod
let op_Star_Hat                 = mul
let op_Star_Question_Hat        = mul_underspec
let op_Star_Percent_Hat         = mul_mod
let op_Slash_Hat                = div
let op_Percent_Hat              = rem
let op_Hat_Hat                  = logxor
let op_Amp_Hat                  = logand
let op_Bar_Hat                  = logor
let op_Less_Less_Hat            = shift_left
let op_Greater_Greater_Hat      = shift_right
let op_Equals_Hat               = eq
let op_Greater_Hat              = gt
let op_Greater_Equals_Hat       = gte
let op_Less_Hat                 = lt
let op_Less_Equals_Hat          = lte
