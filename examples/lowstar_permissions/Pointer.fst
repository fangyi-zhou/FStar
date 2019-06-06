module Pointer


open LowStar.Resource
open LowStar.RST
open LowStar.Permissions.Pointer

#reset-options "--z3rlimit 20 --max_fuel 0 --max_ifuel 0 --z3cliopt smt.qi.eager_threshold=100"

let read_write_without_sharing () : RST unit
  (empty_resource)
  (fun _ -> empty_resource)
  (fun _ -> True)
  (fun _ _ _ -> True)
  =
  let ptr = ptr_alloc 42ul in
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  let x1 = ptr_read ptr in
  ptr_write ptr FStar.UInt32.(x1 +%^ 1ul);
  ptr_free ptr;
  ()