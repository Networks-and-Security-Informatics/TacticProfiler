(*  Title:      dynamic_metadata.ML
    Author:     davidebonav
    Project:    TacticProfiler

    A flexible key-value store where values are typed variants.
    Supports adding, retrieving, and removing entries with type safety,
    as well as various merging strategies for metadata maps.
*)

signature DYNAMIC_METADATA =
sig
  (* Supported value types for metadata entries *)
  datatype metadata_value =
      String of string
    | Int    of int
    | Bool   of bool
    | Real   of real
    | Ctxt   of Proof.context
    | Thm    of thm

  (* A mapping from string keys to metadata values *)
  type metadata_store = metadata_value Symtab.table

  (* ----- Basic operations ----- *)
  val empty        : metadata_store
  val add          : string -> metadata_value -> metadata_store -> metadata_store
  val lookup       : string -> metadata_store -> metadata_value option
  val extract      : string -> metadata_store -> (metadata_value option * metadata_store)

  (* ----- Typed addition ----- *)
  val add_string   : string -> string          -> metadata_store -> metadata_store
  val add_int      : string -> int             -> metadata_store -> metadata_store
  val add_bool     : string -> bool            -> metadata_store -> metadata_store
  val add_real     : string -> real            -> metadata_store -> metadata_store
  val add_context  : string -> Proof.context   -> metadata_store -> metadata_store
  val add_theorem  : string -> thm             -> metadata_store -> metadata_store

  (* ----- Typed lookup ----- *)
  val get_string   : string -> metadata_store -> string option
  val get_int      : string -> metadata_store -> int option
  val get_bool     : string -> metadata_store -> bool option
  val get_real     : string -> metadata_store -> real option
  val get_context  : string -> metadata_store -> Proof.context option
  val get_theorem  : string -> metadata_store -> thm option

  (* ----- Typed extraction (remove and return) ----- *)
  val extract_string : string -> metadata_store -> (string option * metadata_store)
  val extract_int    : string -> metadata_store -> (int option * metadata_store)
  val extract_bool   : string -> metadata_store -> (bool option * metadata_store)
  val extract_real   : string -> metadata_store -> (real option * metadata_store)
  val extract_context : string -> metadata_store -> (Proof.context option * metadata_store)
  val extract_theorem : string -> metadata_store -> (thm option * metadata_store)

  (* ----- Merging strategies ----- *)
  val union_strict   : metadata_store -> metadata_store -> metadata_store
      (* raises exception on duplicate keys *)
  val union_left     : metadata_store -> metadata_store -> metadata_store
      (* keep left entry on conflict *)
  val union_right    : metadata_store -> metadata_store -> metadata_store
      (* keep right entry on conflict (standard union) *)
  val merge_with     : (string -> metadata_value * metadata_value -> metadata_value)
                       -> metadata_store -> metadata_store -> metadata_store
      (* custom conflict resolution *)
end;

structure DynamicMetadata : DYNAMIC_METADATA =
struct

  datatype metadata_value =
      String of string
    | Int    of int
    | Bool   of bool
    | Real   of real
    | Ctxt   of Proof.context
    | Thm    of thm

  type metadata_store = metadata_value Symtab.table

  val empty = Symtab.empty

  (* ----- Basic operations ----- *)

  fun add key value store = Symtab.update (key, value) store

  fun lookup key store = Symtab.lookup store key

  fun extract key store =
    let val value = lookup key store
        val store' = Symtab.delete_safe key store
    in (value, store') end

  (* ----- Helpers for typed projections ----- *)

  fun project_value constructor extractor opt =
    case opt of
      SOME (constructor v) => SOME (extractor v)
    | _ => NONE

  (* ----- Typed addition ----- *)

  fun add_string key value = add key (String value)
  fun add_int    key value = add key (Int    value)
  fun add_bool   key value = add key (Bool   value)
  fun add_real   key value = add key (Real   value)
  fun add_context key value = add key (Ctxt   value)
  fun add_theorem key value = add key (Thm    value)

  (* ----- Typed lookup ----- *)

  fun get_string key store =
    lookup key store |> project_value String (fn x => x)
  fun get_int key store =
    lookup key store |> project_value Int (fn x => x)
  fun get_bool key store =
    lookup key store |> project_value Bool (fn x => x)
  fun get_real key store =
    lookup key store |> project_value Real (fn x => x)
  fun get_context key store =
    lookup key store |> project_value Ctxt (fn x => x)
  fun get_theorem key store =
    lookup key store |> project_value Thm (fn x => x)

  (* ----- Typed extraction ----- *)

  fun extract_generic project key store =
    let val (opt, store') = extract key store
    in (project opt, store') end

  fun extract_string key store = extract_generic (fn String s => SOME s | _ => NONE) key store
  fun extract_int    key store = extract_generic (fn Int i    => SOME i | _ => NONE) key store
  fun extract_bool   key store = extract_generic (fn Bool b   => SOME b | _ => NONE) key store
  fun extract_real   key store = extract_generic (fn Real r   => SOME r | _ => NONE) key store
  fun extract_context key store = extract_generic (fn Ctxt c   => SOME c | _ => NONE) key store
  fun extract_theorem key store = extract_generic (fn Thm t    => SOME t | _ => NONE) key store

  (* ----- Merging strategies ----- *)

  (* Strict union: raises exception if any key appears in both tables. *)
  fun union_strict left right = Symtab.merge (K false) (left, right)

  (* Left‑biased union: keep entry from left table on conflict. *)
  fun union_left left right = Symtab.merge (K true) (left, right)

  (* Right‑biased union: keep entry from right table on conflict.
     This is the typical union operation for dictionaries. *)
  fun union_right left right =
    let
      fun combine _ (_, right_val) = right_val
    in
      Symtab.join (fn _ => (fn (_, right_val) => right_val)) (left, right)
    end

  (* Custom merge with user‑provided conflict resolver.
     The resolver receives the key and the pair (value_from_left, value_from_right)
     and must return a single metadata_value. *)
  fun merge_with resolver left right =
    let
      fun combine key (left_val, right_val) = resolver key (left_val, right_val)
    in
      Symtab.join combine (left, right)
    end

end;

(* -------------------------------------------------------------------------
   Common metadata keys used across the TacticProfiler project.
   These are not part of the core signature but are provided for convenience.
   ------------------------------------------------------------------------- *)

structure StandardMetadataKeys =
struct
  (* Execution outcome *)
  val success_flag   = "success"
  val timestamp_us   = "timestamp_us"
  val elapsed_ms     = "elapsed_ms"
  val elapsed_us     = "elapsed_us"
  val cpu_time_ms    = "cpu_ms"
  val cpu_time_us    = "cpu_us"
  val error_message  = "error"

  (* Benchmarking statistics *)
  val iterations     = "iterations"
  val average_us     = "avg_us"
  val minimum_us     = "min_us"
  val maximum_us     = "max_us"
  val stddev_us      = "stddev_us"
  val total_us       = "total_us"

  (* Theory and proof context identification *)
  val theory_name    = "theory_name"
  val lemma_name     = "lemma_name"

  (* Tracer and tactic identification *)
  val tracer_name    = "tracer_name"
  val tactic_name    = "tactic_name"

  (* Change tracking *)
  val changed_flag   = "changed"
end;