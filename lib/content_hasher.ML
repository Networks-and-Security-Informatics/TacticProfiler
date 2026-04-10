(*  Title:      ml/utils/content_hasher.ML
    Author:     davidebonav
    Project:    TacticProfiler

    Hashing utilities for strings and theorem contents.
    Currently supports SHA-1 digest computation.
*)

signature CONTENT_HASHER =
sig
  (* Supported hash algorithms *)
  datatype hashing_algorithm = SHA1 | PLACEHOLDER

  (* Compute the digest of a string using the given algorithm. *)
  val compute_digest : hashing_algorithm -> string -> string

  (* Compute the hash of a theorem's full proposition (as a string). *)
  val hash_of_theorem : hashing_algorithm -> Proof.context -> thm -> string

  (* Convenience: SHA-1 hash of a theorem. *)
  val sha1_hash_of_theorem : Proof.context -> thm -> string
end;

structure ContentHasher : CONTENT_HASHER =
struct

  datatype hashing_algorithm = SHA1 | PLACEHOLDER

  (* String digest using the specified algorithm. *)
  fun compute_digest (SHA1 : hashing_algorithm) (input : string) : string =
        SHA1.rep (SHA1.digest input)
    | compute_digest (PLACEHOLDER : hashing_algorithm) _ : string =
        "PLACEHOLDER_HASH"

  (* Convert a theorem to a canonical string representation:
     the full proposition printed without special modes. *)
  fun theorem_to_string (context : Proof.context) (theorem : thm) : string =
    Print_Mode.setmp [] (fn () =>
      Syntax.string_of_term context (Thm.full_prop_of theorem)) ()

  (* Compute the hash of a theorem using the given algorithm. *)
  fun hash_of_theorem algorithm context theorem =
    compute_digest algorithm (theorem_to_string context theorem)

  (* SHA-1 hash of a theorem (most common use). *)
  fun sha1_hash_of_theorem context theorem =
    hash_of_theorem SHA1 context theorem

end;