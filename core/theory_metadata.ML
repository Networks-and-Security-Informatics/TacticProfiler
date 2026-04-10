(*  Title:      ml/utils/theory_metadata.ML
    Author:     davidebonav
    Project:    TacticProfiler

    Utilities for extracting metadata from theories and theorems:
    names, identifiers, subgoal counts, equality testing.
*)

signature THEORY_METADATA =
sig
  (* Theory metadata *)
  val get_theory_name      : Proof.context -> string
  val get_theory_identifier: Proof.context -> string   (* currently same as name *)

  (* Theorem metadata *)
  val get_theorem_content_hash : Proof.context -> thm -> string
  val get_theorem_identifier   : Proof.context -> thm -> string

  (* Subgoal information *)
  val subgoal_count        : thm -> int
  val format_subgoals_rich : Proof.context -> thm -> string   (* with markup *)
  val format_subgoals_plain: Proof.context -> thm -> string   (* plain text *)

  (* Equality test with alpha‑, beta‑, eta‑equivalence *)
  val are_equal_theorems   : Proof.context -> thm -> thm -> bool
end;

structure TheoryMetadata : THEORY_METADATA =
struct

  (* Helper: join a list of strings with a separator. *)
  fun join_strings_with (separator : string) (components : string list) : string =
    case components of
        []      => ""
      | [single] => single
      | first :: rest => first ^ separator ^ join_strings_with separator rest

  (* ---------- Theory metadata ---------- *)

  fun get_theory_name (context : Proof.context) : string =
    let val thy = Proof_Context.theory_of context
    in Context.theory_name {long = false} thy end

  fun get_theory_identifier (context : Proof.context) : string =
    get_theory_name context   (* currently same as short name *)

  (* ---------- Theorem metadata ---------- *)

  fun get_theorem_content_hash (context : Proof.context) (theorem : thm) : string =
    ContentHasher.sha1_hash_of_theorem context theorem

  fun get_theorem_identifier (context : Proof.context) (theorem : thm) : string =
    let
      val theory_name = get_theory_name context
      val content_hash = get_theorem_content_hash context theorem
    in
      ContentHasher.compute_digest ContentHasher.SHA1
        (join_strings_with "." [theory_name, content_hash])
    end

  (* ---------- Subgoal information ---------- *)

  fun subgoal_count theorem = Thm.nprems_of theorem

  fun format_subgoals_rich context theorem =
    Goal_Display.pretty_goals context theorem
      |> Pretty.chunks
      |> Pretty.string_of

  fun format_subgoals_plain context theorem =
    format_subgoals_rich context theorem
      |> YXML.parse_body
      |> XML.content_of

  (* ---------- Equality testing ---------- *)

  fun are_equal_theorems context theorem_a theorem_b =
    let
      (* Full propositions of the two theorems *)
      val prop_a = Thm.full_prop_of theorem_a
      val prop_b = Thm.full_prop_of theorem_b

      (* Import terms with consistent variable renaming *)
      val (imported_terms, _) = Variable.import_terms true [prop_a, prop_b] context
      val (term_a', term_b') =
        case imported_terms of
          [ta, tb] => (ta, tb)
        | _ => error "are_equal_theorems: unexpected number of terms"

      (* Beta-eta contract to remove surface differences *)
      val term_a'' = Envir.beta_eta_contract term_a'
      val term_b'' = Envir.beta_eta_contract term_b'
    in
      (* Alpha-equivalence (ignoring variable names, indices, and types) *)
      Term.aconv_untyped (term_a'', term_b'')
    end

end;