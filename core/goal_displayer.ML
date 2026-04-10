(*  Title:      ml/utils/goal_displayer.ML
    Author:     davidebonav
    Project:    TacticProfiler

    Utilities for displaying theorem goals (subgoals) in both rich (with markup)
    and plain text formats, with robust error handling.
*)

signature GOAL_DISPLAYER =
sig
  (* Returns the number of subgoals of a theorem. *)
  val subgoal_count : thm -> int

  (* Returns a pretty-printed string representation of the goals,
     including Isabelle markup (YXML). Errors are caught and reported. *)
  val format_goals_rich : Proof.context -> thm -> string

  (* Returns a plain-text string representation of the goals,
     with all XML markup stripped. Errors are caught and reported. *)
  val format_goals_plain : Proof.context -> thm -> string
end;

structure GoalDisplayer : GOAL_DISPLAYER =
struct

  (* Number of remaining subgoals. *)
  fun subgoal_count theorem = Thm.nprems_of theorem

  (* Pretty-print goals with markup, catching any exceptions. *)
  fun format_goals_rich context theorem =
    let
      val goals_output =
        (Pretty.string_of (Pretty.chunks (Goal_Display.pretty_goals context theorem))
         handle
           ERROR error_msg => "<cannot pretty-print goals (ERROR): " ^ error_msg ^ ">"
         | Fail fail_msg  => "<cannot pretty-print goals (Fail): " ^ fail_msg ^ ">")
    in
      goals_output
    end

  (* Pretty-print goals as plain text, stripping all YXML markup.
     Exceptions are caught and reported as error strings. *)
  fun format_goals_plain context theorem =
    let
      val plain_text =
        Goal_Display.pretty_goals context theorem
        |> Pretty.chunks
        |> Pretty.string_of
        |> YXML.parse_body
        |> XML.content_of
    in
      plain_text
    end
    handle
      ERROR error_msg => "<cannot pretty-print goals (ERROR): " ^ error_msg ^ ">"
    | Fail fail_msg  => "<cannot pretty-print goals (Fail): " ^ fail_msg ^ ">"

end;