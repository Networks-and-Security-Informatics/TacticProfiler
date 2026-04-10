(*  Title:      event_formatter.ML
    Author:     davidebonav
    Project:    TacticProfiler

    Formatters that convert trace events into human‑readable or machine‑readable
    strings. Provides a record of formatting functions and several predefined
    formatters for console output, file logging, and metadata serialization.
*)

signature EVENT_FORMATTER =
sig
  (* A collection of formatting functions for different event types.
     Each function returns NONE if the event type is not handled. *)
  type event_formatter = {
    on_display_goals : TraceEventTypes.trace_event -> string option,
    on_log_message   : TraceEventTypes.trace_event -> string option,
    on_emit_metadata : TraceEventTypes.trace_event -> string option
  }

  (* Serialize a metadata event as a semicolon‑separated key=value string. *)
  val serialize_metadata : TraceEventTypes.trace_event -> string option

  (* Format a DisplayGoals event for console output (rich goals with markup). *)
  val format_goals_for_console : TraceEventTypes.trace_event -> string option

  (* Format a DisplayGoals event for file output (plain text, no markup). *)
  val format_goals_for_file : TraceEventTypes.trace_event -> string option

  (* Extract the message string from a LogMessage event. *)
  val format_log_message : TraceEventTypes.trace_event -> string option
end;

structure EventFormatter : EVENT_FORMATTER =
struct

  type event_formatter = {
    on_display_goals : TraceEventTypes.trace_event -> string option,
    on_log_message   : TraceEventTypes.trace_event -> string option,
    on_emit_metadata : TraceEventTypes.trace_event -> string option
  }

  (* -------------------------------------------------------------------------
     Helper: convert a metadata value to a string representation.
     ------------------------------------------------------------------------- *)
  fun metadata_value_to_string (DynamicMetadata.String s) = s
    | metadata_value_to_string (DynamicMetadata.Int i)    = Int.toString i
    | metadata_value_to_string (DynamicMetadata.Bool b)   = Bool.toString b
    | metadata_value_to_string (DynamicMetadata.Real r)   = Real.toString r
    | metadata_value_to_string (DynamicMetadata.Ctxt _)   = "<context>"
    | metadata_value_to_string (DynamicMetadata.Thm _)    = "<theorem>"

  (* -------------------------------------------------------------------------
     Serialize a DumpMeta event: key=value;key=value;...
     ------------------------------------------------------------------------- *)
  fun serialize_metadata (TraceEventTypes.EmitMetadata store) =
        let
          val pairs = Symtab.dest store
          val components = map (fn (key, value) => key ^ "=" ^ metadata_value_to_string value) pairs
        in
          SOME (String.concatWith ";" components)
        end
    | serialize_metadata _ = NONE

  (* -------------------------------------------------------------------------
     Format a DisplayGoals event with a user‑supplied goal printer.
     ------------------------------------------------------------------------- *)
  fun format_goals_with printer (TraceEventTypes.DisplayGoals {context, theorem}) =
        let
          val subgoal_count = TheoryMetadata.subgoal_count theorem
          val goals_text = printer context theorem
        in
          SOME (Int.toString subgoal_count ^ " subgoal(s) to close:\n" ^ goals_text)
        end
    | format_goals_with _ _ = NONE

  (* Rich goals with XML markup (suitable for Isabelle/jEdit console) *)
  fun format_goals_for_console event =
    format_goals_with GoalDisplayer.format_goals_rich event

  (* Plain goals with markup stripped (suitable for log files) *)
  fun format_goals_for_file event =
    format_goals_with GoalDisplayer.format_goals_plain event

  (* Extract the message string from a LogMessage event *)
  fun format_log_message (TraceEventTypes.LogMessage msg) = SOME msg
    | format_log_message _ = NONE

end;