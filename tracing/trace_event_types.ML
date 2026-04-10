(*  Title:      trace_event_types.ML
    Author:     davidebonav
    Project:    TacticProfiler

    Definition of traceable events that can occur during tactic execution.
    Events include goal displays, log messages, and metadata dumps.
*)

signature TRACE_EVENT_TYPES =
sig
  (* Kinds of events that can be recorded during profiling *)
  datatype trace_event =
      DisplayGoals of { context: Proof.context, theorem: thm }
    | LogMessage   of string
    | EmitMetadata of DynamicMetadata.metadata_store
end;

structure TraceEventTypes : TRACE_EVENT_TYPES =
struct
  datatype trace_event =
      DisplayGoals of { context: Proof.context, theorem: thm }
    | LogMessage   of string
    | EmitMetadata of DynamicMetadata.metadata_store
end;