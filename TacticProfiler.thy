(*  Title:      TacticProfiler.thy
    Author:     davidebonav
    Project:    TacticProfiler

    Main entry point for the tactic profiling framework.
    Loads all modules in dependency order.
*)

theory TacticProfiler
imports Pure
begin

(* ========== Level 0: Platform-independent base libraries ========== *)
ML_file "lib/robust_file_io.ML"          (* Safe file operations *)
ML_file "lib/content_hasher.ML"          (* SHA‑1 hashing of terms *)
ML_file "lib/dynamic_metadata.ML"        (* Typed key‑value store *)

(* ========== Level 1: Core domain utilities ========== *)
ML_file "core/goal_displayer.ML"         (* Goal pretty‑printing *)
ML_file "core/theory_metadata.ML"        (* Theory/thm metadata extraction *)
ML_file "core/performance_benchmarker.ML"(* Timing and benchmarking *)

(* ========== Level 2: Event tracing and formatting ========== *)
ML_file "tracing/trace_event_types.ML"   (* Event datatype *)
ML_file "tracing/event_formatter.ML"     (* Event \<rightarrow> string conversion *)
ML_file "tracing/event_router.ML"        (* Output sinks (console/file) *)

(* ========== Level 3: Configurable tactic profiler ========== *)
ML_file "profiling/tactic_profiler_config.ML"  (* Signature for functor config *)
ML_file "profiling/tactic_profiler.ML"         (* Functor implementing the profiler *)

(* Optional: instantiate a default profiler with standard settings *)
ML \<open>
structure DefaultProfilerConfig : TACTIC_PROFILER_CONFIG =
struct
  val tracer_name = "DefaultTracer"
  fun trace_goals _ = true
  fun trace_unchanged _ = false
  fun benchmark_iterations _ = 10
  fun output_sink _ = EventRouter.console_sink_with_default_formatter true
end;

structure DefaultProfiler = TacticProfilerFn(DefaultProfilerConfig);
\<close>

end