signature TACTIC_PROFILER_CONFIG =
sig
  val tracer_name : string
  val trace_goals : Proof.context -> bool
  val trace_unchanged : Proof.context -> bool
  val benchmark_iterations : Proof.context -> int
  val output_sink : Proof.context -> EventRouter.output_sink
end;