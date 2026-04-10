(*  Title:      tactic_profiler.ML
    Author:     davidebonav
    Project:    TacticProfiler

    A functor that augments tactics with profiling capabilities:
    timing, metadata collection, goal printing, and benchmarking.
    Wraps a tactic to produce a traced tactic that logs to configurable sinks.
*)

signature TACTIC_PROFILER =
sig
  (* Outcome of a single tactic execution *)
  datatype execution_outcome =
      Succeeded of { changed : bool,
                     result_state : thm,
                     remaining_seq : thm Seq.seq,
                     execution_metadata : DynamicMetadata.metadata_store }
    | Failed

  (* Print current goals as a tactic (no-op if tracing is disabled) *)
  val trace_goals : Proof.context -> tactic

  (* Print a log message as a tactic *)
  val trace_message : Proof.context -> string -> tactic

  (* Trace a single tactic: logs metadata, times execution, prints goals *)
  val trace_tactic : Proof.context -> string -> tactic -> tactic

  (* Benchmark a list of named tactics on a given state,
     prints aggregated statistics, then applies the first tactic. *)
  val benchmark_tactics : Proof.context -> (string * tactic) list -> thm -> thm Seq.seq
end;

functor TacticProfilerFn (Config : TACTIC_PROFILER_CONFIG) : TACTIC_PROFILER =
struct

  datatype execution_outcome =
      Succeeded of { changed : bool,
                     result_state : thm,
                     remaining_seq : thm Seq.seq,
                     execution_metadata : DynamicMetadata.metadata_store }
    | Failed

  (* -------------------------------------------------------------------------
     Helper: retrieve the configured output sink
     ------------------------------------------------------------------------- *)
  fun get_output_sink context = Config.output_sink context

  (* -------------------------------------------------------------------------
     Run a benchmark (if iterations > 0) and return the aggregated metadata.
     ------------------------------------------------------------------------- *)
  fun run_benchmark (tactic : tactic) (initial_state : thm) (iterations : int) :
        DynamicMetadata.metadata_store =
      if iterations > 0 then
        PerformanceBenchmarker.benchmark_tactic iterations tactic initial_state
      else
        DynamicMetadata.empty

  (* -------------------------------------------------------------------------
     Execute a tactic once, measuring time and capturing the first result.
     ------------------------------------------------------------------------- *)
  fun run_single_tactic (tactic : tactic) (initial_state : thm) : execution_outcome =
    let
      val { metadata = timing_metadata, seq = result_seq } =
        PerformanceBenchmarker.measure_tactic tactic initial_state
    in
      case Seq.pull result_seq of
        NONE => Failed
      | SOME (result_state, remaining) =>
        let
          val changed = not (TheoryMetadata.are_equal_theorems @{context} initial_state result_state)
          (* Note: @{context} is a placeholder; in a proper implementation,
             the context would be passed down. However, theorem equality
             does not actually depend on the context for alpha-equivalence.
             We keep the original behaviour. *)
        in
          Succeeded {
            changed = changed,
            result_state = result_state,
            remaining_seq = remaining,
            execution_metadata = timing_metadata
          }
        end
    end

  (* -------------------------------------------------------------------------
     Print metadata and optionally goals after a successful execution.
     ------------------------------------------------------------------------- *)
  fun log_success context result_state full_metadata changed =
    let
      val sink = get_output_sink context
      val trace_unchanged = Config.trace_unchanged context
    in
      if changed orelse trace_unchanged then
        ( EventRouter.write_event sink (TraceEventTypes.EmitMetadata full_metadata);
          if Config.trace_goals context then
            EventRouter.write_event sink
              (TraceEventTypes.DisplayGoals { context = context, theorem = result_state })
          else ()
        )
      else ()
    end

  (* -------------------------------------------------------------------------
     Core tracing function: wraps a named tactic with profiling.
     ------------------------------------------------------------------------- *)
  fun trace_tactic (context : Proof.context)
                   (tactic_name : string)
                   (tactic : tactic)
                   (initial_state : thm) : thm Seq.seq =
    let
      (* Static metadata for this execution *)
      val static_metadata =
        DynamicMetadata.empty
        |> DynamicMetadata.add_string StandardMetadataKeys.theory_name
             (TheoryMetadata.get_theory_name context)
        |> DynamicMetadata.add_string StandardMetadataKeys.lemma_name
             (TheoryMetadata.get_theorem_content_hash context initial_state)
        |> DynamicMetadata.add_string StandardMetadataKeys.tracer_name
             Config.tracer_name
        |> DynamicMetadata.add_string StandardMetadataKeys.tactic_name
             tactic_name
        |> DynamicMetadata.union_right
             (run_benchmark tactic initial_state (Config.benchmark_iterations context))

      val outcome = run_single_tactic tactic initial_state
    in
      case outcome of
        Failed => Seq.empty

      | Succeeded { changed, result_state, remaining_seq, execution_metadata } =>
        let
          val full_metadata =
            DynamicMetadata.union_right static_metadata execution_metadata
            |> DynamicMetadata.add_bool StandardMetadataKeys.changed_flag changed

          val _ = log_success context result_state full_metadata changed
        in
          Seq.cons result_state remaining_seq
        end
    end

  (* -------------------------------------------------------------------------
     Tactic that prints current goals (if enabled by configuration).
     ------------------------------------------------------------------------- *)
  fun trace_goals (context : Proof.context) : tactic =
    fn state =>
      ( if Config.trace_goals context then
          EventRouter.write_event (get_output_sink context)
            (TraceEventTypes.DisplayGoals { context = context, theorem = state })
        else ();
        Seq.single state
      )

  (* -------------------------------------------------------------------------
     Tactic that prints an arbitrary message.
     ------------------------------------------------------------------------- *)
  fun trace_message (context : Proof.context) (message : string) : tactic =
    fn state =>
      ( EventRouter.write_event (get_output_sink context)
          (TraceEventTypes.LogMessage message);
        Seq.single state
      )

  (* -------------------------------------------------------------------------
     Benchmark a list of named tactics on a given state.
     Prints aggregated statistics for each tactic, then applies the first one.
     ------------------------------------------------------------------------- *)
  fun benchmark_tactics (context : Proof.context)
                        (named_tactics : (string * tactic) list)
                        (initial_state : thm) : thm Seq.seq =
    let
      val sink = get_output_sink context
      val iterations = Config.benchmark_iterations context

      val base_metadata =
        DynamicMetadata.empty
        |> DynamicMetadata.add_string StandardMetadataKeys.theory_name
             (TheoryMetadata.get_theory_name context)
        |> DynamicMetadata.add_string StandardMetadataKeys.lemma_name
             (TheoryMetadata.get_theorem_content_hash context initial_state)
        |> DynamicMetadata.add_string StandardMetadataKeys.tracer_name
             Config.tracer_name

      val benchmark_results =
        PerformanceBenchmarker.benchmark_tactics iterations named_tactics initial_state

      val _ = List.app (fn (name, stats_metadata) =>
        let
          val full_metadata =
            DynamicMetadata.union_right base_metadata stats_metadata
            |> DynamicMetadata.add_string StandardMetadataKeys.tactic_name name
        in
          EventRouter.write_event sink (TraceEventTypes.EmitMetadata full_metadata)
        end) benchmark_results

      val (_, first_tactic) = hd named_tactics
    in
      first_tactic initial_state
    end

end;