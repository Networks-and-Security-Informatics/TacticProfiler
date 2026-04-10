(*  Title:      performance_benchmarker.ML
    Author:     davidebonav
    Project:    TacticProfiler

    Functions for measuring execution time of tactics, functions, and
    for running benchmarks with statistical aggregation (mean, variance,
    outlier trimming). Supports both single-run timing and multi-iteration
    benchmarking.
*)

signature PERFORMANCE_BENCHMARKER =
sig
  (* Result of a timed tactic execution: metadata and resulting sequence *)
  type measured_tactic_result = {
    metadata : DynamicMetadata.metadata_store,
    seq      : thm Seq.seq
  }

  (* Single-run timing *)
  val measure_tactic            : tactic -> thm -> measured_tactic_result
  val measure_indexed_tactic    : (int -> tactic) -> int -> thm -> measured_tactic_result
  val measure_function          : (unit -> 'a) -> 'a * DynamicMetadata.metadata_store

  (* Multi-iteration benchmarking (returns aggregated statistics) *)
  val benchmark_tactic          : int -> tactic -> thm -> DynamicMetadata.metadata_store
  val benchmark_indexed_tactic  : int -> (int -> tactic) -> int -> thm -> DynamicMetadata.metadata_store
  val benchmark_function        : int -> (unit -> 'a) -> DynamicMetadata.metadata_store

  (* Benchmark multiple items (interleaved) and return per‑item statistics *)
  val benchmark_functions       : int -> (string * (unit -> unit)) list
                                    -> (string * DynamicMetadata.metadata_store) list
  val benchmark_tactics         : int -> (string * tactic) list -> thm
                                    -> (string * DynamicMetadata.metadata_store) list
  val benchmark_indexed_tactics : int -> (string * (int -> tactic)) list
                                    -> int -> thm -> (string * DynamicMetadata.metadata_store) list

  (* Statistical helpers *)
  structure StatisticalSummary : sig
    val compute_summary   : int list -> DynamicMetadata.metadata_store
    val discard_extremes  : int list -> int list
  end
end;

structure PerformanceBenchmarker : PERFORMANCE_BENCHMARKER =
struct

  type measured_tactic_result = {
    metadata : DynamicMetadata.metadata_store,
    seq      : thm Seq.seq
  }

  (* -------------------------------------------------------------------------
     Helpers for time conversion and measurement
     ------------------------------------------------------------------------- *)

  local
    fun real_time_in_ms t = Time.toReal t * 1000.0
    fun real_time_in_us t = Time.toMicroseconds t
  in
    (* Measure the execution time and CPU time of a computation.
       Returns the result (or raises the exception) together with metadata. *)
    fun measure_execution_time (thunk : unit -> 'a) : 'a * DynamicMetadata.metadata_store =
      let
        val real_timer   = Timer.startRealTimer ()
        val cpu_timer    = Timer.startCPUTimer ()
        val start_real   = Time.now ()

        val captured_result = Exn.capture thunk ()

        val elapsed_real = Timer.checkRealTimer real_timer
        val {usr, sys}   = Timer.checkCPUTimer cpu_timer
        val total_cpu    = Time.+ (usr, sys)
      in
        case captured_result of
          Exn.Res value =>
            (value,
             DynamicMetadata.empty
             |> DynamicMetadata.add_bool StandardMetadataKeys.success_flag true
             |> DynamicMetadata.add_int StandardMetadataKeys.timestamp_us (real_time_in_us start_real)
             |> DynamicMetadata.add_int StandardMetadataKeys.elapsed_ms (Real.round (real_time_in_ms elapsed_real))
             |> DynamicMetadata.add_int StandardMetadataKeys.elapsed_us (real_time_in_us elapsed_real)
             |> DynamicMetadata.add_int StandardMetadataKeys.cpu_time_ms (Real.round (real_time_in_ms total_cpu))
             |> DynamicMetadata.add_int StandardMetadataKeys.cpu_time_us (real_time_in_us total_cpu))
        | Exn.Exn exception_raised =>
            (* Re-raise the exception; we do not store error metadata
               because the caller should handle the failure. *)
            Exn.reraise exception_raised
      end
  end

  (* Force the evaluation of a tactic’s first result (if any) without
     consuming the whole sequence. Useful for measuring overhead. *)
  fun force_tactic_application tactic state =
    let
      val first_cell = Seq.pull (tactic state)
      val _ = case first_cell of
                SOME (result_thm, _) => (Thm.prop_of result_thm; ())
              | NONE => ()
    in
      first_cell
    end

  (* -------------------------------------------------------------------------
     Single‑run timing
     ------------------------------------------------------------------------- *)

  fun measure_tactic (tactic : tactic) (initial_state : thm) : measured_tactic_result =
    let
      val (result_cell, metadata) = measure_execution_time (fn () => force_tactic_application tactic initial_state)
      val resulting_sequence =
        case result_cell of
          NONE => Seq.empty
        | SOME (first_thm, remaining_seq) => Seq.cons first_thm remaining_seq
    in
      { metadata = metadata, seq = resulting_sequence }
    end

  fun measure_indexed_tactic (indexed_tactic : int -> tactic) (index : int) (initial_state : thm) =
    measure_tactic (indexed_tactic index) initial_state

  fun measure_function (thunk : unit -> 'a) : 'a * DynamicMetadata.metadata_store =
    measure_execution_time thunk

  (* -------------------------------------------------------------------------
     Statistical aggregation (outlier trimming, mean, variance, stddev)
     ------------------------------------------------------------------------- *)

  structure StatisticalSummary =
  struct
    fun safe_integer_division dividend divisor =
      if divisor = 0 then 0 else dividend div divisor

    (* Remove the first occurrence of x from the list *)
    fun remove_first _ [] = []
      | remove_first element (y :: ys) =
          if element = y then ys else y :: remove_first element ys

    fun find_extreme compare_func items =
      fold (fn item => fn extreme => if compare_func (item, extreme) then item else extreme) items (hd items)

    fun discard_extremes [] = []
      | discard_extremes [single] = [single]
      | discard_extremes values =
        let val max_val = find_extreme (fn (a, b) => a > b) values
            val min_val = find_extreme (fn (a, b) => a < b) values
        in
          remove_first min_val values |> remove_first max_val
        end

    fun compute_variance (values : int list) (length : int) (mean : int) : int =
      fold (fn v => fn acc => (v - mean) * (v - mean) + acc) values 0
      |> (fn sum_sq => safe_integer_division sum_sq length)

    fun compute_stddev (variance : int) : int =
      Real.round (Math.sqrt (Real.fromInt variance))

    fun compute_summary (raw_times : int list) : DynamicMetadata.metadata_store =
      let
        val times = raw_times   (* caller is responsible for trimming outliers *)
        val n = length times
        val maximum = find_extreme (fn (a, b) => a > b) times
        val minimum = find_extreme (fn (a, b) => a < b) times
        val total = fold (fn t => fn sum => t + sum) times 0
        val average = safe_integer_division total n
        val variance = compute_variance times n average
        val stddev = compute_stddev variance
      in
        DynamicMetadata.empty
        |> DynamicMetadata.add_int StandardMetadataKeys.iterations n
        |> DynamicMetadata.add_int StandardMetadataKeys.average_us average
        |> DynamicMetadata.add_int StandardMetadataKeys.minimum_us minimum
        |> DynamicMetadata.add_int StandardMetadataKeys.maximum_us maximum
        |> DynamicMetadata.add_int StandardMetadataKeys.stddev_us stddev
        |> DynamicMetadata.add_int StandardMetadataKeys.total_us total
      end
  end

  (* -------------------------------------------------------------------------
     Multi‑iteration benchmarking (single item)
     ------------------------------------------------------------------------- *)

  fun benchmark_function (iterations : int) (thunk : unit -> 'a) : DynamicMetadata.metadata_store =
    let
      val _ = thunk ()   (* warm‑up / dry run *)

      fun run_one _ =
        let val (_, metadata) = measure_execution_time thunk
        in
          case DynamicMetadata.get_int StandardMetadataKeys.elapsed_us metadata of
            SOME us => us
          | NONE => raise Fail "benchmark_function: missing elapsed_us in metadata"
        end

      val raw_samples = map run_one (1 upto iterations)
      val trimmed_samples = StatisticalSummary.discard_extremes raw_samples
    in
      StatisticalSummary.compute_summary trimmed_samples
    end

  fun benchmark_tactic (iterations : int) (tactic : tactic) (initial_state : thm) =
    benchmark_function iterations (fn () => force_tactic_application tactic initial_state)

  fun benchmark_indexed_tactic (iterations : int) (indexed_tactic : int -> tactic) (index : int) (initial_state : thm) =
    benchmark_tactic iterations (indexed_tactic index) initial_state

  (* -------------------------------------------------------------------------
     Interleaved benchmarking of multiple items
     (each iteration runs all items once to reduce temporal bias)
     ------------------------------------------------------------------------- *)

  fun benchmark_functions (iterations : int) (named_functions : (string * (unit -> unit)) list)
      : (string * DynamicMetadata.metadata_store) list =
    let
      (* warm‑up: run each function once *)
      List.app (fn (_, f) => f ()) named_functions

      (* accumulator: string -> int list (raw times) *)
      fun accumulate_one_sample (name, func) acc =
        let val (_, metadata) = measure_execution_time func
            val time_us = case DynamicMetadata.get_int StandardMetadataKeys.elapsed_us metadata of
                            SOME us => us
                          | NONE => raise Fail "benchmark_functions: missing elapsed_us"
        in
          Symtab.map_default (name, []) (fn times => time_us :: times) acc
        end

      fun run_one_round acc =
        fold accumulate_one_sample named_functions acc

      val all_samples =
        fold (fn _ => run_one_round) (1 upto iterations) Symtab.empty

      fun finalize_entry (name, raw_times) =
        (name, StatisticalSummary.compute_summary (StatisticalSummary.discard_extremes raw_times))
    in
      Symtab.dest all_samples |> map finalize_entry
    end

  fun benchmark_tactics (iterations : int) (named_tactics : (string * tactic) list) (initial_state : thm)
      : (string * DynamicMetadata.metadata_store) list =
    let
      fun tactic_to_thunk (name, tactic) =
        (name, fn () => (Seq.pull (tactic initial_state); ()))
    in
      benchmark_functions iterations (map tactic_to_thunk named_tactics)
    end

  fun benchmark_indexed_tactics (iterations : int) (named_indexed_tactics : (string * (int -> tactic)) list)
                                 (index : int) (initial_state : thm) =
    let
      fun apply_index (name, indexed_tac) = (name, indexed_tac index)
    in
      benchmark_tactics iterations (map apply_index named_indexed_tactics) initial_state
    end

end;