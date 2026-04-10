(*  Title:      event_router.ML
    Author:     davidebonav
    Project:    TacticProfiler

    Configurable output sinks that route trace events (goals, messages,
    metadata) to different destinations (stdout, files, multiple sinks).
    Each sink can be enabled/disabled and uses a formatter to convert
    events to strings.
*)

signature EVENT_ROUTER =
sig
  (* Formatter type: a record of functions that optionally produce a string
     from a trace event for each event category. *)
  type event_formatter = EventFormatter.event_formatter

  (* Output sink types *)
  datatype output_sink =
      ConsoleSink of { enabled: bool, formatter: event_formatter }
    | FileSink    of { enabled: bool, formatter: event_formatter, file_path: string }
    | CompositeSink of output_sink list
    | NullSink

  (* Smart constructors *)
  val console_sink    : bool -> event_formatter -> output_sink
  val file_sink       : bool -> event_formatter -> string -> output_sink
  val composite_sink  : output_sink list -> output_sink
  val null_sink       : output_sink

  (* Predefined formatters for common use cases *)
  val console_sink_with_default_formatter   : bool -> output_sink
  val file_sink_with_default_formatter      : bool -> string -> output_sink

  (* Dynamic sink construction based on flags *)
  val dynamic_sink : { use_console: bool, use_file: bool } -> bool -> string -> output_sink
  val dynamic_sink_with_formatters :
       { use_console: bool, use_file: bool }
    -> { console_formatter: event_formatter, file_formatter: event_formatter }
    -> bool -> string -> output_sink

  (* Write an event to a sink *)
  val write_event : output_sink -> TraceEventTypes.trace_event -> unit
end;

structure EventRouter : EVENT_ROUTER =
struct

  type event_formatter = EventFormatter.event_formatter

  datatype output_sink =
      ConsoleSink of { enabled: bool, formatter: event_formatter }
    | FileSink    of { enabled: bool, formatter: event_formatter, file_path: string }
    | CompositeSink of output_sink list
    | NullSink

  (* -------------------------------------------------------------------------
     Default formatters (matching typical use cases)
     ------------------------------------------------------------------------- *)

  (* Console formatter: shows goals with markup, plain messages, serialized metadata *)
  val default_console_formatter : event_formatter = {
    on_display_goals = EventFormatter.format_goals_for_console,
    on_log_message   = EventFormatter.format_log_message,
    on_emit_metadata = EventFormatter.serialize_metadata
  }

  (* File formatter: plain goals (no markup), same messages, serialized metadata *)
  val default_file_formatter : event_formatter = {
    on_display_goals = EventFormatter.format_goals_for_file,
    on_log_message   = EventFormatter.format_log_message,
    on_emit_metadata = EventFormatter.serialize_metadata
  }

  (* -------------------------------------------------------------------------
     Smart constructors
     ------------------------------------------------------------------------- *)

  fun console_sink enabled formatter = ConsoleSink { enabled = enabled, formatter = formatter }
  fun file_sink enabled formatter path = FileSink { enabled = enabled, formatter = formatter, file_path = path }
  fun composite_sink sinks = CompositeSink sinks
  val null_sink = NullSink

  fun console_sink_with_default_formatter enabled =
    console_sink enabled default_console_formatter

  fun file_sink_with_default_formatter enabled path =
    file_sink enabled default_file_formatter path

  (* -------------------------------------------------------------------------
     Dynamic sink: builds a single sink (composite if needed) from flags.
     ------------------------------------------------------------------------- *)

  local
    fun collect_sinks use_console console_sink use_file file_sink =
      (if use_console then [console_sink] else []) @
      (if use_file then [file_sink] else [])

    fun build_sink_from_list [] = NullSink
      | build_sink_from_list [single] = single
      | build_sink_from_list multiple = CompositeSink multiple
  in
    fun dynamic_sink { use_console, use_file } enabled file_path =
      let
        val console = console_sink_with_default_formatter enabled
        val file = file_sink_with_default_formatter enabled file_path
        val sinks = collect_sinks use_console console use_file file
      in
        build_sink_from_list sinks
      end

    fun dynamic_sink_with_formatters { use_console, use_file }
                                     { console_formatter, file_formatter }
                                     enabled file_path =
      let
        val console = console_sink enabled console_formatter
        val file = file_sink enabled file_formatter file_path
        val sinks = collect_sinks use_console console use_file file
      in
        build_sink_from_list sinks
      end
  end

  (* -------------------------------------------------------------------------
     Output primitives
     ------------------------------------------------------------------------- *)

  local
    fun write_to_console text = tracing text   (* Isabelle’s standard output channel *)

    fun write_to_file file_path text =
      let
        (* Use the robust file I/O from earlier refactoring *)
        val _ = RobustFileIO.append_line file_path text
      in
        ()
      end

    (* Apply the appropriate formatter function based on the event type *)
    fun format_event formatter event =
      case event of
        TraceEventTypes.DisplayGoals _ => #on_display_goals formatter event
      | TraceEventTypes.LogMessage   _ => #on_log_message   formatter event
      | TraceEventTypes.EmitMetadata  _ => #on_emit_metadata formatter event

    fun write_if_enabled should_write write_function formatter event =
      if should_write then
        case format_event formatter event of
          SOME formatted_string => write_function formatted_string
        | NONE => ()
      else ()
  in
    (* Dispatch an event to a sink *)
    fun write_event sink event =
      case sink of
        ConsoleSink { enabled, formatter } =>
          write_if_enabled enabled write_to_console formatter event

      | FileSink { enabled, formatter, file_path } =>
          write_if_enabled enabled (write_to_file file_path) formatter event

      | CompositeSink sinks =>
          List.app (fn s => write_event s event) sinks

      | NullSink => ()
  end

end;
