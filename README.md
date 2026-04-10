# TacticProfiler

A modular, configurable framework for profiling tactic execution in Isabelle/Pure.  
It provides **timing measurements**, **benchmarking**, **metadata logging** (key‑value store),  
**goal tracing** (rich or plain text), and **configurable output sinks** (console, file, or both).  
All components are fully refactored, documented, and follow a clean dependency hierarchy.

---

## Table of Contents

- [Features](#features)
- [Architecture](#architecture)
- [Module Overview](#module-overview)
- [Getting Started](#getting-started)
- [Usage Examples](#usage-examples)
- [Configuration](#configuration)
- [Extending the Framework](#extending-the-framework)
- [License](#license)

---

## Features

- **⏱️ Timing & Benchmarking** – measure real and CPU time, run multiple iterations, compute mean/standard deviation, and trim outliers.
- **📊 Metadata Collection** – attach arbitrary typed data (strings, ints, bools, reals, contexts, theorems) to every tactic execution.
- **🖨️ Goal Tracing** – print subgoals after each step (rich XML for console, plain text for files).
- **📁 Multiple Output Sinks** – send logs to stdout, files, or both simultaneously; enable/disable per sink.
- **🔧 Functor‑based Design** – instantiate the profiler with your own configuration (tracer name, benchmark iterations, output sink, etc.).
- **🧩 Fully Documented** – every `.ML` file contains a formal header with title, author, project, and description.
- **⚙️ Platform‑aware I/O** – works on Unix and Windows (uses bash emulation when needed).

---

## Architecture

The project is split into four layers:

```
lib/                → platform‑independent base libraries
core/               → Isabelle‑specific utilities (goals, theorems, benchmarking)
tracing/            → event types, formatters, and output routers
profiling/          → the configurable functor that puts everything together
```

### Dependency Graph

```
robust_file_io.ML ──┐
content_hasher.ML   ├──> dynamic_metadata.ML ──> theory_metadata.ML
goal_displayer.ML ──┘
                              │
performance_benchmarker.ML ──┼──> trace_event_types.ML ──> event_formatter.ML ──> event_router.ML
                              │
                              └──> tactic_profiler_config.ML ──> tactic_profiler.ML (functor)
```

All modules are loaded via `ML_file` in the main theory `TacticProfiler.thy` in correct dependency order.

---

## Module Overview

| File | Description |
|------|-------------|
| `lib/robust_file_io.ML` | Safe file write/append with automatic parent directory creation (Unix/Windows). |
| `lib/content_hasher.ML` | SHA‑1 digest of strings and theorem full propositions. |
| `lib/dynamic_metadata.ML` | Typed key‑value store (string → int/bool/real/context/thm/…). |
| `core/goal_displayer.ML` | Pretty‑print subgoals (rich with XML or plain text). |
| `core/theory_metadata.ML` | Extract theory name, theorem hash, subgoal count, alpha‑equivalence. |
| `core/performance_benchmarker.ML` | Time a function/tactic; run benchmarks; compute statistics (mean, variance, outlier trimming). |
| `tracing/trace_event_types.ML` | Datatype for events: `DisplayGoals`, `LogMessage`, `EmitMetadata`. |
| `tracing/event_formatter.ML` | Convert events to strings (goals, messages, serialised metadata). |
| `tracing/event_router.ML` | Output sinks (console, file, composite) with enabled/disabled state. |
| `profiling/tactic_profiler_config.ML` | Signature for the functor configuration. |
| `profiling/tactic_profiler.ML` | Functor `TacticProfilerFn` that adds profiling to any tactic. |

---

## Getting Started

### Prerequisites

- **Isabelle2024** (or later) – the framework uses `SHA1`, `Goal_Display`, `Timer`, etc.
- No additional external libraries are required.

### Building the Session

Place the whole `TacticProfiler` folder inside your Isabelle `contrib` directory or link it as a session.  
A minimal `ROOT` file is provided. To build the session, run:

```bash
isabelle build -D TacticProfiler
```

### Loading in a Theory

```isabelle
theory MyProfilingTest
imports TacticProfiler
begin

(* Instantiate a profiler with custom settings *)
ML ‹
structure MyConfig : TACTIC_PROFILER_CONFIG =
struct
  val tracer_name = "MyProfiler"
  fun trace_goals _ = true
  fun trace_unchanged _ = false
  fun benchmark_iterations _ = 5
  fun output_sink _ = EventRouter.console_sink_with_default_formatter true
end

structure MyProfiler = TacticProfilerFn(MyConfig);
›

(* Now use MyProfiler.trace_tactic, etc. *)
end
```

---

## Usage Examples

### 1. Trace a Single Tactic

```isabelle
ML ‹
fun my_tactic ctxt = resolve_tac ctxt @{thms refl} 1

val traced = MyProfiler.trace_tactic @{context} "my_tactic_name" my_tactic

(* Apply to a goal state *)
val st = @{prop "x = x"} |> Thm.cterm_of @{context} |> Goal.init
val seq = traced st
›
```

### 2. Benchmark a List of Tactics

```isabelle
ML ‹
val tactics = [
  ("resolve_refl", resolve_tac @{context} @{thms refl} 1),
  ("assumption",   assume_tac @{context})
]

val st = @{prop "P ⟹ P"} |> Thm.cterm_of @{context} |> Goal.init
val _ = MyProfiler.benchmark_tactics @{context} tactics st
›
```

### 3. Write Logs to a File

```isabelle
ML ‹
structure FileConfig : TACTIC_PROFILER_CONFIG =
struct
  val tracer_name = "FileTracer"
  fun trace_goals _ = true
  fun trace_unchanged _ = true
  fun benchmark_iterations _ = 0
  fun output_sink _ = EventRouter.file_sink_with_default_formatter true "/tmp/profiler.log"
end

structure FileProfiler = TacticProfilerFn(FileConfig);
›
```

### 4. Composite Sink (Console + File)

```isabelle
val console = EventRouter.console_sink_with_default_formatter true
val file    = EventRouter.file_sink_with_default_formatter true "/tmp/trace.log"
val both    = EventRouter.composite_sink [console, file]

(* Use both in configuration *)
fun output_sink _ = both
```

---

## Configuration

The functor `TacticProfilerFn` expects a structure of type `TACTIC_PROFILER_CONFIG` with the following fields:

| Field | Type | Description |
|-------|------|-------------|
| `tracer_name` | `string` | A name that will be added to every metadata entry (key `tracer_name`). |
| `trace_goals` | `Proof.context -> bool` | Whether to print subgoals after each successful step. |
| `trace_unchanged` | `Proof.context -> bool` | Whether to log metadata even when the theorem did not change (e.g., after `rotate_tac`). |
| `benchmark_iterations` | `Proof.context -> int` | Number of warm‑up iterations for benchmarking (0 disables benchmarking). |
| `output_sink` | `Proof.context -> EventRouter.output_sink` | Where to write the logs (console, file, composite). |

---

## Extending the Framework

The layered design makes it easy to extend:

- **New metadata types** – add a constructor to `DynamicMetadata.metadata_value`.
- **New event types** – extend `trace_event_types` and update the formatter.
- **New output sinks** – add a case to `EventRouter.output_sink` and implement the `write_event` dispatch.
- **Different benchmark statistics** – modify `StatisticalSummary` inside `performance_benchmarker.ML`.

All modules are documented with a standard header (`(* Title: ... *)`), so reading the source is straightforward.

---

<!-- ## License

This project is released under the **MIT License**.  
See the `LICENSE` file for details. -->

---

<!-- **Author:** davidebonav  
**Project:** TacticProfiler  
**Date:** 2026‑04‑04 -->