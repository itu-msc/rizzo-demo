open! Rizzo.Types
open! Rizzo.Signal
open! Rizzo.Channel

let print_time_example () =
    (* Setup input channels, signals *)
    let console_channel = console_input () in
    let console_in = mkSig_of_channel console_channel in

    (* Create clock signal to sample time every second *)
    let every_second, every_second_stop = clock_signal 1.0 in
    let start_time = head every_second in

    (* Signal that only updates on "time" commands *)
    let time_filter = filterL (fun s -> s = "time") console_in in
    
    (* Sample the clock when time command has been registered *)
    let sampled_console = sampleL time_filter every_second in

    (* Output the time to the console *)
    let formatted =
      (mapL (fun (_, f) -> string_of_float (f -. start_time))
          sampled_console) in
    console_outputL formatted;

    (* Start the event loop *)
    start_event_loop ();
    (* Stop the clock when the event loop is over *)
    every_second_stop ()

let time_step_example () = 
  let console_channel = console_input () in
  let port_channel = port_input 9001 in

  let console_in = mkSig_of_channel console_channel in
  let port_in = mkSig_of_channel port_channel in
  
  (* Send console input to port output *)
  port_outputL Unix.inet_addr_loopback 9001 console_in;

  (* Write input to console out *)
  console_outputL (mapL (fun s -> "From port: " ^ s) port_in);
  console_outputL (mapL (fun s -> "From console: " ^ s) console_in);

  start_event_loop ()

let growing_heap_example () =
  let g = ref None in
  let timer, _ = clock_signal 0.1 in
  g := Some timer;
  start_event_loop ()

let () =
  print_endline "Select an example to run:";
  print_endline "1. Print time example";
  print_endline "2. Time steps example";
  print_endline "3. Growing heap example";
  print_string "Enter your choice (1, 2 or 3): ";
  flush stdout;
  match read_line () with
  | "1" -> print_time_example ()
  | "2" -> time_step_example ()
  | "3" -> growing_heap_example ()
  | _ -> print_endline "Invalid choice"
