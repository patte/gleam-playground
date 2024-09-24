// notes

// print size of acc in bytes
//
// import gleam/float
// case acc {
//   _ if acc % 1_000_000_000 == 0 -> {
//     logging.log(
//       logging.Info,
//       " acc: "
//         <> acc |> int.to_string |> string.slice(0, 10)
//         <> "... size: "
//         <> acc
//       |> int.to_base2
//       |> string.length
//       |> int.to_float
//       |> float.divide(8.0)
//       |> result.unwrap(0.0)
//       |> float.ceiling
//       |> float.to_string
//         <> " bytes",
//     )
//     Nil
//   }
//   _ -> Nil
// }

// print memory used by process
//
// erl
// % Returns the amount of memory used by the current process in bytes
// process_memory_used() ->
//     {memory, Mem} = process_info(self(), memory),
//     Mem.
//
// gleam
// @external(erlang, "erlib", "process_memory_used")
// pub fn process_memory_used() -> Int
//
// let memory_used = process_memory_used()
// logging.log(
//   logging.Info,
//   "process memory used:" <> memory_used |> int.to_string,
// )

// print system memory info
//
// erl
// % Get the system memory data as a list of tagged tuples
// get_system_memory_data() ->
//     erlang:memory().
//
// gleam
// @external(erlang, "erlib", "get_system_memory_data")
// pub fn get_system_memory_data() -> List(#(String, Int))
//
// let mem_info = get_system_memory_data()
// logging.log(
//   logging.Info,
//   "system memory data: " <> mem_info |> string.inspect,
// )
