// an example of a cpu intensive actor
// supports: factorial 5! = 5 * 4 * 3 * 2 * 1 = 120

import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor.{type Next}
import gleam/regex
import gleam/string
import logging
import shared/build/packages/gleam_stdlib/src/gleam/result

pub type CalculatorActorMessage {
  // Subjects are generic over their message type, and `Result(String, Nil)` is the type
  // we want our public `solve` function to return.
  Solve(reply_to: Subject(Result(String, Nil)), calculation: String)
  Shutdown
}

pub fn is_calculation(content: String) -> Bool {
  // regex {number}! or {number}!!
  let assert Ok(input_pattern) = regex.from_string("^\\d+!+$")
  regex.check(input_pattern, content)
}

pub fn start() -> Subject(CalculatorActorMessage) {
  logging.log(logging.Info, "Starting calculator actor")

  let state = Nil
  let assert Ok(actor) = actor.start(state, handle_message)

  actor
}

fn handle_message(
  message: CalculatorActorMessage,
  _state: Nil,
) -> Next(CalculatorActorMessage, Nil) {
  case message {
    Shutdown -> actor.Stop(process.Normal)
    Solve(client, calculation) -> {
      let number =
        calculation
        |> string.split("!")
        |> list.first
        |> result.unwrap("")
        |> int.parse

      let result = case number {
        Ok(number) -> {
          let result = number |> factorial(1) |> int.to_string
          logging.log(logging.Info, "Calculation inner finished!")
          Ok(result)
        }
        _ -> {
          logging.log(logging.Error, "Invalid calculation")
          Error(Nil)
        }
      }

      process.send(client, result)
      actor.continue(Nil)
    }
  }
}

fn factorial(n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    _ -> factorial(n - 1, n * acc)
  }
}
