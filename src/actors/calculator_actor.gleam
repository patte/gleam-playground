// an example of a cpu intensive actor
// supports: factorial 5! = 5 * 4 * 3 * 2 * 1 = 120

import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor.{type Next}
import gleam/regex
import gleam/result
import gleam/string
import logging

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
          let result = factorial(number) |> int.to_string
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

pub fn factorial(x: Int) -> Int {
  factorial_loop(x, 1)
}

fn factorial_loop(n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    1 -> acc
    _ -> factorial_loop(n - 1, n * acc)
  }
}
