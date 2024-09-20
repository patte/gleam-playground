import gleam/int
import gleam/list
import gleam/string

import mist.{type Connection, type IpAddress, ConnectionInfo, get_client_info}

/// Prints the client URL as `ip:port`
pub fn get_client_host_port(conn: Connection) -> String {
  case get_client_info(conn) {
    Ok(ConnectionInfo(port, ip_address)) -> {
      let ip_str = ip_address_to_string(ip_address)
      string.concat([ip_str, ":", port |> int.to_string()])
    }
    _ -> "Invalid connection info"
  }
}

/// Converts IpAddress to a String
fn ip_address_to_string(ip: IpAddress) -> String {
  case ip {
    mist.IpV4(a, b, c, d) ->
      string.concat([
        a |> int.to_string(),
        ".",
        b |> int.to_string(),
        ".",
        c |> int.to_string(),
        ".",
        d |> int.to_string(),
      ])
    mist.IpV6(a, b, c, d, e, f, g, h) -> format_ipv6([a, b, c, d, e, f, g, h])
  }
}

/// Formats an IPv6 address into the standard colon notation
fn format_ipv6(segments: List(Int)) -> String {
  segments
  |> list.map(fn(segment) { segment |> int.to_base16() })
  |> string.join(":")
}
