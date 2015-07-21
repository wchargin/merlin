val sync :
  Buffer.t ->
  sync:Protocol.synchronization ->
  kind:[`None | `Cursor | `Full | `Any] ->
  unit
