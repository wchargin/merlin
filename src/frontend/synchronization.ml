let sync buf ~sync ~kind =
  begin match sync with
    | Protocol.Sync_none -> ()
    | Protocol.Sync_set s ->
      let lexer = Buffer.start_lexing buf in
      assert (Lexer.feed lexer s);
      Lexer.feed lexer "";
      Buffer.update buf lexer;
  end
