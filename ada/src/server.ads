package Server is

  procedure Initialize(host: in String;
                       port: in Natural;
                       num_threads: in Positive);
  procedure Start;
  procedure Stop;
  procedure Send(message: in String);
end Server;
