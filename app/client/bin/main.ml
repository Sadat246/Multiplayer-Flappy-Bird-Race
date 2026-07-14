(** Browser entry point: mount the page shell into the [#app] div, start the
    imperative game loop (which finds the canvas by id once Bonsai has
    rendered it), and open the RPC connection back to the server that served
    this page. Compiled to [main.bc.js] by js_of_ocaml. *)

let () =
  Bonsai_web.Start.start Flappy_client.View.app;
  Flappy_client.Game_loop.start ();
  Flappy_client.Net.start ()
;;
