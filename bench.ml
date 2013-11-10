open Core.Std
open Core_bench.Std

let t1 =
  let d = Glicko2.bench_data () in
    Bench.Test.create ~name:"glicko2" (fun () ->  Glicko2.bench d)

let () = Command.run (Bench.make_command [t1])

