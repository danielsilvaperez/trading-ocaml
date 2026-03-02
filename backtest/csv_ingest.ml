open Trading_core

let split_csv_line line =
  line |> String.split_on_char ',' |> List.map String.trim

let load_bars ~instrument ~path =
  try
    let ic = open_in path in
    let rec loop acc first =
      match input_line ic with
      | exception End_of_file ->
          close_in ic;
          Ok (List.rev acc)
      | line ->
          if first then loop acc false
          else
            let cols = split_csv_line line in
            (match cols with
            | ts :: open_ :: high :: low :: close :: volume :: _ ->
                let ts =
                  match Time.parse_rfc3339 ts with
                  | Ok t -> t
                  | Error _ -> Types.now ()
                in
                let evt =
                  Types.Bar
                    {
                      instrument;
                      open_ = float_of_string open_;
                      high = float_of_string high;
                      low = float_of_string low;
                      close = float_of_string close;
                      volume = float_of_string volume;
                      timestamp = ts;
                    }
                in
                loop (evt :: acc) false
            | _ -> loop acc false)
    in
    loop [] true
  with
  | Sys_error msg -> Error (Types.Config_error msg)
  | Failure msg -> Error (Types.Invalid_input ("Invalid CSV numeric field: " ^ msg))
