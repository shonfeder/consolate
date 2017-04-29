module Reader = struct
  type t = Json_t.json
  let channel = Json_j.read_json
  let string  = Json_j.json_of_string
end

module Load = Io.Load (Reader)
