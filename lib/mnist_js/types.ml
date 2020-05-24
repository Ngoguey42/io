type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

type db = uint8_ba * uint8_ba * uint8_ba * uint8_ba

type backend = [ `Tfjs_webgl | `Tfjs_cpu | `Tfjs_wasm ] [@@deriving enum]

type lr = [ `Down of float * float ]

type state =
  | Creating_network
  | Selecting_backend of { encoder : Fnn.network; decoder : Fnn.network; seed : int }
  | Creating_training of {
      encoder : Fnn.network;
      decoder : Fnn.network;
      seed : int;
      backend : backend;
      images_seen : int;
    }
  | Training of {
      encoder : Fnn.network;
      decoder : Fnn.network;
      seed : int;
      backend : backend;
      images_seen : int;
      config : Training_types.training_config;
    }

type event =
  | Network of { encoder : Fnn.network; decoder : Fnn.network; seed : int }
  | Backend of backend
  | Training_conf of Training_types.training_config
  | End of { encoder : Fnn.network; decoder : Fnn.network; images_seen : int }
  | Crash of exn
  | Abort
