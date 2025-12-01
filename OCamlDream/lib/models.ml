open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type user = {
  id : int;
  username : string;
  email : string;
} [@@deriving yojson]

type create_user_request = {
  username : string;
  email : string;
} [@@deriving yojson]

type update_user_request = {
  username : string option; [@yojson.option]
} [@@deriving yojson]

type task = {
  id : int;
  user_id : int;
  title : string;
  description : string;
  done_ : bool; [@key "done"]
} [@@deriving yojson]

type create_task_request = {
  title : string;
  description : string;
} [@@deriving yojson]

type update_task_request = {
  title : string option; [@yojson.option]
  description : string option; [@yojson.option]
} [@@deriving yojson]

type patch_task_done_request = {
  done_ : bool; [@key "done"]
} [@@deriving yojson]

type error_response = {
  error : string;
} [@@deriving yojson]

type health_response = {
  status : string;
  version : string;
} [@@deriving yojson]
