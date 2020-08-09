module Ref = struct
  type 'a t = 'a ref = { mutable contents : 'a }

  let replace t ~f =
    t := f !t
end

let opt_merge ~f a b =
  match a, b with
  | Some a, Some b -> Some (f a b)
  | Some _, _ -> a
  | _, Some _ -> b
  | _, _ -> None
