
(** simulate an unbalanced coinflip, with odds of success equal to <chance> in interval [0.0 , 1.0] *)
let coinflip chance =
  (Random.float 1.0) <= chance

