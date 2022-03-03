
(** simulate an unbalanced coinflip, with odds of success equal to {b chance} in interval [0.0 , 1.0]
  @param chance odds of success for the coinflip
*)
let coinflip chance =
  (Random.float 1.0) <= chance

