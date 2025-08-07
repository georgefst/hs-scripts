-- TODO publish as a `miso-optics` library
module Util.MisoOptics where

import Miso
import Miso.Lens qualified
import Optics

(<-->) :: (Is k1 A_Lens, Is k2 A_Lens) => Optic' k1 is1 parent a -> Optic' k2 is2 model a -> Binding parent model
l1 <--> l2 = Miso.Lens.fromVL (toLensVL l1) Miso.<--> Miso.Lens.fromVL (toLensVL l2)

(-->) :: (Is k1 A_Getter, Is k2 A_Setter) => Optic' k1 is1 parent a -> Optic' k2 is2 model a -> Binding parent model
l1 --> l2 = (^. l1) Miso.--> (l2 .~)

(<--) :: (Is k1 A_Setter, Is k2 A_Getter) => Optic' k1 is1 parent a -> Optic' k2 is2 model a -> Binding parent model
l1 <-- l2 = (l1 .~) Miso.<-- (^. l2)
