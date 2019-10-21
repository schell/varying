change log
==========

0.1.5.0 - added Control.Varying.Spline

0.2.0.0 - reordered spline type variables for MonadTrans

0.3.0.0 - updated the type of mapOutput to a more friendly, usable signature
          bug fixes

0.3.1.0 - added stepMany, eitherE

0.4.0.0 - Var and Spline are now parameterized with Identity, removed mix, changed
          the behavior of race, added untilEvent variants, added tests

0.5.0.0 - changed stepMany to remove Monoid requirement, added raceMany, added
          anyE, more tests and SplineT obeys Applicative and Monad laws

0.5.0.1 - removed time as dependency

0.5.0.2 - separated tweening time and value, added runSplineE, builds on all GHC
          since 7.6

0.6.0.0 - changed the internal type of SplineT to use Either, reducing unused
          output values and preventing time/space leaks. Updated tween types.
          Added withTween(_).

0.7.0.0 - added proofs, reduced API size by removing trivial or weird (special)
          combinators, changed some names, Event is a synonym of Maybe, removed
      Time (moved functions to Event), renamed Event.mergeE to Event.bothE,
          added Spline.untilProc and Spline.whileProc, documentation - working
      towards 1.0

0.7.1.2 - Fixed broken ArrowLoop instance, updated documentation.

0.8.0.0 - TweenT is a newtype.

0.8.1.0 - Remove senseless ArrowApply instance
