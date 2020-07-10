Changelog
=========

Version 0.2.2.0
---------------

*July 9, 2020*

<https://github.com/mstksg/mutable/releases/tag/v0.2.2.0>

*   Bugfix: *Generics* mechanisms for `MutBranch` for data types with more than
    two constructors fixed.

Version 0.2.1.0
---------------

*July 6, 2020*

<https://github.com/mstksg/mutable/releases/tag/v0.2.1.0>

*   Use TH to generate tuple instances for `Mutable` up to 12.
*   Use TH to generate `ListRefTuple` instances for lists up to length 12.
    Previously the instances up to the maximum length of `Mutable` tuple
    instances were missing.

Version 0.2.0.0
---------------

*July 5, 2020*

<https://github.com/mstksg/mutable/releases/tag/v0.2.0.0>

*   Demonadification: Revamp the typeclass system to be parameterized on the
    `s` `PrimState` state token, and not the monad itself.

Version 0.1.0.1
---------------

*February 11, 2020*

<https://github.com/mstksg/mutable/releases/tag/v0.1.0.1>

*   Update to work with *generic-lens-2.0.0.0* and *generic-lens-core-2.0.0.0*.

Version 0.1.0.0
---------------

*January 23, 2020*

<https://github.com/mstksg/mutable/releases/tag/v0.1.0.0>

*   Initial release
