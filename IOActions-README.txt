Some of the files in this directory provide an (ad hoc) packaging
of Haskell prelude and standard library functions as "IO Actions".
These files are intended to be used as part of an introduction to
the use of Haskell's IO type.  For other applications, it would
likely make more sense just to use to original prelude functions
and or libraries directly.

The main files are as follows:

IOActions.lhs:
  A collection of standard IO Actions from the Haskell prelude.
  This file should load and be usable in both Hugs and GHCi
  without any special treatment.  (Do not be concerned if you load
  this file in GHCi and see a warning indicating that "Module
  'System.Cmd' is deprecated'; this allows the same source file to
  be loaded in both GHCi and Hugs.  If you only plan to use this
  file in GHCi, you can edit the source code to use System.Process
  instead, as described by comments in the file.)

IOExercises.lhs:
  A template with some simple IO actions exercises for you to try
  filling out ... I will likely share my solutions at some point,
  but I encourage you to try to recreate them for yourself before
  that!

WebActions.lhs:
  This file packages up some simple IO Actions downloading and
  manipulating pages and files from the web; it is intended
  primarily for demonstration purposes.  Again, if you wanted to
  use functions like this in a real application, it would likely
  make more sense to import the items that you need directly from
  the libraries where they are defined rather than using this
  module.

  The WebActions file relies on libraries that (I think) are only
  available for ghc/ghci.  So sorry, you probably won't be able to
  use it with Hugs.  Before you can load this file in GHCi, you
  will need to install at least the uri and download-curl
  libraries that it depends on.  (This will also install some
  additional libraries that uri and download-curl depend on.)  The
  following sequence of commands can be used to accomplish this
  (note that this may take some time to complete):

    cabal update
    cabal install uri --user
    cabal install download-curl --user

  I have tested this on a recent macOS release and on the
  departmental Linux machines.  The same commands, or something
  very similar, should also work on other Unix-based (and perhaps
  Windows-based) machines.  The "--user" flag indicates that the
  libraries should be installed in a subfolder of your home
  directory, so you do not require special system write access.

