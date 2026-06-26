# Expectations for code and environment

Preferences for coding style.

For any Python:
* use the local mamba 'py313' environment for development; do not propose to pip install anything into the base environment.
* need to generate complete documentation using Google-style docstrings for all significant functions and classes
* use Python's pathlib to other pathing solutions in Python
* use typing hints always
* generate clear tests for all significant functions
* prefer working in small, easily (human) digestible steps to complicated multistep approaches
* prefer Python fstrings to enable more concise and readable string formatting

For any code, regardless of language:
* prefer 'DRY' coding; move repeated code to generic modules when possible
* aim for consistent naming of objects between code bits
* prefer simple and explicit to clever and compact, always
* prefer longer, more expressive variable names to short, possibly confusing names
* shorter names are probably OK in a local (function) scope if they are simple indexes or similar
* generate clear tests for all significant functions
* prefer working in small, easily (human) digestible steps to complicated multistep approaches

For Fortran code:
* need to generate complete documentation using Doxygen Javadoc style for any functions, subroutines, or modules:
      /**
       * a normal member taking two arguments and returning an integer value.
       * @param a an integer argument.
       * @param s a constant character pointer.
       * @see Javadoc_Test()
       * @see ~Javadoc_Test()
       * @see testMeToo()
       * @see publicVar()
       * @return The test results
       */
