-- | Funny operators that are mere type aliases for the constructs in 'TypeSpec'
module Test.TypeSpecCrazy
      (
      -- * Crazy Type operators for 'It'
      type (--*)
      , type (~~~)
      , type (~~~~~~)
      , type (~~~~~~~~~)
      , type (~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      , type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
      -- * Crazy Type operators for 'TypeSpec'
      , type (###)
      , type (######)
      , type (#########)
      , type (############)
      , type (###############)
      , type (##################)
      , type (#####################)
      , type (########################)
      , type (###########################)
      , type (##############################)
      , type (#################################)
      , type (####################################)
      , type (#######################################)
      , type (##########################################)
      , type (#############################################)
      , type (################################################)
      , type (###################################################)
      , type (######################################################)
      , type (#########################################################)
      , type (############################################################)
      , type (###############################################################)
      , type (##################################################################)
      , type (#####################################################################)
      , type (########################################################################)

        -- * Grouping Aliases
      , type (-*)
      , type (-*-)

      , module ReExport)
    where

import Test.TypeSpec as ReExport

{-| Alias for 'It', note that the number of @~@s is alway a multiple of 3. This
provides the impression of an underlined title followed by other expectations.

It allows to write the following type:

>
> type ExpectationWithTitle =
>   TypeSpec (
>
>      "This is a title for some assertions:"
>      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
>
>      (2 + 2)  `Is`   4
>
>    )
 -}
type (--*) title expr = It title expr
type (~~~) title expr = It title expr
type (~~~~~~) title expr = It title expr
type (~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr = It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr
type (~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~) title expr =
  It title expr

infixr 3 --*
infixr 3 ~~~
infixr 3 ~~~~~~
infixr 3 ~~~~~~~~~
infixr 3 ~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{-| Create a 'TypeSpec' with an initial description or title followed by some
expectations. Note that the number of @#@s is alway a multiple of 3.

It allows to rewrite the example above in a shorter way:

>
> type ExpectationWithTitleShorter =
>
>   "This is a title for some assertions:"
>   ######################################
>
>   (2 + 2)  `Is`   4
>

-}
type (###) title expr = Explain title expr
type (######) title expr = Explain title expr
type (#########) title expr = Explain title expr
type (############) title expr = Explain title expr
type (###############) title expr = Explain title expr
type (##################) title expr = Explain title expr
type (#####################) title expr = Explain title expr
type (########################) title expr = Explain title expr
type (###########################) title expr = Explain title expr
type (##############################) title expr = Explain title expr
type (#################################) title expr = Explain title expr
type (####################################) title expr = Explain title expr
type (#######################################) title expr = Explain title expr
type (##########################################) title expr =
  Explain title expr
type (#############################################) title expr =
  Explain title expr
type (################################################) title expr =
  Explain title expr
type (###################################################) title expr =
  Explain title expr
type (######################################################) title expr =
  Explain title expr
type (#########################################################) title expr =
  Explain title expr
type (############################################################) title expr =
  Explain title expr
type (###############################################################) title expr =
  Explain title expr
type (##################################################################) title expr =
  Explain title expr
type (#####################################################################) title expr =
  Explain title expr
type (########################################################################) title expr =
  Explain title expr

infixr 1 ###
infixr 1 ######
infixr 1 #########
infixr 1 ############
infixr 1 ###############
infixr 1 ##################
infixr 1 #####################
infixr 1 ########################
infixr 1 ###########################
infixr 1 ##############################
infixr 1 #################################
infixr 1 ####################################
infixr 1 #######################################
infixr 1 ##########################################
infixr 1 #############################################
infixr 1 ################################################
infixr 1 ###################################################
infixr 1 ######################################################
infixr 1 #########################################################
infixr 1 ############################################################
infixr 1 ###############################################################
infixr 1 ##################################################################
infixr 1 #####################################################################
infixr 1 ########################################################################

{-| Crazy operator alias for '-/-' with higher precedence.

It allows to group expectations more beautiful than using type level lists.

> specCrazyMoreNested ::
>
>   "Title"
>   ######
>
>     "Top-level "
>     ~~~~~~~~~~~~
>
>          "Nested:"
>          ~~~~~~~~~
>          Int `Is` Int
>                             -*-
>          Int `Is` Int
>                             -*-
>          Int `Is` Int
>
>                             -/-
>
>     "Top-level "
>     ~~~~~~~~~~~~
>
>          "Nested:"
>          ~~~~~~~~~
>          Int `Is` Int
>                             -*-
>              "Nested:"
>              ~~~~~~~~~
>              Int `Is` Int
>                             -*-
>              Int `Is` Int
>
> specCrazyMoreNested = Valid

-}
type expectation1 -*- expectation2 = expectation1 -/- expectation2
infixr 3 -*-

-- | Crazy operator alias for '-/-'.
--
-- Make a list of expectations.  The precedence of this operator is even higher
-- than that of '-*-'.
type expectation1 -* expectation2 = expectation1 -/- expectation2
infixr 4 -*
