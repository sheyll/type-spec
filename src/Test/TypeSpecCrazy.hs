-- | Funny operators that are mere type aliases for the constructs in
-- 'TypeSpec'
module Test.TypeSpecCrazy
      ( module Test.TypeSpecCrazy
      , module ReExport)
    where

import Test.TypeSpec as ReExport

-- * Crazy Type operators

-- | Create a 'TypeSpec' with an initial description or title followed by some
-- expectations. Note that the number of @#@s is alway a multiple of 3.
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

infixr 0 ###
infixr 0 ######
infixr 0 #########
infixr 0 ############
infixr 0 ###############
infixr 0 ##################
infixr 0 #####################
infixr 0 ########################
infixr 0 ###########################
infixr 0 ##############################
infixr 0 #################################
infixr 0 ####################################
infixr 0 #######################################
infixr 0 ##########################################
infixr 0 #############################################
infixr 0 ################################################
infixr 0 ###################################################
infixr 0 ######################################################
infixr 0 #########################################################
infixr 0 ############################################################
infixr 0 ###############################################################
infixr 0 ##################################################################
infixr 0 #####################################################################
infixr 0 ########################################################################

-- | Specify an expectation using an underlined title using 'It'.
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

infixl 2 --*
infixl 2 ~~~
infixr 2 ~~~~~~
infixr 2 ~~~~~~~~~
infixr 2 ~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
infixr 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type expectation1 -*- expectation2 = expectation1 -/- expectation2
infixl 2 -*-
