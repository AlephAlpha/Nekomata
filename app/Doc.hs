module Doc where

import Nekomata.Builtin
import Nekomata.CodePage
import qualified Nekomata.Particle as Particle

-- | Generate documentation for Nekomata's code page.
docCodePage :: String
docCodePage =
    "# Code Page\n\n"
        ++ "Nekomata uses a custom code page, which contains 256 characters, "
        ++ "each representing a single byte.\n\n"
        ++ "The language is still in an early stage, so the code page is "
        ++ "incomplete. Unassigned characters are represented by `�`. "
        ++ "The code page is subject to change.\n\n"
        ++ codePageMarkdown

-- | Generate documentation for Nekomata's built-in functions and particles.
docBuiltins :: String
docBuiltins =
    "# Built-ins Functions and Particles\n\n"
        ++ "Nekomata is still in an early stage. The full names, short names, "
        ++ "and meanings of built-in functions and particles are subject "
        ++ "to change.\n\n"
        ++ "## Functions\n\n"
        ++ concat [infoMarkdown b | b <- builtins]
        ++ "## Particles\n\n"
        ++ concat [Particle.infoMarkdown p | p <- Particle.builtinParticles]
