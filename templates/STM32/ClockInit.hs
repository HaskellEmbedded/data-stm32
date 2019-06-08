{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module {{ modns }}
  ( init_clocks
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Family

{% for fam in fams %}
import qualified Ivory.BSP.STM32{{ fam }}.ClockInit as {{ fam }}
{% endfor %}

init_clocks :: Family -> ClockConfig -> Def('[]':->())
{% for fam in fams %}
init_clocks {{ fam }} = {{ fam }}.init_clocks
{% endfor %}

