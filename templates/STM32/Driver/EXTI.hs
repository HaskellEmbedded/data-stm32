{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module {{ modns }} (
    extiTower
  ) where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.Peripheral.EXTI

{{#versions}}
import qualified Ivory.BSP.STM32.Driver.EXTIv{{ version }} as V{{ version }}
{{/versions}}

extiTower :: EXTI
          -> [EXTIPin]
          -> Tower e [ChanOutput ('Stored IBool)]
extiTower exti ePins = case exti of
  {{#versions}}
  (WrappedV{{ version }} x) -> V{{ version }}.extiTower x ePins
  {{/versions}}
