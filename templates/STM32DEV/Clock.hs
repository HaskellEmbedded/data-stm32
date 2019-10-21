module {{ modns }} (
{{#clocks}}
{{#prefixRest}}{{/prefixRest}}clock{{ clockName }}
{{/clocks}}
  ) where

import Data.STM32.Clock

-- External clocks defined here use default values,
-- values for your board may differ
{{#clocks}}
clock{{ clockName }} :: ClockSource
clock{{ clockName }} = {{ clockName }} {{ clockHz }}

{{/clocks}}
