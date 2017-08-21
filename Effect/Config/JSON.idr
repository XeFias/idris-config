-- ---------------------------------------------------------------- [ JSON.idr ]
-- Module    : JSON.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Effect.Config.JSON

import Effects
import Effect.File

import Effect.Config.Common

import public Config.Error
import public Config.JSON

export
readJSONConfig : String -> Eff (Either ConfigError JsonValue) [FILE ()]
readJSONConfig = readConfigFile jsonTokMap (Just stripWhiteSpace) json

-- --------------------------------------------------------------------- [ EOF ]
