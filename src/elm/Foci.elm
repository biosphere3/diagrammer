module Foci exposing (..)

import Focus exposing (..)


globalTransform =
    Focus.create .globalTransform (\f r -> { r | globalTransform = f r.globalTransform })


translate =
    Focus.create .translate (\f r -> { r | translate = f r.translate })
