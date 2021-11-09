module Format where

import Types
import Parse

{-
    Goal, parse md into:

    {
        "heaer": {
            "header":{
                "bullets": {
                    "text": "",
                    "text":
                },
                "header": {
                    "text": "",
                    "text": ""
                },

            }
        }
    }
-}

format :: Options -> FileContents -> FileContents
format opts file = file
