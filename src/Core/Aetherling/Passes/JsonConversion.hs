module Aetherling.Passes.JsonConversion where
import Data.Aeson
import Aetherling.Operations.AST
import Aetherling.Operations.Types

instance ToJSON PortThroughput where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON PortType where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TokenType where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON ComposeResult where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON FailureType where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON LineBufferData where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Op where
    toEncoding = genericToEncoding defaultOptions
