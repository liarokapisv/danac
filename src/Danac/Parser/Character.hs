module Danac.Parser.Character where

data EscapeSequence = ELineFeed                                                                                                                                                                                 
                    | ETab                                                                                                                                                                                      
                    | ECarriageReturn                                                                                                                                                                           
                    | EZero                                                                                                                                                                                     
                    | EBackslash                                                                                                                                                                                
                    | EQuote                                                                                                                                                                                    
                    | EDoubleQuote                                                                                                                                                                              
                    | EAsciiCode Int Int                                                                                                                                                                        
                    deriving (Eq, Show)                                                                                                                                                                         
                                                                                                                                                                                                                
data Character = CChar Char | CEscapeSequence EscapeSequence                                                                                                                                                    
    deriving (Show, Eq)   

