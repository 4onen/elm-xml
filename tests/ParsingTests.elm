module ParsingTests exposing (..)

import Dict
import Parser exposing ((|.),(|=))

import Test exposing (Test,describe,test,fuzz)
import Expect
import Fuzz exposing (Fuzzer)

import Xml.Advanced exposing (..)

suite : Test
suite =
    describe "The Xml.Advanced module"
        [ describe "Tag types"
            [ fuzz shortPresenceTag "Short presence tag fuzzer" <|
                \(name,_,tag) ->
                    Expect.equal 
                        (Result.Ok (name,PresenceTag))
                        (Parser.run xml tag)
            , fuzz longPresenceTag "Long presence tag fuzzer" <|
                \(name,_,tag) ->
                    Expect.equal
                        (Result.Ok (name,PresenceTag))
                        (Parser.run xml tag)
            , fuzz intTag "Integer data tag fuzzer" <|
                \(name,int,tag) ->
                    Expect.equal
                        (Result.Ok (name,int))
                        (Parser.run xml tag)
            , fuzz floatTag "Floating-point data tag fuzzer" <|
                \(name,float,tag) ->
                    Expect.equal
                        (Result.Ok (name,float))
                        (Parser.run xml tag
                        )
            {--, Test.skip <| fuzz stringTag "String data tag fuzzer" <| -- stringTag presently breaks the Expect module by blowing the stack. FFS!
                \(name,content,tag) ->
                    Expect.equal
                        (Result.Ok (name,content))
                        ( case Parser.run xml tag of
                            Result.Ok (nme,PresenceTag) -> Result.Ok (nme,XmlString "")
                            Result.Ok (nme,XmlString s) -> Result.Ok (nme,XmlString s)
                            otherwise -> otherwise
                        )--}
            , fuzz subTag "Subtag fuzzer" <|
                \(name,subtagcontent,string) ->
                    Expect.equal
                        (Result.Ok (name,subtagcontent))
                        (Parser.run xml string)
            ]
        , test "Complete file parsing test" <| 
            \_ ->
                Expect.equal
                    chummer5crittersxmlparseresult
                    (Parser.run xmlFile chummer5crittersxmlsample)
        ]

subTag : Fuzzer (String,XmlTag,String)
subTag =
    subTagHelp 2

subTagHelp : Int -> Fuzzer (String,XmlTag,String)
subTagHelp depth =
    Fuzz.map2
        (\name l ->
            let 
                childData = 
                    if List.length l > 0 then 
                        l   |> List.foldl
                                (\(n,x,_) d ->
                                    Dict.update n 
                                        (\m ->
                                            case m of 
                                                Just list ->
                                                    Just (x::list)
                                                Nothing ->
                                                    Just <| List.singleton x
                                        ) d
                                ) Dict.empty
                            |> SubTags
                    else
                        PresenceTag
                string = 
                    if List.length l > 0 then
                        l   |> List.map (\(_,_,str) -> str)
                            |> List.foldr (++) ""
                            |> (\childString -> 
                                (openTagify name)
                                    ++childString
                                    ++(closeTagify name)
                                )
                    else
                        "<"++name++" />"
            in
                (name,childData,string)
        )
        xmlName
        (Fuzz.oneOf 
            [ shortPresenceTag
            , longPresenceTag
            , intTag
            , floatTag
            --, stringTag --stringTags presently break the Expect module, blowing the stack. FFS Elm! This is the thing you're supposed to fix!
            ,   if depth > 0 then
                    subTagHelp <| depth-1
                else
                    Fuzz.constant ("depthLimitStruck",PresenceTag,"<depthLimitStruck />")
            ]
            |> Fuzz.map
                (\l ->
                    case l of
                        (n,SubTags d,s) ->
                            if Dict.isEmpty d then
                                (n,PresenceTag,"<"++n++" />")
                            else
                                l
                                
                        _ ->
                            l
                )
            |> Fuzz.list
        )

{--
elm-test 0.19.0-beta4
---------------------

Running 1 test. To reproduce these results, run: elm-test --fuzz 100 --seed 1673012830

>>>>>>>: Ok ("QLcep9n7",SubTags (Dict.fromList []))
<<<<<<<: Ok ("QLcep9n7",PresenceTag)
↓ ParsingTests
↓ The Xml.Advanced module
↓ Tag types
✗ Subtag fuzzer

    This test failed because it threw an exception: "RangeError: Maximum call stack size exceeded"
--}



shortPresenceTag : Fuzzer (String,XmlTag,String)
shortPresenceTag =
    Fuzz.map (\s -> (s,PresenceTag,"<"++s++" />")) xmlName

longPresenceTag : Fuzzer (String,XmlTag,String)
longPresenceTag =
    Fuzz.map (\s -> (s,PresenceTag,(openTagify s)++(closeTagify s))) xmlName

intTag : Fuzzer (String,XmlTag,String)
intTag =
    Fuzz.map2 
        (\s i -> (s,XmlString <| String.fromInt i,(openTagify s)++(String.fromInt i)++(closeTagify s)))
        xmlName
        Fuzz.int

floatTag : Fuzzer (String,XmlTag,String)
floatTag =
    Fuzz.map2 
        (\s i -> (s,XmlString <| String.fromFloat i,(openTagify s)++(String.fromFloat i)++(closeTagify s)))
        xmlName
        (Fuzz.constant 0.0)

stringTag : Fuzzer (String,XmlTag,String)
stringTag =
    Fuzz.map2
        (\s c ->
            if String.length c > 0 then
                (s,XmlString c, (openTagify s) ++ c ++ (closeTagify s))
            else
                (s,PresenceTag, (openTagify s) ++ (closeTagify s))
        )
        xmlName
        (Fuzz.map 
              (String.trimLeft << String.filter (\c -> c /= '<' && c /= '>'))
              Fuzz.string
        )

xmlName : Fuzzer String
xmlName = 
    Fuzz.map2 (::) xmlNameFirstChar (Fuzz.list xmlNameInnerChar)
        |> Fuzz.map String.fromList
    
        
xmlNameFirstChar : Fuzzer Char
xmlNameFirstChar = 
    "abcdefghijklmnopqrstuvwxyz_"
        |> (\s -> s ++ (String.toUpper s))
        |> String.toList
        |> List.map Fuzz.constant
        |> Fuzz.oneOf

xmlNameInnerChar : Fuzzer Char
xmlNameInnerChar = 
    "abcdefghijklmnopqrstuvwxyz0123456789_-."
        |> (\s -> s ++ (String.toUpper s))
        |> String.toList
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


openTagify : String -> String
openTagify s = 
    "<"++s++">"

closeTagify : String -> String
closeTagify s =
    "</"++s++">"


--File samples


chummer5crittersxmlparseresult = 
    Result.Ok 
        (SubTags (Dict.fromList 
            [ ("categories",
                [ SubTags (Dict.fromList 
                    [ ("category",
                        [ XmlString "Toxic Spirits"
                        , XmlString "Toxic Critters"
                        , XmlString "Technocritters"
                        , XmlString "Sprites"
                        , XmlString "Spirits"
                        , XmlString "Shedim"
                        , XmlString "Shadow Spirits"
                        , XmlString "Protosapients"
                        , XmlString "Primordial Spirits"
                        , XmlString "Paranormal Critters"
                        , XmlString "Mutant Critters"
                        , XmlString "Mundane Critters"
                        , XmlString "Insect Spirits"
                        , XmlString "Infected"
                        , XmlString "Imps"
                        , XmlString "Harbingers"
                        , XmlString "Ghosts and Haunts"
                        , XmlString "Fey"
                        , XmlString "Entropic Sprites"
                        , XmlString "Dracoforms"
                        , XmlString "A.I.s"
                        ]
                      )
                    ])
                ]
              )
            , ("metatypes",
                [ SubTags (Dict.fromList 
                    [ ("metatype",
                        [SubTags (Dict.fromList 
                            [ ("agiaug",[XmlString "5"])
                            , ("agimin",[XmlString "5"])
                            , ("agimax",[XmlString "5"])
                            , ("bodaug",[XmlString "2"])
                            , ("bodmax",[XmlString "2"])
                            , ("bodmin",[XmlString "2"])
                            , ("bonus",
                                [SubTags (Dict.fromList 
                                    [ ("enabletab",
                                        [SubTags (Dict.fromList 
                                            [("name",[XmlString "critter"])]
                                        )]
                                      )
                                    ])
                                ]
                              )
                            , ("category",[XmlString "Mundane Critters"])
                            , ("chaaug",[XmlString "3"])
                            , ("chamax",[XmlString "3"])
                            , ("chamin",[XmlString "3"])
                            , ("depaug",[XmlString "6"])
                            , ("depmax",[XmlString "6"])
                            , ("depmin",[XmlString "0"])
                            , ("edgaug",[XmlString "4"])
                            , ("edgmax",[XmlString "4"])
                            , ("edgmin",[XmlString "4"])
                            , ("essaug",[XmlString "6"])
                            , ("essmax",[XmlString "6"])
                            , ("essmin",[XmlString "0"])
                            , ("id",[XmlString "8958c476-e37e-4783-b6d9-54a0acfd846a"])
                            , ("iniaug",[XmlString "14"])
                            , ("inimax",[XmlString "14"])
                            , ("inimin",[XmlString "2"])
                            , ("intaug",[XmlString "4"])
                            , ("intmax",[XmlString "4"])
                            , ("intmin",[XmlString "4"])
                            , ("karma",[XmlString "0"])
                            , ("logaug",[XmlString "1"])
                            , ("logmax",[XmlString "1"])
                            , ("logmin",[XmlString "1"])
                            , ("magaug",[XmlString "6"])
                            , ("magmax",[XmlString "6"])
                            , ("magmin",[XmlString "0"])
                            , ("movement",[XmlString "10/30"])
                            , ("name",[XmlString "Arctic Fox"])
                            , ("page",[XmlString "72"])
                            , ("powers",
                                [SubTags (Dict.fromList 
                                    [ ("power",
                                        [ XmlString "Natural Weapon"
                                        , XmlString "Enhanced Senses"
                                        ]
                                      )
                                    ]
                                )]
                              )
                            , ("reaaug",[XmlString "4"])
                            , ("reamax",[XmlString "4"])
                            , ("reamin",[XmlString "4"])
                            , ("resaug",[XmlString "6"])
                            , ("resmax",[XmlString "6"])
                            , ("resmin",[XmlString "0"])
                            , ("skills",
                                [SubTags (Dict.fromList 
                                    [ ("skill",
                                        [ XmlString "Unarmed Combat"
                                        , XmlString "Tracking"
                                        , XmlString "Perception"
                                        , XmlString "Infiltration"
                                        ]
                                      )
                                    ]
                                )]
                              )
                            , ("source",[XmlString "HP"])
                            , ("straug",[XmlString "2"])
                            , ("strmax",[XmlString "2"])
                            , ("strmin",[XmlString "2"])
                            , ("wilaug",[XmlString "3"])
                            , ("wilmax",[XmlString "3"])
                            , ("wilmin",[XmlString "3"])
                            ]
                        )]
                    )]
                )]
              )
            , ("version",[XmlString "0"])
            ]
        ))

chummer5crittersxmlsample = """<?xml version="1.0" encoding="utf-8"?>

<!--This file is part of Chummer5a.

    Chummer5a is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Chummer5a is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Chummer5a.  If not, see <http://www.gnu.org/licenses/>.

    You can obtain the full source code for Chummer5a at
    https://github.com/chummer5a/chummer5a
-->
<chummer xmlns="" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.w3.org/2001/XMLSchema critters.xsd">
  <version>0</version>
  <categories>
    <category>A.I.s</category>
    <category>Dracoforms</category>
    <category>Entropic Sprites</category>
    <category>Fey</category>
    <category>Ghosts and Haunts</category>
    <category>Harbingers</category>
    <category>Imps</category>
    <category>Infected</category>
    <category>Insect Spirits</category>
    <category>Mundane Critters</category>
    <category>Mutant Critters</category>
    <category>Paranormal Critters</category>
    <category>Primordial Spirits</category>
    <category>Protosapients</category>
    <category>Shadow Spirits</category>
    <category>Shedim</category>
    <category>Spirits</category>
    <category>Sprites</category>
    <category>Technocritters</category>
    <category>Toxic Critters</category>
    <category>Toxic Spirits</category>
  </categories>
  <metatypes>
    <!-- Region Mundane Critters -->
    <!-- Region Hazard Pay -->
    <metatype>
      <id>8958c476-e37e-4783-b6d9-54a0acfd846a</id>
      <name>Arctic Fox</name>
      <category>Mundane Critters</category>
      <karma>0</karma>
      <bodmin>2</bodmin>
      <bodmax>2</bodmax>
      <bodaug>2</bodaug>
      <agimin>5</agimin>
      <agimax>5</agimax>
      <agiaug>5</agiaug>
      <reamin>4</reamin>
      <reamax>4</reamax>
      <reaaug>4</reaaug>
      <strmin>2</strmin>
      <strmax>2</strmax>
      <straug>2</straug>
      <chamin>3</chamin>
      <chamax>3</chamax>
      <chaaug>3</chaaug>
      <intmin>4</intmin>
      <intmax>4</intmax>
      <intaug>4</intaug>
      <logmin>1</logmin>
      <logmax>1</logmax>
      <logaug>1</logaug>
      <wilmin>3</wilmin>
      <wilmax>3</wilmax>
      <wilaug>3</wilaug>
      <inimin>2</inimin>
      <inimax>14</inimax>
      <iniaug>14</iniaug>
      <edgmin>4</edgmin>
      <edgmax>4</edgmax>
      <edgaug>4</edgaug>
      <magmin>0</magmin>
      <magmax>6</magmax>
      <magaug>6</magaug>
      <resmin>0</resmin>
      <resmax>6</resmax>
      <resaug>6</resaug>
      <depmin>0</depmin>
      <depmax>6</depmax>
      <depaug>6</depaug>
      <essmin>0</essmin>
      <essmax>6</essmax>
      <essaug>6</essaug>
      <movement>10/30</movement>
      <bonus>
        <enabletab>
          <name>critter</name>
        </enabletab>
      </bonus>
      <powers>
        <power select="Smell, Hearing">Enhanced Senses</power>
        <power select="Claw/Bite: DV 2P, AP 0">Natural Weapon</power>
      </powers>
      <skills>
        <skill rating="2">Infiltration</skill>
        <skill rating="4">Perception</skill>
        <skill rating="3">Tracking</skill>
        <skill rating="3">Unarmed Combat</skill>
      </skills>
      <source>HP</source>
      <page>72</page>
    </metatype>
  </metatypes>
</chummer>
"""