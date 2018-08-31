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
                        (case Parser.run xml tag of
                            Result.Ok (returnedName,XmlInt 0) ->
                                Result.Ok (returnedName,XmlFloat 0)
                            r ->
                                r
                        )
            , fuzz stringTag "String data tag fuzzer" <|
                \(name,content,tag) ->
                    Expect.equal
                        (Result.Ok (name,content))
                        ( case Parser.run xml tag of
                            Result.Ok (nme,PresenceTag) -> Result.Ok (nme,XmlString "")
                            Result.Ok (nme,XmlInt n) -> Result.Ok <| (nme,XmlString <| String.fromInt n)
                            Result.Ok (nme,XmlFloat f) -> Result.Ok <| (nme,XmlString <| String.fromFloat f)
                            Result.Ok (nme,XmlString s) -> Result.Ok (nme,XmlString s)
                            otherwise -> otherwise
                        )
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
    subTagHelp 0

subTagHelp : Int -> Fuzzer (String,XmlTag,String)
subTagHelp depth =
    if depth > 0 then
        Fuzz.map2
            (\name l ->
                let 
                    childData = 
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
                    string = 
                        l   |> List.map (\(_,_,str) -> str)
                            |> List.foldr (++) ""
                            |> (\childString -> 
                                (openTagify name)
                                    ++childString
                                    ++(closeTagify name)
                                )
                in
                    (name,childData,string)
            )
            xmlName
            (Fuzz.oneOf 
                [ shortPresenceTag
                , longPresenceTag
                , intTag
                , floatTag
                , stringTag
                , subTagHelp <| depth-1
                ]
                |> Fuzz.list
            )
    else
        Fuzz.constant ("depthLimitStruck",PresenceTag,"<depthLimitStruck />")

shortPresenceTag : Fuzzer (String,XmlTag,String)
shortPresenceTag =
    Fuzz.map (\s -> (s,PresenceTag,"<"++s++" />")) xmlName

longPresenceTag : Fuzzer (String,XmlTag,String)
longPresenceTag =
    Fuzz.map (\s -> (s,PresenceTag,(openTagify s)++(closeTagify s))) xmlName

intTag : Fuzzer (String,XmlTag,String)
intTag =
    Fuzz.map2 
        (\s i -> (s,XmlInt i,(openTagify s)++(String.fromInt i)++(closeTagify s)))
        xmlName
        Fuzz.int

floatTag : Fuzzer (String,XmlTag,String)
floatTag =
    Fuzz.map2 
        (\s i -> (s,XmlFloat i,(openTagify s)++(String.fromFloat i)++(closeTagify s)))
        xmlName
        (Fuzz.constant 0.0)

stringTag : Fuzzer (String,XmlTag,String)
stringTag =
    Fuzz.map2
        (\s c -> 
            case String.toInt c of
            (s,XmlString c,(openTagify s)++c++(closeTagify s))
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
                            [ ("agiaug",[XmlInt 5])
                            , ("agimax",[XmlInt 5])
                            , ("agimin",[XmlInt 5])
                            , ("bodaug",[XmlInt 2])
                            , ("bodmax",[XmlInt 2])
                            , ("bodmin",[XmlInt 2])
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
                            , ("chaaug",[XmlInt 3])
                            , ("chamax",[XmlInt 3])
                            , ("chamin",[XmlInt 3])
                            , ("depaug",[XmlInt 6])
                            , ("depmax",[XmlInt 6])
                            , ("depmin",[XmlInt 0])
                            , ("edgaug",[XmlInt 4])
                            , ("edgmax",[XmlInt 4])
                            , ("edgmin",[XmlInt 4])
                            , ("essaug",[XmlInt 6])
                            , ("essmax",[XmlInt 6])
                            , ("essmin",[XmlInt 0])
                            , ("id",[XmlString "8958c476-e37e-4783-b6d9-54a0acfd846a"])
                            , ("iniaug",[XmlInt 14])
                            , ("inimax",[XmlInt 14])
                            , ("inimin",[XmlInt 2])
                            , ("intaug",[XmlInt 4])
                            , ("intmax",[XmlInt 4])
                            , ("intmin",[XmlInt 4])
                            , ("karma",[XmlInt 0])
                            , ("logaug",[XmlInt 1])
                            , ("logmax",[XmlInt 1])
                            , ("logmin",[XmlInt 1])
                            , ("magaug",[XmlInt 6])
                            , ("magmax",[XmlInt 6])
                            , ("magmin",[XmlInt 0])
                            , ("movement",[XmlString "10/30"])
                            , ("name",[XmlString "Arctic Fox"])
                            , ("page",[XmlInt 72])
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
                            , ("reaaug",[XmlInt 4])
                            , ("reamax",[XmlInt 4])
                            , ("reamin",[XmlInt 4])
                            , ("resaug",[XmlInt 6])
                            , ("resmax",[XmlInt 6])
                            , ("resmin",[XmlInt 0])
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
                            , ("straug",[XmlInt 2])
                            , ("strmax",[XmlInt 2])
                            , ("strmin",[XmlInt 2])
                            , ("wilaug",[XmlInt 3])
                            , ("wilmax",[XmlInt 3])
                            , ("wilmin",[XmlInt 3])
                            ]
                        )]
                    )]
                )]
              )
            , ("version",[XmlInt 0])
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