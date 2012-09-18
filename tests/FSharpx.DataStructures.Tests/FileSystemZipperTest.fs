module FSharpx.DataStructures.Tests.FileSystemZipperTest

open System
open FSharpx
open FSharpx.DataStructures
open NUnit.Framework
open FsUnit

type FileName = string
type FSItem =
    | File of FileName
    | Folder of FileName * FSItem list

type Path = 
    | Top
    | Node of FileName * FSItem list * FSItem list

type FSZipper = FSItem * Path

/// Moves the zipper one directory up
let up (zipper:FSZipper) : FSZipper = 
    match zipper with
    | (item,(Node (name,ls,rs) as bs)) -> Folder(name,(ls @ [item] @ rs)), bs

let nameIs name = function
    | File fileName -> name = fileName
    | Folder(folderName,_) ->name = folderName  

let moveTo name (zipper:FSZipper) : FSZipper = 
    match zipper with
    | Folder(folderName,items),bs ->
        let (ls, item::rs) = List.split (nameIs name) items
        item,Node(folderName,ls,rs)
  
let zipper fileSystem : FSZipper = fileSystem,Top

let disk =
    Folder("root",
        [File "goat_yelling_like_man.wmv"
         File "pope_time.avi"
         Folder("pics",
            [File "ape_throwing_up.jpg"
             File "watermelon_smash.gif"
             File "skull_man(scary).bmp"])
         File "dijon_poupon.doc"
         Folder("programs",
            [File "fartwizard.exe"
             File "owl_bandit.dmg"
             File "not_a_virus.exe"
             Folder("source code",
                [File "best_hs_prog.hs"
                 File "random.hs"])
            ])
        ])

[<Test>]
let ``Can move to subdir``() =       
   let item,_ = disk |> zipper |> moveTo "pics" |> moveTo "skull_man(scary).bmp"  
   Assert.AreEqual(item,File "skull_man(scary).bmp")