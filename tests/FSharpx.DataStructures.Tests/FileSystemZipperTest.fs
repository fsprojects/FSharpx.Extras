module FSharpx.DataStructures.Tests.FileSystemZipperTest

open System
open FSharpx
open FSharpx.DataStructures
open NUnit.Framework
open FsUnit

type FileName = string

type Folder = { Name: FileName; Items: FSItem list }
and FSItem =
    | File of FileName
    | Folder of Folder

let folder(name,items) = Folder { Name = name ; Items = items }

type Path = FileName * FSItem list * FSItem list

type FSZipper = { Focus : FSItem; Path : Path option }

/// Moves the zipper one directory up
let up (zipper:FSZipper) : FSZipper = 
   
    match zipper.Path with
    | Some (name,ls,rs) -> { zipper with Focus = Folder { Name = name; Items = ls @ [zipper.Focus] @ rs} }

let getName = function
    | File fileName -> fileName
    | Folder folder -> folder.Name  

let nameIs name item = getName item = name

/// Moves down to the folder with the given name
let moveTo name (zipper:FSZipper) : FSZipper = 
    match zipper.Focus with
    | Folder folder ->
        let (ls, item::rs) = List.split (nameIs name) folder.Items
        { Focus = item; Path = Some(folder.Name,ls,rs) }
 
/// Renames the given focus
let rename newName zipper =
    match zipper.Focus with
    | File name -> { zipper with Focus = File newName }
    | Folder folder -> { zipper with Focus = Folder { folder with Name = newName } }

let zipper fileSystem : FSZipper = { Focus = fileSystem; Path = None }

let disk =
    folder("root",
        [File "goat_yelling_like_man.wmv"
         File "pope_time.avi"
         folder("pics",
            [File "ape_throwing_up.jpg"
             File "watermelon_smash.gif"
             File "skull_man(scary).bmp"])
         File "dijon_poupon.doc"
         folder("programs",
            [File "fartwizard.exe"
             File "owl_bandit.dmg"
             File "not_a_virus.exe"
             folder("source code",
                [File "best_hs_prog.hs"
                 File "random.hs"])
            ])
        ])

[<Test>]
let ``Can move to subdir``() =       
    let z = disk |> zipper |> moveTo "pics" |> moveTo "skull_man(scary).bmp"  
    Assert.AreEqual(z.Focus,File "skull_man(scary).bmp")

[<Test>]
let ``Can move to subdir and up again``() =       
    let z = disk |> zipper |> moveTo "pics" |> moveTo "skull_man(scary).bmp" |> up
    Assert.AreEqual(getName z.Focus,"pics")

[<Test>]
let ``Can rename a folder``() =       
    let z = disk |> zipper |> moveTo "pics" |> rename "photo" |> up |> moveTo "photo"
    Assert.AreEqual(getName z.Focus,"photo")