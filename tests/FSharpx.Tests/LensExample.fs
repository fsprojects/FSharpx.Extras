module FSharpx.Tests.LensExample

// http://stackoverflow.com/questions/8179485/updating-nested-immutable-data-structures

open FSharpx
open FSharpx.Collections

type Monster = {
    Awake: bool
} with 
    static member awake =
        { Get = fun (x: Monster) -> x.Awake
          Set = fun v (x: Monster) -> { x with Awake = v } }

type Room = {
    Locked: bool
    Monsters: Monster list
} with
    static member locked = 
        { Get = fun (x: Room) -> x.Locked
          Set = fun v (x: Room) -> { x with Locked = v } }
    static member monsters = 
        { Get = fun (x: Room) -> x.Monsters
          Set = fun v (x: Room) -> { x with Monsters = v } }

type Level = {
    Illumination: int
    Rooms: Room list
} with
    static member illumination = 
        { Get = fun (x: Level) -> x.Illumination
          Set = fun v (x: Level) -> { x with Illumination = v } }
    static member rooms = 
        { Get = fun (x: Level) -> x.Rooms
          Set = fun v (x: Level) -> { x with Rooms = v } }
    
type Dungeon = {
    Levels: Level list
} with
    static member levels =
        { Get = fun (x: Dungeon) -> x.Levels 
          Set = fun v (x: Dungeon) -> { x with Levels = v } }
    static member print (d: Dungeon) = 
        d.Levels 
        |> List.iteri (fun i e -> 
            printfn "Level %d: Illumination %d" i e.Illumination
            e.Rooms |> List.iteri (fun i e ->
                let state = if e.Locked then "locked" else "unlocked"
                printfn "  Room %d is %s" i state
                e.Monsters |> List.iteri (fun i e ->
                    let state = if e.Awake then "awake" else "asleep"
                    printfn "    Monster %d is %s" i state)))

// generate test dungeon
let m1 = { Monster.Awake = true }
let m2 = { Monster.Awake = false }
let m3 = { Monster.Awake = true }
let m4 = { Monster.Awake = false }
let m5 = { Monster.Awake = true }
let m6 = { Monster.Awake = false }
let m7 = { Monster.Awake = true }
let m8 = { Monster.Awake = false }

let r1 = { Room.Locked = true;  Monsters = [m1; m2] }
let r2 = { Room.Locked = false; Monsters = [m3; m4] }
let r3 = { Room.Locked = true;  Monsters = [m5; m6] }
let r4 = { Room.Locked = false; Monsters = [m7; m8] }

let l1 = { Level.Illumination = 100; Rooms = [r1; r2] }
let l2 = { Level.Illumination = 50;  Rooms = [r3; r4] }

let dungeon = { Dungeon.Levels = [l1; l2] }

open FSharpx.Lens.Operators

Dungeon.print dungeon

let mapMonstersOnLevel nLevel f =
    Dungeon.levels >>| Lens.forList nLevel >>| Level.rooms >>| Lens.listMap Room.monsters
    |> Lens.update (f |> List.map |> List.map)

// toggle wake status of all monsters
let dungeon1 = dungeon |> mapMonstersOnLevel 0 (Monster.awake.Update not)

Dungeon.print dungeon1

// remove monsters that are asleep 
// which are in locked rooms on levels where illumination < 100 
// and unlock those rooms

let unlock = Room.locked.Set false
let removeAsleepMonsters = Room.monsters.Update (List.filter Monster.awake.Get)

let removeAsleepMonsters_unlock_rooms = List.mapIf Room.locked.Get (unlock >> removeAsleepMonsters)

let isLowIllumination = Level.illumination.Get >> ((>)100)
let removeAsleepMonsters_unlock_level = Level.rooms.Update removeAsleepMonsters_unlock_rooms
let removeAsleepMonsters_unlock_levels = List.mapIf isLowIllumination removeAsleepMonsters_unlock_level

let dungeon2 = dungeon |> Dungeon.levels.Update removeAsleepMonsters_unlock_levels
Dungeon.print dungeon2

open NUnit.Framework

[<Test>]
let dungeon1_validate() = 
    let level0 = dungeon1.Levels.[0]
    let monsters = level0.Rooms |> List.collect (fun x -> x.Monsters)
    Assert.False(monsters.[0].Awake)
    Assert.True (monsters.[1].Awake)

[<Test>]
let dungeon2_validate() = 
    let level1 = dungeon2.Levels.[1]
    let rooms = level1.Rooms
    for r in rooms do
        Assert.False r.Locked
    Assert.AreEqual(dungeon.Levels.[0], dungeon2.Levels.[0])
    let expectedLevel = 
        { Level.Illumination = 50
          Rooms = [ 
                    { Room.Locked = false; Monsters = [ { Monster.Awake = true } ] }
                    { Room.Locked = false; Monsters = [ { Monster.Awake = true }; { Monster.Awake = false } ] } 
                  ] }
    Assert.AreEqual(expectedLevel, level1)