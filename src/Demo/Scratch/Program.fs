// Scratch contains sketches for several new ideas and concepts extending FSharp.Data.Adaptive for reviewing purposes.
open System.Collections.Generic
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

type IComponent<'a> =
    abstract member ElementType : System.Type

type Pos =
    interface IComponent<float> with
        member x.ElementType = typeof<float>

type Vel =
    interface IComponent<float> with
        member x.ElementType = typeof<float>

type Memory() =
    let mutable mem : HashMap<System.Type, System.Array> = HashMap.empty
    let mutable slot = 0
    let mutable cap = 1024
    let mutable count = 0
    
    member x.Alloc(attributes : HashMap<System.Type, obj>) =
        let id = slot
        slot <- slot + 1
        if id >= cap then
            mem <- mem |> HashMap.map (fun _ v ->
               failwith "realloc"
            )
            
        for (k, value) in attributes do
            match HashMap.tryFind k mem with
            | Some arr -> arr.SetValue(value, id)
            | None ->
                let arr = System.Array.CreateInstance(value.GetType(), cap)
                arr.SetValue(value, id)
                mem <- HashMap.add k arr mem
            
        count <- count + 1
        id
        
    member x.Count = count
       
    member x.Get(name : System.Type) =
        mem.[name]

    member x.Get<'a, 'b when 'a :> IComponent<'b>>() =
        mem.[typeof<'a>] :?> array<'b>


let simulate (dt : float) (mem : Memory) =
    let pos = mem.Get<Pos, _>()
    let vel = mem.Get<Vel, _>()
    
    for i in 0 .. mem.Count - 1 do
        pos.[i] <- pos.[i] + vel.[i] * dt

let mem = Memory()

let o0 = 
    mem.Alloc (
        HashMap.ofList [
            typeof<Pos>, 0.0
            typeof<Vel>, 1.0
        ]
    )


let o1 = 
    mem.Alloc (
        HashMap.ofList [
            typeof<Pos>, 2.0
            typeof<Vel>, -1.0
        ]
    )

for i in 0 .. 10 do
    simulate 0.01 mem

printfn "%A" (mem.Get<Pos,_>())

//ABag.run()
//Observable.run()
//LookupAll.example()
//AListSub.run()
//Arr.run()