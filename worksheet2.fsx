// /// record type definition with fields Age, Name, Email
//     /// line breaks are typical, but could be all on one line
// type Person = {
//         Age: int; 
//         FirstName: string; 
//         FamilyName: string; 
//         Email: string
//     }
    
//     /// constructing a data item of Person type
// let tom = {
//     Age=61; 
//     FirstName="Thomas"; 
//     FamilyName="Clarke"; 
//     Email="tom@xx.co.uk"}
    
//     /// using copy and update to make a new item
// let tomsTwin = {tom with FirstName="Bob"; Email="bob@yy.co.uk"}
    
//     /// access fields of a record
// let toTuple p = p.Age, p.FirstName, p.FamilyName,p.Email
    
//     /// accessing old item in construction of new item
//     /// note that this creates a new record, it does not chnage the old one
// let ageUp p = {p with Age = p.Age + 1}
    
//     /// match on part of a record
// let printFamily = function
//     | {FamilyName="Clarke"} as p -> printfn "%s is in Tom's family" p.FirstName
//     | {FamilyName = "Smith"} as p -> printfn "%s is in the Smith family" p.FirstName
//     | {FirstName=name; FamilyName=nameF} -> printfn "This person is %s %s" name nameF

// let newEmail person s = {
//         person with Email = s}

// let newPerson = newEmail tom "askdjaksd@gmail.com"

type Person = { // record type
    Name: string
    Age: int
}
    
    
let testRecordDeconstruction() =
    let person = {Name="Mary"; Age=21} // sample record of Person type
        
    // use field selectors to deconstruct
        

    let {Name=x;Age=y} = person // pattern match to find x and y
    printf "Name is %s and age is %i" x y

// printfn "testingggg"
// printfn "The person's name is %s and email is %s" newPerson.FirstName newPerson.Email
    
let opt1 = Some 3
let opt2 = Some "how"
let opt3 = Some ("this is a tuple",3,'c')
let (opt4:Option<int>) = None

// let find_number lst_of_ints = 
//     let find_neg_number lst1 = List.tryFind ((>) 0) lst1
//     let find_pos_number lst2 = List.tryFind ((>) 0) lst2
//     let value = find_neg_number lst_of_ints
//     let value2 = find_pos_number lst_of_ints
//     Option.defaultValue (Option.orElse 0 (value|> Option.to ) value

let find_number lst = 
    List.tryFind((>) 0) lst
    |> Option.orElse (List.tryFind((<) 0) lst)
    |> Option.defaultValue 0

let tup1 = "Sean", "Bean"
let n = match tup1 with
        | fstName , sndName -> sprintf "%s's second name is %s" fstName sndName

let rec filter checkIfTrue lst = 
    match lst with  
    | hd::tl when checkIfTrue hd -> hd :: (filter checkIfTrue tl)
    | _::tl -> filter checkIfTrue tl
    | [] -> []

let sum = List.fold (+) 0

let res1 = sum [1;5;3]
let res2 = sum []

let factorial n = 
    [1..n]
    |> List.fold (*) 1

let reverseList lst =
    let revList f b = b::f
    ([], lst) ||> List.fold revList 

let trans = fun x -> [for i in x -> i * 10]

let list2 = [ [1; 2]; [3; 4]; [5; 6] ]

let subsetsWithSingleton subsets singleton = 
    List.allPairs subsets [[singleton];[]]
    |> List.map (fun(x,y) -> List.append x y)

let subsets lst =
    List.fold subsetsWithSingleton [[]] lst

let rec subsetsrec lst =
    match lst with
    | hd :: tl ->  
        List.allPairs [[hd];[]] (subsetsrec tl)
        |> List.map (fun (x,y) -> List.append x y)
    | [] -> [[]]

let subsetstail lst = 
    let rec subfunct l subs = 
        match l with
        | hd::tl ->
            List.allPairs subs [[hd];[]]
            |> List.map ((<||) List.append)
            |> subfunct tl
        | [] -> subs
    subfunct lst [[]]

let n2 = [1..5] 
let f = subsets n2

let nf5 = (0, ["a" ; "bc" ; "def"]) ||> List.fold (fun len str -> str.Length + len)

let countlen lst = 
    List.fold (fun n _ -> n+1) 0 lst


let subLists lst = 
    let folder lsts el = List.collect (fun lst -> [lst; el :: lst]) lsts
    ([[]], lst) ||> List.fold folder
    |> List.map List.rev

let x333 = subLists [1..2]
printf "%A" x333

let num = List.map trans list2
