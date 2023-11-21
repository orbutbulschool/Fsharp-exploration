open System


// Discriminated unions are a type specific to FSharp that can hold many completely different data types
type statusOfTasks =
    | NotStarted | Completed


// User defined task type, similar to classes in other languages
type Task = { Id: int; Description: string; Status: statusOfTasks }

// List with fake tasks, Value type inference
let mutable tasks= [
    { Id = 1; Description = "Finish homework"; Status = statusOfTasks.NotStarted }
    { Id = 2; Description = "Buy groceries"; Status = statusOfTasks.NotStarted }
    ]

// Explicit type statement, print specific task
let printTask (task: Task) =
    // Match statement similar to switch statement
    match task.Status with
    | statusOfTasks.Completed -> printfn "%d. [X] %s" task.Id task.Description
    | _ -> printfn "%d. [ ] %s" task.Id task.Description


let addTask description =
    let newTask = { Id = List.length tasks + 1; Description = description; Status = statusOfTasks.NotStarted }
    tasks <- tasks @ [newTask]
    printfn "Task added: %s" description

// Pattern matching for task completion, infered type
let completeTask taskId =
    // try to find a task in the list with an id matching the input of the list
    // Match is like a switch statemnt and the with is like the case
    match List.tryFind (fun task -> task.Id = taskId) tasks with
    // Some holds a type, but its undefined
    | Some task ->
        let updatedTask = { task with Status = statusOfTasks.Completed }
        // showing off hoe some values can be mutable in F#, assigns the task with the right id to the place in the list where it matched
        tasks <- List.map (fun t -> if t.Id = taskId then updatedTask else t) tasks
        printfn "Task %d marked as completed" taskId
    | None -> printfn "Task with id %d not found" taskId


// Main menu for the task manager
let rec mainLoop () =
    printfn "\n"
    printfn "Task Manager"
    printfn "1. View tasks"
    printfn "2. Add task"
    printfn "3. Complete task"
    printfn "4. Exit"
    printfn "Enter your choice:"

    // type inference, get option from input
    let choice = Console.ReadLine()

    match choice with
    // loop through lists with iter, and call the function each time
    | "1" ->
        tasks |> List.iter printTask
        mainLoop()
    // Create task components 
    | "2" ->
        printfn "Enter task description:"
        let description = Console.ReadLine()
        addTask description
        mainLoop ()

    // complete task
    | "3" ->
        printfn "Enter task id to mark as completed:"
        // Utilize type inference for taskId
        let taskId = Console.ReadLine() |> int
        completeTask taskId
        mainLoop ()
    // Finish program
    | "4" -> printfn "Exiting..."
    | _ ->
    
        printfn "Invalid choice. Please enter a valid option."
        

    printfn "\n"


// Attribute to make sure this method is the firs thing called.
let main argv =
    mainLoop ()
    0

main()