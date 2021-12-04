module Main

//Having a dummy entrypoint as the last file in the project prevents a compiler bug that leaves all let-bindings outside of tests Null.
//Grrreat!

[<EntryPoint>]
let main (args:string[]) = 0