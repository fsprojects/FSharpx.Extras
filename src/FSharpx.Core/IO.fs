namespace FSharpx

open System
open System.IO
open System.Text

module IO =
    let LinuxLineBreaks = "\n"
    let WindowsLineBreaks = "\r\n"
    let MacLineBreaks = "\r"

    /// Creates a DirectoryInfo for the given path
    let inline directoryInfo path = new DirectoryInfo(path)

    /// Creates a FileInfo for the given path
    let inline fileInfo path = new FileInfo(path)

    /// Creates a FileInfo or a DirectoryInfo for the given path
    let inline fileSystemInfo path : FileSystemInfo =
        if Directory.Exists path
            then upcast directoryInfo path
            else upcast fileInfo path

    /// Converts a file to it's full file system name
    let inline getFullName fileName = Path.GetFullPath fileName

    /// Gets all subdirectories
    let inline subDirectories (dir:DirectoryInfo) = dir.GetDirectories()

    /// Gets all files in the directory
    let inline filesInDir (dir:DirectoryInfo) = dir.GetFiles()

    /// Gets the current directory
    let currentDirectory = Path.GetFullPath "."

    /// Checks if the file exists on disk.
    let checkFileExists fileName =
        if not <| File.Exists fileName then failwithf "File %s does not exist." fileName

    /// Checks if all given files exists
    let allFilesExist files = Seq.forall File.Exists files

    /// Reads a file as one text
    let readFileAsString file = File.ReadAllText(file,Encoding.Default)

    /// Reads a file line by line
    let readFile (file:string) =   
        seq {use textReader = new StreamReader(file, Encoding.Default)
             while not textReader.EndOfStream do
                 yield textReader.ReadLine()}

    /// Writes a file line by line
    let writeToFile append fileName (lines: seq<string>) =    
        let fi = fileInfo fileName

        use writer = new StreamWriter(fileName,append && fi.Exists,Encoding.Default) 
        lines |> Seq.iter writer.WriteLine

    /// Writes a single string to a file
    let writeStringToFile append file text = writeToFile append file [text]

    /// Replaces the file with the given string
    let replaceFile fileName lines =
        let fi = fileInfo fileName
        if fi.Exists then
            fi.IsReadOnly <- false
            fi.Delete()
        writeToFile false fileName lines

    open Strings 

    /// Converts the given text from linux or mac linebreaks to windows line breaks
    let convertTextToWindowsLineBreaks text = 
        text
        |> replace WindowsLineBreaks LinuxLineBreaks 
        |> replace MacLineBreaks LinuxLineBreaks 
        |> replace LinuxLineBreaks WindowsLineBreaks

    /// Appends a text
    let inline append s (builder:StringBuilder) = builder.Append(sprintf "\"%s\" " s)

    /// Appends a text if the predicate is true
    let inline appendIfTrue p s builder = if p then append s builder else builder

    /// Appends a text if the predicate is false
    let inline appendIfFalse p = appendIfTrue (not p)

    /// Appends a text if the value is not null
    let inline appendIfNotNull value s = appendIfTrue (value <> null) (sprintf "%s%A" s value)

    /// Appends a text if the value is not null
    let inline appendStringIfValueIsNotNull value = appendIfTrue (value <> null)

    /// Appends a text if the value is not null or empty
    let inline appendStringIfValueIsNotNullOrEmpty value = appendIfTrue (isNullOrEmpty value |> not)

    /// Appends all notnull fileNames
    let inline appendFileNamesIfNotNull fileNames (builder:StringBuilder) =
        fileNames 
          |> Seq.fold (fun builder file -> appendIfTrue (isNullOrEmpty file |> not) file builder) builder

    /// The directory separator string. On most systems / or \
    let directorySeparator = Path.DirectorySeparatorChar.ToString()

    /// Combines two path strings
    let inline combinePaths path1 (path2:string) = Path.Combine(path1,path2.TrimStart [|'\\'|])