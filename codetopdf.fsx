#r "nuget: MAB.DotIgnore"
#r "nuget: ceTe.DynamicPDF.Converter.NET"
#r "nuget: ceTe.DynamicPDF.CoreSuite.NET"
#r "nuget: ceTe.DynamicPDF.HtmlConverter.NET"
#r "nuget: Microsoft.Playwright"
#r "nuget: Jering.Web.SyntaxHighlighters.HighlightJS"

open System
open System.IO
open System.Text
open System.Threading
open System.Text.Json
open System.Diagnostics
open System.Collections.Generic
open System.Security.Cryptography
open System.Text.RegularExpressions

open MAB.DotIgnore
open ceTe.DynamicPDF.Conversion
open Microsoft.Playwright
open Jering.Web.SyntaxHighlighters.HighlightJS

type ResultBuilder() =
    member inline _.Bind(x, f) = Result.bind f x
    member inline _.Return(x) = Ok(x)
    member inline _.ReturnFrom(x: Result<_, _>) = x

let result = ResultBuilder()

let highlight code language =
    StaticHighlightJSService.HighlightAsync(code, language).Result

let rec rmdir path attempts =
    try
        Directory.Delete(path, true)
    with :? IOException ->
        if attempts = 1 then
            reraise ()
        else
            Thread.Sleep(1000)
            rmdir path (attempts - 1)

let tryRmdir path attempts =
    try
        rmdir path attempts
        Ok()
    with (:? IOException | :? UnauthorizedAccessException) as ex ->
        Error ex.Message

let md5 (string: string) =
    use md5 = MD5.Create()
    let bytes = Encoding.Default.GetBytes(string)
    md5.ComputeHash(bytes) |> BitConverter.ToString |> _.Replace("-", "")

let generateFileId =
    let fileIdMap = Dictionary()

    fun filePath ->
        match fileIdMap.TryGetValue(filePath) with
        | true, id -> id
        | false, _ ->
            let value = $"file-{md5 filePath}"
            fileIdMap[filePath] <- value
            value

let execShellCmd entrypoint args =
    use proc =
        new Process(
            StartInfo =
                ProcessStartInfo(
                    FileName = entrypoint,
                    Arguments = args,
                    UseShellExecute = false,
                    CreateNoWindow = true
                )
        )

    proc.Start() |> ignore
    proc.WaitForExit()

let cloneRepo host repo =
    printfn "Cloning repository: %s/%s ..." host repo

    let tmpDir = Path.Combine(Directory.GetCurrentDirectory(), repo.Replace("/", "-"))

    let clone () =
        execShellCmd "git" $"clone https://{host}/{repo}.git {tmpDir}" |> ignore

    try
        if Path.Exists(tmpDir) then
            rmdir tmpDir 3

        clone ()
        Ok tmpDir
    with ex ->
        Error ex.Message

type Tree<'n, 't> =
    | Node of 'n * Tree<'n, 't> list
    | Tip of 't

let rec flatten tree =
    match tree with
    | Node(_, children) -> children |> Seq.collect flatten
    | Tip t -> Seq.singleton t

let rec generateDirectoryTrees excluded (dir: DirectoryInfo) =
    dir.EnumerateFileSystemInfos()
    |> Seq.choose (function
        | :? DirectoryInfo as subdir when not <| excluded subdir.Name true ->
            Some(generateDirectoryTree excluded subdir)
        | :? FileInfo as file when not <| excluded file.Name false -> Some(Tip file)
        | _ -> None)
    |> Seq.toList

and generateDirectoryTree excluded dir =
    Node(dir.Name, generateDirectoryTrees excluded dir)

let renderDirectoryTree maxFileSize (output: StringBuilder) trees =
    let rec recurse prefix trees =
        let count = List.length trees

        trees
        |> List.iteri (fun i tree ->
            let isLast = i = count - 1
            let connector = if isLast then "└── " else "├── "

            match tree with
            | Node(entry, children) ->
                output.Append($"{prefix}{connector}{entry}/\n") |> ignore
                recurse (prefix + (if isLast then "    " else "|    ")) children
            | Tip(fileInfo: FileInfo) ->
                let string =
                    if fileInfo.Length <= maxFileSize then
                        let fileId = generateFileId fileInfo.FullName
                        $"{prefix}{connector}<a href=\"#{fileId}\">{fileInfo.Name}</a>\n"
                    else
                        $"{prefix}{connector}%s{fileInfo.Name} (skip oversized file)\n"

                output.Append(string) |> ignore)

    recurse "" trees

let getLanguage (languageMap: IReadOnlyDictionary<string, string>) (ext: string) =
    languageMap.GetValueOrDefault(ext[1..].ToLowerInvariant(), "text")

let loadLanguageMap path =
    try
        use stream = File.OpenRead(path)

        JsonSerializer.Deserialize<Dictionary<string, string>>(stream).AsReadOnly()
        |> Ok
    with (:? IOException | :? JsonException) as ex ->
        Error ex.Message

let loadStylesheet path =
    try
        Ok(File.ReadAllText(path))
    with :? IOException as ex ->
        Error ex.Message

let loadIgnoreFile path =
    try
        path |> File.ReadAllLines |> IgnoreList |> Ok
    with :? IOException as ex ->
        Error ex.Message

let mkExcluded (ignorePath: string option) =
    match ignorePath with
    | Some path ->
        loadIgnoreFile path
        |> Result.map (fun ignore path isDirectory -> ignore.IsIgnored(path, isDirectory, null) || path = ".git")
    | None -> Ok(fun path _ -> path = ".git")

let generateHtml maxFileSize stylesheet excluded getLanguage dir title =
    let result = StringBuilder()

    let directoryTrees = generateDirectoryTrees excluded (DirectoryInfo(dir))

    result
        .Append("""<!DOCTYPE html><html><head><meta charset="UTF-8">""")
        .Append($"<style>{stylesheet}</style>")
        .Append("</head><body>")
        .Append($"<h1>{title}</h1>")
        .Append("""<h2>Directory Structure</h2><div class="directory-tree">""")
    |> ignore

    renderDirectoryTree maxFileSize result directoryTrees

    result.Append("</div>").Append("<h2>Contents</h2>") |> ignore

    directoryTrees
    |> Seq.collect flatten
    |> Seq.filter (fun file -> file.Length <= maxFileSize)
    |> Seq.iter (fun file ->
        use stream = file.OpenRead()
        use reader = new StreamReader(stream)
        let code = reader.ReadToEnd()
        let fileId = generateFileId file.FullName
        let language = getLanguage file.Extension
        let highlightedCode = highlight code language

        result
            .Append("""<div class="file-container">""")
            .Append($"""<h3 id="{fileId}">{file.FullName}</h3>""")
            .Append($"""<pre><code class="hljs language-{language}">{highlightedCode}</code></pre>""")
            .Append("</div>")
        |> ignore)

    result.Append("</body></html>") |> ignore
    string result

type Input =
    | Local of path: string
    | Remote of host: string * repo: string * removeDir: bool

type Output =
    | Html
    | Pdf
    | PdfUsingBrowser

type Args =
    { LanguageMapPath: string
      StylesheetPath: string
      IgnorePath: string option
      Input: Input
      Output: Output
      OutFile: string option
      MaxFileSize: int }

type Target =
    | GetHelp
    | Work of Args

type Reading =
    | ReadingLanguageMap
    | ReadingStylesheet
    | ReadingIgnore
    | ReadingOutFile
    | ReadingMaxFileSize

type ArgsBuilder =
    { Argv: string list
      Reading: Reading option
      LanguageMapPath: string option
      StylesheetPath: string option
      RemoveClonedRepo: bool
      IgnorePath: string option
      Input: Input option
      Output: Output option
      OutFile: string option
      MaxFileSize: int option }

let mkArgs builder : Result<Args, string> =
    let mk input =
        { LanguageMapPath = builder.LanguageMapPath |> Option.defaultValue "extmap.json"
          StylesheetPath = builder.StylesheetPath |> Option.defaultValue "styles.css"
          IgnorePath = builder.IgnorePath
          Input = input
          Output = builder.Output |> Option.defaultValue Pdf
          OutFile = builder.OutFile
          MaxFileSize = builder.MaxFileSize |> Option.defaultValue (1024 * 1000) }
        |> Ok

    match builder.Input with
    | None -> Error "expect input path"
    | Some(Local _) when builder.RemoveClonedRepo -> Error "-u must come with a github repo"
    | Some(Remote(host, repo, _)) -> mk (Remote(host, repo, builder.RemoveClonedRepo))
    | Some input -> mk input

let localOrRemote =
    let githubPatterns =
        [ @"^https://([a-zA-Z\d\.]+)/([^/]+\/[^/]+)$"
          @"^git@([a-zA-Z\d\.]+):([^/]+\/[^/]+)\.git$" ]
        |> List.map Regex

    fun string ->
        let rec iter (githubPatterns: Regex list) =
            match githubPatterns with
            | pattern :: rest ->
                let m = pattern.Match(string)

                if m.Success then
                    Remote(m.Groups[1].Value, m.Groups[2].Value.Replace(".git", ""), false)
                else
                    iter rest
            | [] -> Local string

        iter githubPatterns

let parseArgv argv =
    let (|Output|_|) arg =
        match arg with
        | "-html" -> Some Html
        | "-pdf" -> Some Pdf
        | _ -> None

    let rec iter builder : Result<Target, string> =
        match builder.Argv with
        | [] ->
            match builder.Reading with
            | Some _ -> Error "expect more arguments"
            | None -> mkArgs builder |> Result.map Work
        | arg :: rest ->
            let builder = { builder with Argv = rest }

            match builder.Reading with
            | Some reading ->
                let builder = { builder with Reading = None }

                match reading with
                | ReadingLanguageMap ->
                    iter
                        { builder with
                            LanguageMapPath = Some arg }
                | ReadingStylesheet ->
                    iter
                        { builder with
                            StylesheetPath = Some arg }
                | ReadingIgnore -> iter { builder with IgnorePath = Some arg }
                | ReadingOutFile -> iter { builder with OutFile = Some arg }
                | ReadingMaxFileSize ->
                    match Int32.TryParse(arg) with
                    | true, maxFileSize ->
                        iter
                            { builder with
                                MaxFileSize = Some(maxFileSize * 1024) }
                    | false, _ -> Error "max file size must be an integer"
            | None ->
                match arg with
                | Output format ->
                    match builder.Output with
                    | Some _ -> Error "output format specified twice"
                    | None -> iter { builder with Output = Some format }
                | "-map" ->
                    match builder.LanguageMapPath with
                    | Some _ -> Error "language map path specified twice"
                    | None ->
                        iter
                            { builder with
                                Reading = Some ReadingLanguageMap }
                | "-style" ->
                    match builder.StylesheetPath with
                    | Some _ -> Error "stylesheet path specified twice"
                    | None ->
                        iter
                            { builder with
                                Reading = Some ReadingStylesheet }
                | "-o" ->
                    match builder.OutFile with
                    | Some _ -> Error "output filename specified twice"
                    | None ->
                        iter
                            { builder with
                                Reading = Some ReadingOutFile }
                | "-u" ->
                    if builder.RemoveClonedRepo then
                        Error "duplicate -u option"
                    else
                        iter { builder with RemoveClonedRepo = true }
                | "-fsize" ->
                    match builder.MaxFileSize with
                    | Some _ -> Error "max file size specified twice"
                    | None ->
                        iter
                            { builder with
                                Reading = Some ReadingMaxFileSize }
                | "-b" ->
                    match builder.Output with
                    | Some Pdf
                    | None ->
                        iter
                            { builder with
                                Output = Some PdfUsingBrowser }
                    | Some Html -> Error "-b mustn't be used with -html"
                    | Some PdfUsingBrowser -> Error "duplicate -b option"
                | "-help" -> Ok GetHelp
                | _ ->
                    match builder.Input with
                    | Some _ -> Error "intput argument specified twice"
                    | None ->
                        iter
                            { builder with
                                Input = Some(localOrRemote arg) }

    { Argv = List.ofSeq argv
      Reading = None
      LanguageMapPath = None
      StylesheetPath = None
      RemoveClonedRepo = false
      IgnorePath = None
      Input = None
      Output = None
      OutFile = None
      MaxFileSize = None }
    |> iter

let findGitignore path =
    if not <| Directory.Exists(path) then
        Error $"path {path} does not exist"
    else
        let gitignorePath = Path.Combine(path, ".gitignore")

        if File.Exists(gitignorePath) then
            Ok(path, Some gitignorePath)
        else
            Ok(path, None)

// Returns .gitgnore path (if any)
let prepareDirectory input =
    match input with
    | Local path -> findGitignore path
    | Remote(host, repo, _) -> cloneRepo host repo |> Result.bind findGitignore

let mkTitle input =
    match input with
    | Local path -> $"Local directory: {path}"
    | Remote(host, repo, _) -> $"Remote: {host}/{repo}"

let save html outFile input output =
    let barePath =
        outFile
        |> Option.defaultWith (fun () ->
            match input with
            | Local path -> path
            | Remote(_, repo, _) -> repo.Replace("/", "-"))

    try
        match output with
        | Html -> File.WriteAllText(barePath + ".html", html)
        | Pdf ->
            let options = HtmlConversionOptions(false)
            let converter = HtmlConverter(html, options)
            converter.Convert(barePath + ".pdf")
        | PdfUsingBrowser ->
            File.WriteAllText(barePath + ".html", html)

            task {
                use! playwright = Playwright.CreateAsync()
                let! browser = playwright.Chromium.LaunchAsync(BrowserTypeLaunchOptions(Headless = true))
                let! page = browser.NewPageAsync()
                let! _ = page.GotoAsync($"""file:///{barePath}.html""")
                let! _ = page.PdfAsync(PagePdfOptions(Path = barePath + ".pdf", Format = "A4"))
                do! browser.CloseAsync()
                return ()
            }
            |> _.Result

            File.Delete(barePath + ".html")

        Ok()
    with ex ->
        Error ex.Message

let runResult =
    function
    | Ok() -> ()
    | Error message -> eprintfn "Error: %s" message

[<Literal>]
let help =
    """dotnet fsi codetopdf <local-path/remote-repo> [-h] [-u] [-html] [-pdf] [-b] [-map <path>] [-style <path>] [-ignore <path>] [-o <path-without-extension>] [-fsize <int>]
    -local-path: a path
    -github-repo: https://<host>/<owner>/<repo> / git@<host>:<owner>/<repo>.git
    -help: display this message. omit any other option
    -u: remove cloned repo after generation. used in conjunction with <remote-repo>
    -html: generate html output
    -pdf: generate pdf output, which is the default
    -b: generate pdf using a browser
    -map: specify a json file use to determine file's language from its extension. default to "extmap.json"
    -style: specify a css file injected into the output. default to "styles.css"
    -ignore: specify a .gitignore-like file to exclude unwanted files. default to directory's .gitignore (if it has)
    -fsize: specify the max file size (KB). default to 1000KB.
    -o: output the production file to path-without-extension + .html/.pdf"""

let work (args: Args) =
    result {
        let! languageMap = loadLanguageMap args.LanguageMapPath
        let! stylesheet = loadStylesheet args.StylesheetPath
        let! dir, gitignorePath = prepareDirectory args.Input
        let! excluded = args.IgnorePath |> Option.orElse gitignorePath |> mkExcluded

        let html =
            generateHtml args.MaxFileSize stylesheet excluded (getLanguage languageMap) dir (mkTitle args.Input)

        do! save html args.OutFile args.Input args.Output

        do!
            match args.Input with
            | Remote(_, _, true) -> tryRmdir dir 3
            | _ -> Ok()

        printfn "Ok"

        return ()
    }

do
    result {
        match! parseArgv (Environment.GetCommandLineArgs()[2..]) with // Ignore "fsi codetopdf.fsx".
        | Work args -> return! work args
        | GetHelp ->
            printfn "%s" help
            return ()
    }
    |> runResult
