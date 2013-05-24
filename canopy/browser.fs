[<AutoOpen>]
module canopy.browser

open OpenQA.Selenium.Firefox
open OpenQA.Selenium
open OpenQA.Selenium.Support.UI
open OpenQA.Selenium.Interactions
open Microsoft.FSharp.Core.Printf
open System.IO
open System
open configuration
open levenshtein
open reporters

type CanopyException(message) = inherit Exception(message)
type CanopyReadOnlyException(message) = inherit CanopyException(message)
type CanopyOptionNotFoundException(message) = inherit CanopyException(message)
type CanopySelectionFailedExeception(message) = inherit CanopyException(message)
type CanopyDeselectionFailedException(message) = inherit CanopyException(message)
type CanopyWaitForException(message) = inherit CanopyException(message)
type CanopyElementNotFoundException(message) = inherit CanopyException(message)
type CanopyMoreThanOneElementFoundException(message) = inherit CanopyException(message)
type CanopyEqualityFailedException(message) = inherit CanopyException(message)
type CanopyNotEqualsFailedException(message) = inherit CanopyException(message)
type CanopyValueNotInListException(message) = inherit CanopyException(message)
type CanopyValueInListException(message) = inherit CanopyException(message)
type CanopyContainsFailedException(message) = inherit CanopyException(message)
type CanopyCountException(message) = inherit CanopyException(message)
type CanopyDisplayedFailedException(message) = inherit CanopyException(message)
type CanopyNotDisplayedFailedException(message) = inherit CanopyException(message)
type CanopyNotStringOrElementException(message) = inherit CanopyException(message)
type CanopyOnException(message) = inherit CanopyException(message)

//directions
type direction =
    | Left
    | Right

let sleep seconds =
    match box seconds with
    | :? int as i -> System.Threading.Thread.Sleep(i * 1000)
    | _ -> System.Threading.Thread.Sleep(1 * 1000)

type CanopyBrowser(browser:IWebDriver) =
    member val wipTest = false with get, set
    member val searchedFor = [] with get, set

    member this.browser = browser

    member this.screenshot directory filename =
        let pic = (browser :?> ITakesScreenshot).GetScreenshot().AsByteArray
        if not <| Directory.Exists(directory) 
            then Directory.CreateDirectory(directory) |> ignore
        IO.File.WriteAllBytes(Path.Combine(directory,filename + ".png"), pic)
        pic

    member this.js script = 
        (browser :?> IJavaScriptExecutor).ExecuteScript(script)

    member private this.swallowedJs script = 
        try this.js script |> ignore with | ex -> ()

    member this.puts (text : string) = 
        reporter.write text
        let escapedText = text.Replace("'", @"\'");
        let info = "
            var infoDiv = document.getElementById('canopy_info_div'); 
            if(!infoDiv) { infoDiv = document.createElement('div'); } 
            infoDiv.id = 'canopy_info_div'; 
            infoDiv.setAttribute('style','position: absolute; border: 1px solid black; bottom: 0px; right: 0px; margin: 3px; padding: 3px; background-color: white; z-index: 99999; font-size: 20px; font-family: monospace; font-weight: bold;'); 
            document.getElementsByTagName('body')[0].appendChild(infoDiv); 
            infoDiv.innerHTML = 'locating: " + escapedText + "';";
        this.swallowedJs info

    member private this.wait timeout f =
        let wait = new WebDriverWait(browser, TimeSpan.FromSeconds(timeout))
        wait.Until(fun _ -> (
                                try
                                    (f ()) = true
                                with
                                | :? CanopyException as ce -> raise(ce)
                                | _ -> false
                            )
                    ) |> ignore        
        ()

    member private this.colorizeAndSleep cssSelector =
        this.puts cssSelector
        this.swallowedJs <| sprintf "document.querySelector('%s').style.border = 'thick solid #FFF467';" cssSelector
        sleep wipSleep    
        this.swallowedJs <| sprintf "document.querySelector('%s').style.border = 'thick solid #ACD372';" cssSelector

    member this.highlight (cssSelector : string) = 
        this.swallowedJs <| sprintf "document.querySelector('%s').style.border = 'thick solid #ACD372';" cssSelector

    member this.suggestOtherSelectors (cssSelector : string) =
        if not disableSuggestOtherSelectors then
            let allElements = browser.FindElements(By.CssSelector("html *")) |> Array.ofSeq
            let classes = allElements |> Array.Parallel.map (fun e -> "." + e.GetAttribute("class"))
            let ids = allElements |> Array.Parallel.map (fun e -> "#" + e.GetAttribute("id"))
            Array.append classes ids 
            |> Seq.distinct |> List.ofSeq 
            |> remove "." |> remove "#" |> Array.ofList
            |> Array.Parallel.map (fun u -> levenshtein cssSelector u)
            |> Array.sortBy (fun r -> r.distance)
            |> Seq.take 5
            |> Seq.map (fun r -> r.selector) |> List.ofSeq
            |> (fun suggestions -> reporter.suggestSelectors cssSelector suggestions)    

    member this.waitFor (f : unit -> bool) =
        try        
            this.wait compareTimeout f
        with
        | :? WebDriverTimeoutException -> 
            this.puts "Condition not met in given amount of time. If you want to increase the time, put compareTimeout <- 10.0 anywhere before a test to increase the timeout"
            raise (CanopyWaitForException(sprintf "waitFor condition failed to become true in %.1f seconds" compareTimeout))

    //find related
    member private this.findByCss cssSelector f =
        try
            f(By.CssSelector(cssSelector)) |> List.ofSeq
        with | ex -> []

    member private this.findByXpath xpath f =
        try
            f(By.XPath(xpath)) |> List.ofSeq
        with | ex -> []

    member private this.findByLabel locator f =
        let isInputField (element : IWebElement) =
            element.TagName = "input" && element.GetAttribute("type") <> "hidden"
    
        let isField (element : IWebElement) =
            element.TagName = "select" || element.TagName = "textarea" || isInputField element

        let firstFollowingField (label : IWebElement) =
            let followingElements = label.FindElements(By.XPath("./following-sibling::*[1]")) |> Seq.toList
            match followingElements with
                | head :: tail when isField head-> [head]
                | _ -> []
        try
            let label = browser.FindElement(By.XPath(sprintf ".//label[text() = '%s']" locator))
            if (label = null) then
                []
            else
                match label.GetAttribute("for") with
                | null -> firstFollowingField label
                | id -> [f(By.Id(id))]
        with | _ -> []

    member private this.findByText text f =
        try
            let byValue = this.findByCss (sprintf "*[value='%s']" text) f |> List.ofSeq
            if byValue.Length > 0 then
                byValue
            else
                f(By.XPath(sprintf ".//*[text() = '%s']" text)) |> List.ofSeq
        with | _ -> []
    
    member private this.findElements (cssSelector : string) (searchContext : ISearchContext) =
        this.searchedFor <- (cssSelector, browser.Url) :: this.searchedFor
        let findInIFrame () =
            let iframes = this.findByCss "iframe" searchContext.FindElements
            if iframes.IsEmpty then 
                browser.SwitchTo().DefaultContent() |> ignore
                []
            else
                let webElements = ref []
                iframes |> List.iter (fun frame -> 
                    browser.SwitchTo().Frame(frame) |> ignore
                    webElements := this.findElements cssSelector searchContext
                )
                !webElements

        try
            seq {
                yield (this.findByCss    cssSelector searchContext.FindElements)
                yield (this.findByLabel  cssSelector searchContext.FindElement)
                yield (this.findByText   cssSelector searchContext.FindElements)
                yield (this.findByXpath  cssSelector searchContext.FindElements)
                yield (findInIFrame())
            }
            |> Seq.filter(fun list -> not(list.IsEmpty))
            |> Seq.head
        with | ex -> []

    member private this.findByFunction cssSelector (timeout : float) waitFunc (searchContext : ISearchContext) =
        if this.wipTest then this.colorizeAndSleep cssSelector
        let wait = new WebDriverWait(browser, TimeSpan.FromSeconds(elementTimeout))
        try
            wait.Until(fun _ -> waitFunc cssSelector searchContext)        
        with
            | :? WebDriverTimeoutException ->   
                this.puts "Element not found in the allotted time. If you want to increase the time, put elementTimeout <- 10.0 anywhere before a test to increase the timeout"
                this.suggestOtherSelectors cssSelector
                raise (CanopyElementNotFoundException(sprintf "cant find element %s" cssSelector))

    member private this.find (cssSelector : string) (timeout : float) (searchContext : ISearchContext) =
        (this.findByFunction cssSelector timeout this.findElements searchContext).Head

    member private this.findMany cssSelector timeout (searchContext : ISearchContext) =
        this.findByFunction cssSelector timeout this.findElements searchContext

    //get elements
    member private this.someElementFromList cssSelector elementsList =
        match elementsList with
        | [x] -> Some(x)
        | [] -> None
        | _ -> raise (CanopyMoreThanOneElementFoundException(sprintf "More than one element was selected when only one was expected for selector: %s" cssSelector))

    member this.element cssSelector = 
        this.find cssSelector elementTimeout browser

    member this.elementWithin cssSelector (elem:IWebElement) = 
        this.find cssSelector elementTimeout elem

    member this.parent elem = 
        elem |> this.elementWithin ".."

    member this.elements cssSelector = 
        this.findMany cssSelector elementTimeout browser

    member this.elementsWithin cssSelector elem = 
        this.findMany cssSelector elementTimeout elem

    member this.someElement cssSelector = 
        cssSelector |> this.elements |> this.someElementFromList cssSelector

    member this.someElementWithin cssSelector elem = 
        elem |> this.elementsWithin cssSelector |> this.someElementFromList cssSelector

    member this.someParent elem = 
        elem |> this.elementsWithin ".." |> this.someElementFromList "provided element"

    member this.exists cssSelector = 
        this.find cssSelector elementTimeout browser

    member this.nth index cssSelector = 
        List.nth (this.elements cssSelector) index

    member this.first cssSelector = 
        cssSelector |> this.elements |> List.head

    member this.last cssSelector = 
        cssSelector |> this.elements |> List.rev |> List.head

    //read/write
    member private this.writeToSelect cssSelector text =
        let elem = this.element cssSelector
        let options = Seq.toList (elem.FindElements(By.TagName("option")))
        let option = options |> List.filter (fun e -> e.Text = text)
        match option with
        | [] -> raise (CanopyOptionNotFoundException(sprintf "element %s does not contain value %s" cssSelector text))
        | head::tail -> head.Click()

    member this.(<<) cssSelector (text : string) = 
        this.wait (elementTimeout + 1.0) (fun _ ->
            let elems = this.elements cssSelector
            let writeToElement (e : IWebElement) =
                if e.TagName = "select" then
                    this.writeToSelect cssSelector text
                else
                    let readonly = e.GetAttribute("readonly")
                    if readonly = "true" then
                        raise (CanopyReadOnlyException(sprintf "element %s is marked as read only, you can not write to read only elements" cssSelector))
                    try e.Clear() with ex -> ex |> ignore
                    e.SendKeys(text)

            elems |> List.iter writeToElement
            true)

    member private this.textOf (element : IWebElement) =
        match element.TagName  with
        | "input" ->
            element.GetAttribute("value")
        | "select" ->
            let value = element.GetAttribute("value")
            let options = Seq.toList (element.FindElements(By.TagName("option")))
            let option = options |> List.filter (fun e -> e.GetAttribute("value") = value)
            option.Head.Text
        | _ ->
            element.Text    

    member this.read (cssSelector : string) =
        cssSelector |> this.element |> this.textOf
        
    member this.clear (cssSelector : string) = 
        let elem = this.element cssSelector
        let readonly = elem.GetAttribute("readonly")
        if readonly = "true" then
            raise (CanopyReadOnlyException(sprintf "element %s is marked as read only, you can not clear read only elements" cssSelector))
        elem.Clear()

    //status
    member this.selected (cssSelector : string) = 
        let elem = this.element cssSelector
        if not <| elem.Selected then
            raise (CanopySelectionFailedExeception(sprintf "element selected failed, %s not selected." cssSelector))

    member this.deselected (cssSelector : string) = 
        let elem = this.element cssSelector
        if elem.Selected then
            raise (CanopyDeselectionFailedException(sprintf "element deselected failed, %s selected." cssSelector))

    //keyboard
    member this.press key = 
        let elem = ((this.js "return document.activeElement;") :?> IWebElement)
        elem.SendKeys(key)

    //alerts
    member this.alert() = 
        browser.SwitchTo().Alert()

    member this.acceptAlert() = 
        browser.SwitchTo().Alert().Accept()

    member this.dismissAlert() = 
        browser.SwitchTo().Alert().Dismiss()

    //assertions    
    member this.(==) (item : 'a) value =
        match box item with
        | :? IAlert as alert -> 
            let text = alert.Text
            if text <> value then   
                alert.Dismiss()
                raise (CanopyEqualityFailedException(sprintf "equality check failed.  expected: %s, got: %s" value text))
        | :? string as cssSelector -> 
            let bestvalue = ref ""
            try
                this.wait compareTimeout (fun _ -> ( let readvalue = (this.read cssSelector)
                                                if readvalue <> value && readvalue <> "" then
                                                    bestvalue := readvalue
                                                    false
                                                else
                                                    readvalue = value))
            with
            | :? WebDriverTimeoutException -> raise (CanopyEqualityFailedException(sprintf "equality check failed.  expected: %s, got: %s" value !bestvalue))

        | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't check equality on %O because it is not a string or alert" item))

    member this.(!=) cssSelector value =
        try
            this.wait compareTimeout (fun _ -> (this.read cssSelector) <> value)
        with
        | :? WebDriverTimeoutException -> raise (CanopyNotEqualsFailedException(sprintf "not equals check failed.  expected NOT: %s, got: %s" value (this.read cssSelector)))
        
    member this.( *= ) (cssSelector : string) value =
        try        
            this.wait compareTimeout (fun _ -> ( cssSelector |> this.elements |> Seq.exists(fun element -> (this.textOf element) = value)))
        with
        | :? WebDriverTimeoutException -> 
            let sb = new System.Text.StringBuilder()
            cssSelector |> this.elements |> List.map this.textOf |> List.iter (bprintf sb "%s\r\n")
            raise (CanopyValueNotInListException(sprintf "cant find %s in list %s\r\ngot: %s" value cssSelector (sb.ToString())))

    member this.( *!= ) (cssSelector : string) value =
        try
            this.wait compareTimeout (fun _ -> ( cssSelector |> this.elements |> Seq.exists(fun element -> (this.textOf element) = value) |> not))
        with
        | :? WebDriverTimeoutException -> raise (CanopyValueInListException(sprintf "found %s in list %s, expected not to" value cssSelector))
    
    member this.contains (value1 : string) (value2 : string) =
        if (value2.Contains(value1) <> true) then
            raise (CanopyContainsFailedException(sprintf "contains check failed.  %s does not contain %s" value2 value1))

    member this.count cssSelector count =
        try        
            this.wait compareTimeout (fun _ -> ( cssSelector |> this.elements |> List.length = count))
        with
        | :? WebDriverTimeoutException -> raise (CanopyCountException(sprintf "count failed. expected: %i got: %i" count (this.elements cssSelector).Length));

    member private this.regexMatch pattern input = 
        System.Text.RegularExpressions.Regex.Match(input, pattern).Success

    member this.elementsWithText cssSelector regex =
        this.elements cssSelector
        |> List.filter (fun elem -> this.regexMatch regex (this.textOf elem))

    member this.elementWithText cssSelector regex = 
        (this.elementsWithText cssSelector regex).Head

    member this.( =~ ) cssSelector pattern =
        try
            this.wait compareTimeout (fun _ -> this.regexMatch pattern (this.read cssSelector))
        with
        | :? WebDriverTimeoutException -> raise (CanopyEqualityFailedException(sprintf "regex equality check failed.  expected: %s, got: %s" pattern (this.read cssSelector)))

    member this.( *~ ) (cssSelector : string) pattern =
        try        
            this.wait compareTimeout (fun _ -> ( cssSelector |> this.elements |> Seq.exists(fun element -> this.regexMatch pattern (this.textOf element))))
        with
        | :? WebDriverTimeoutException -> 
            let sb = new System.Text.StringBuilder()
            cssSelector |> this.elements |> List.map this.textOf |> List.iter (bprintf sb "%s\r\n")
            raise (CanopyValueNotInListException(sprintf "cant regex find %s in list %s\r\ngot: %s" pattern cssSelector (sb.ToString())))

    member this.is expected actual =
        if expected <> actual then
            raise (CanopyEqualityFailedException(sprintf "equality check failed.  expected: %O, got: %O" expected actual))

    member this.(===) expected actual = 
        this.is expected actual

    member private this.shown cssSelector =
        let elem = this.element cssSelector
        let opacity = elem.GetCssValue("opacity")
        let display = elem.GetCssValue("display")
        display <> "none" && opacity = "1"
       
    member this.displayed cssSelector =
        try
            this.wait compareTimeout (fun _ -> this.shown cssSelector)
        with
            | :? WebDriverTimeoutException -> raise (CanopyDisplayedFailedException(sprintf "display checked for %s failed." cssSelector))

    member this.notDisplayed cssSelector =
        try
            this.wait compareTimeout (fun _ -> not(this.shown cssSelector))
        with
            | :? WebDriverTimeoutException -> raise (CanopyNotDisplayedFailedException(sprintf "notDisplay checked for %s failed." cssSelector));

    member this.fadedIn cssSelector = 
        (fun _ -> this.shown cssSelector)

    //clicking/checking
    member this.click item =     
        match box item with
        | :? IWebElement as element -> element.Click()
        | :? string as cssSelector ->         
            this.wait elementTimeout (fun _ -> let elem = this.element cssSelector
                                               elem.Click()
                                               true)
        | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't click %O because it is not a string or webelement" item))
    
    member this.doubleClick item =
        let js = "var evt = document.createEvent('MouseEvents'); evt.initMouseEvent('dblclick',true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0,null); arguments[0].dispatchEvent(evt);"

        match box item with
        | :? IWebElement as elem -> (browser :?> IJavaScriptExecutor).ExecuteScript(js, elem) |> ignore
        | :? string as cssSelector ->         
            this.wait elementTimeout (fun _ -> ( let elem = this.element cssSelector
                                                 (browser :?> IJavaScriptExecutor).ExecuteScript(js, elem) |> ignore
                                                 true))
        | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't doubleClick %O because it is not a string or webelement" item))


    member this.check cssSelector = 
        if not <| (this.element cssSelector).Selected then this.click cssSelector

    member this.uncheck cssSelector = 
        if (this.element cssSelector).Selected then this.click cssSelector

    //draggin
    member this.(-->) cssSelectorA cssSelectorB =
        this.wait elementTimeout (fun _ ->
            let a = this.element cssSelectorA
            let b = this.element cssSelectorB
            (new Actions(browser)).DragAndDrop(a, b).Perform()
            true)

    member this.drag cssSelectorA cssSelectorB = 
        this.(-->) cssSelectorA cssSelectorB

    member this.pin direction =
        let h = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Height;
        let w = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Width;
        let maxWidth = w / 2    
        browser.Manage().Window.Size <- new System.Drawing.Size(maxWidth,h);        
        match direction with
        | Left -> browser.Manage().Window.Position <- new System.Drawing.Point((maxWidth * 0),0);   
        | Right -> browser.Manage().Window.Position <- new System.Drawing.Point((maxWidth * 1),0);   

    member this.currentUrl() = 
        browser.Url

    member this.on (u: string) = 
        try
            this.wait pageTimeout (fun _ -> (browser.Url.Contains(u)))
        with
            | ex -> raise (CanopyOnException(sprintf "on check failed, expected %s got %s" u browser.Url));

    member this.( !^ ) (u : string) = 
        browser.Navigate().GoToUrl(u)

    member this.url u = 
        this.(!^) u

    member this.title() = 
        browser.Title

    member this.reload = 
        this.currentUrl >> this.url

    member this.coverage url =
        let selectors = 
            this.searchedFor 
            |> List.filter(fun (c, u) -> u = url) 
            |> List.map(fun (cssSelector, u) -> cssSelector) 
            |> Seq.distinct 
            |> List.ofSeq
    
        let script cssSelector = 
            "var results = document.querySelectorAll('" + cssSelector + "'); \
            for (var i=0; i < results.length; i++){ \
                results[i].style.border = 'thick solid #ACD372'; \
            }"
    
        this.(!^) url
        this.on url
        selectors |> List.iter(fun cssSelector -> this.swallowedJs (script cssSelector))
        let p = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), @"\canopy\")
        let f = DateTime.Now.ToString("MMM-d_HH-mm-ss-fff")
        let ss = this.screenshot p f
        reporter.coverage url ss

    member this.quit() =
        browser.Quit()