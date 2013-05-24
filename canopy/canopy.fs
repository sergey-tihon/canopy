[<AutoOpen>]
module canopy.core

open OpenQA.Selenium.Firefox
open OpenQA.Selenium
open OpenQA.Selenium.Support.UI
open OpenQA.Selenium.Interactions
open Microsoft.FSharp.Core.Printf
open System.IO
open System
open browser
open configuration
open levenshtein
open reporters


let mutable canopyBrowser = CanopyBrowser(null);
let mutable (browser : IWebDriver) = null;
let mutable (failureMessage : string) = null
let mutable wipTest = false
let searchedFor() = 
    canopyBrowser.searchedFor

//browser
let firefox = "firefox"
let ie = "ie"
let chrome = "chrome"
  
let mutable browsers = []

//misc
let failsWith message = failureMessage <- message

let screenshot directory filename =
    canopyBrowser.screenshot directory filename
    
let js script = 
    canopyBrowser.js script

let sleep seconds =
    match box seconds with
    | :? int as i -> System.Threading.Thread.Sleep(i * 1000)
    | _ -> System.Threading.Thread.Sleep(1 * 1000)

let puts text = canopyBrowser.puts text

let highlight cssSelector = canopyBrowser.highlight cssSelector

let suggestOtherSelectors cssSelector = canopyBrowser.suggestOtherSelectors cssSelector

let describe (text : string) = puts text

let waitFor f = canopyBrowser.waitFor f

//get elements
let element cssSelector = canopyBrowser.element cssSelector

let elementWithin cssSelector elem = canopyBrowser.elementWithin cssSelector elem

let parent elem = canopyBrowser.parent elem

let elements cssSelector = canopyBrowser.elements cssSelector

let elementsWithin cssSelector elem = canopyBrowser.elementsWithin cssSelector elem

let someElement cssSelector = canopyBrowser.someElement cssSelector

let someElementWithin cssSelector elem = canopyBrowser.someElementWithin cssSelector elem

let someParent elem = canopyBrowser.someParent elem

let exists cssSelector = canopyBrowser.exists cssSelector

let nth index cssSelector = canopyBrowser.nth index cssSelector

let first cssSelector = canopyBrowser.first cssSelector

let last cssSelector = canopyBrowser.last cssSelector
   
//read/write
let (<<) cssSelector text = canopyBrowser.(<<) cssSelector text

let read cssSelector = canopyBrowser.read cssSelector

let clear cssSelector = canopyBrowser.clear cssSelector

//status
let selected cssSelector = canopyBrowser.selected cssSelector

let deselected cssSelector = canopyBrowser.deselected cssSelector

//keyboard
let tab = Keys.Tab
let enter = Keys.Enter
let down = Keys.Down
let up = Keys.Up
let left = Keys.Left
let right = Keys.Right

let press key = canopyBrowser.press key

//alerts
let alert() = canopyBrowser.alert()

let acceptAlert() = canopyBrowser.acceptAlert()

let dismissAlert() = canopyBrowser.dismissAlert()
    
//assertions    
let ( == ) item value = canopyBrowser.( == ) item value

let ( != ) cssSelector value = canopyBrowser.( != ) cssSelector value
        
let ( *= ) cssSelector value = canopyBrowser.( *= ) cssSelector value

let ( *!= ) cssSelector value = canopyBrowser.( *!= ) cssSelector value
    
let contains value1 value2 = canopyBrowser.contains value1 value2

let count cssSelector count = canopyBrowser.count cssSelector count

let elementsWithText cssSelector regex = canopyBrowser.elementsWithText cssSelector regex

let elementWithText cssSelector regex = canopyBrowser.elementWithText cssSelector regex

let ( =~ ) cssSelector pattern = canopyBrowser.( =~ ) cssSelector pattern

let ( *~ ) cssSelector pattern = canopyBrowser.( *~ ) cssSelector pattern

let is expected actual = canopyBrowser.is expected actual

let (===) expected actual = canopyBrowser.(===) expected actual

let displayed cssSelector = canopyBrowser.displayed cssSelector

let notDisplayed cssSelector = canopyBrowser.notDisplayed cssSelector

let fadedIn cssSelector = canopyBrowser.fadedIn cssSelector

//clicking/checking
let click item = canopyBrowser.click item

let doubleClick item = canopyBrowser.doubleClick item

let check cssSelector = canopyBrowser.check cssSelector

let uncheck cssSelector = canopyBrowser.uncheck cssSelector

//draggin
let (-->) cssSelectorA cssSelectorB = canopyBrowser.(-->) cssSelectorA cssSelectorB

let drag cssSelectorA cssSelectorB = canopyBrowser.drag cssSelectorA cssSelectorB
    
//browser related
let pin direction = canopyBrowser.pin direction

let start (b : string) =    
    //for chrome you need to download chromedriver.exe from http://code.google.com/p/chromedriver/wiki/GettingStarted
    //place chromedriver.exe in c:\ or you can place it in a customer location and change chromeDir value above
    //for ie you need to set Settings -> Advance -> Security Section -> Check-Allow active content to run files on My Computer*
    //also download IEDriverServer and place in c:\ or configure with ieDir
    //firefox just works
    match b with
    | "ie" -> browser <- new OpenQA.Selenium.IE.InternetExplorerDriver(ieDir) :> IWebDriver
    | "chrome" -> browser <- new OpenQA.Selenium.Chrome.ChromeDriver(chromeDir) :> IWebDriver
    | _ -> browser <- new OpenQA.Selenium.Firefox.FirefoxDriver() :> IWebDriver
    canopyBrowser <- CanopyBrowser(browser)
    if autoPinBrowserRightOnLaunch = true then canopyBrowser.pin Right
    browsers <- browsers @ [browser]

let switchTo browserObj = 
    match box browserObj with
    | :? OpenQA.Selenium.IWebDriver as b -> 
        browser <- b
        canopyBrowser <- CanopyBrowser(b)
    | :? CanopyBrowser as cb -> 
        browser <- cb.browser
        canopyBrowser <- cb
    | _ -> failwithf "Cannot switch to browser '%O'" browserObj


let tile (browsers : OpenQA.Selenium.IWebDriver list) =   
    let h = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Height;
    let w = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Width;
    let count = browsers.Length
    let maxWidth = w / count

    let rec setSize (browsers : OpenQA.Selenium.IWebDriver list) c =
        match browsers with
        | [] -> ()
        | b :: tail -> 
            b.Manage().Window.Size <- new System.Drawing.Size(maxWidth,h);        
            b.Manage().Window.Position <- new System.Drawing.Point((maxWidth * c),0);
            setSize tail (c + 1)
    
    setSize browsers 0

let quit browser =
    reporter.quit()
    match box browser with
    | :? OpenQA.Selenium.IWebDriver as b -> b.Quit()
    | :? CanopyBrowser as cb -> cb.quit()
    | _ -> browsers |> List.iter (fun b -> b.Quit())

let currentUrl() = canopyBrowser.currentUrl

let on u = canopyBrowser.on u

let ( !^ ) u = canopyBrowser.( !^ ) u

let url u = canopyBrowser.url u

let title() = canopyBrowser.title()

let reload() = canopyBrowser.reload()

let coverage url = canopyBrowser.coverage url