// Spec for User Tutorial : https://www.notion.so/darklang/Hello-World-Tutorial-9f0caa9890e74e47b5ac3e66ee826a4c

// open Prelude

type step = AppTypes.Tutorial.Step.t

let isTutorialCanvas = (~username: string, ~canvasname: string): bool =>
  canvasname == username ++ "-crud"

let generateCRUDContent: Tutorial.tooltipContent = {
  title: "This is an example of a fully working CRUD application. It enables you to create, delete or edit a blog post, store the content and metadata in a datastore and access the blog posts via API endpoint.",
  details: Some(list{
    "If you'd like to try building something complex, we've created a Getting Started tutorial in our documentation.",
  }),
  action: Some(
    "Getting Started tutorial",
    ToolTipMsg(OpenLink("https://docs.darklang.com/tutorials/first-dark-application")),
  ),
  align: Left,
  tipAlignment: "align-left",
  tooltipStyle: Crud,
}

let generateTutorialContent = (tutorialStep: step, username: string): Tutorial.tooltipContent =>
  switch tutorialStep {
  | Welcome => {
      title: "Welcome to Dark! Let's get started by creating a \"Hello World\" endpoint.",
      details: Some(list{
        "To continue, click anywhere on the canvas (the large light gray region in the center of the screen).      ",
        "Type \"hello\", then choose (arrow down and hit enter, or click on) \"New HTTP handler named /hello\".",
        "This will create a handler for the /hello endpoint of your app.",
      }),
      action: None,
      align: Left,
      tipAlignment: "align-left",
      tooltipStyle: Tutorial(tutorialStep),
    }
  | VerbChange => {
      title: "Select GET as the verb for your HTTP handler.",
      details: None,
      action: None,
      align: Right,
      tipAlignment: "align-top",
      tooltipStyle: Tutorial(tutorialStep),
    }
  | ReturnValue => {
      title: "In the return value (the small light gray box inside your HTTP handler), type \"Hello World\". Make sure to include the quotes!",
      details: None,
      action: None,
      align: Right,
      tipAlignment: "align-top",
      tooltipStyle: Tutorial(tutorialStep),
    }
  | OpenTab => {
      title: "Now let's test out the /hello endpoint. Click on the hamburger menu in the upper right of your HTTP handler and select \"Open in new tab\".",
      details: None,
      action: None,
      align: Right,
      tipAlignment: "align-top",
      tooltipStyle: Tutorial(tutorialStep),
    }
  | GettingStarted => {
      title: "Congratulations, you've created your first Hello World in Dark!",
      details: Some(list{"To help you continue to learn, we've created a sample CRUD app canvas."}),
      action: Some(
        "Open CRUD app canvas",
        ToolTipMsg(OpenLink("https://darklang.com/a/" ++ (username ++ "-crud"))),
      ),
      align: Right,
      tipAlignment: "align-top",
      tooltipStyle: Tutorial(tutorialStep),
    }
  }
