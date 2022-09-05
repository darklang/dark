type rec t = Single(string) | Many(array<t>) | Classes(array<string>) | NoTw

// -----------------
// Styles
// -----------------

// For arbitrary HTML/CSS classes
let classNames = (classes: array<string>) => Classes(classes)

// If you want no style
let none = NoTw

let many = (ts: array<t>) => Many(ts)

let twProp = (ts: array<t>): Vdom.property<'msg> => {
  let rec flatten = (t: t): array<string> => {
    switch t {
    | Single(s) => [s]
    | NoTw => []
    | Classes(cs) => cs
    | Many(ts) => ts->Belt.Array.map(flatten)->Belt.Array.concatMany
    }
  }

  let toString = (ts: array<t>): string => {
    Many(ts)->flatten->Js.Array2.joinWith(" ")
  }

  Tea.Html.Attrs.class(toString(ts))
}

// -----------------
// Styles
// -----------------

// flex
let flex = Single("flex")
let flexCol = Single("flex-col")

// border
let border3 = Single("border-3") // CUSTOM in tailwind.config.js
let roundedFull = Single("rounded-full")
let borderSolid = Single("border-solid")
let borderColorb18bba = Single(`border-[#b18bba]`)

// padding
let p0 = Single("p-0")
let px0_5 = Single("px-0.5")
let px1_25 = Single("px-1.25") // CUSTOM in tailwind.config.js
let px2 = Single("px-2")

let py0_5 = Single("py-0.5")
let py1_25 = Single("py-1.25") // CUSTOM in tailwind.config.js

// margin
let m0 = Single("m-0")
let mx0_5 = Single("mx-0.5")
let mx1_25 = Single("mx-1.25") // CUSTOM in tailwind.config.js
let mx2 = Single("mx-2")

let my0_5 = Single("my-0.5")
let my1_25 = Single("my-1.25") // CUSTOM in tailwind.config.js

// -----------------
// Sizing
// -----------------

// width
let w6 = Single("w-6")
let w8 = Single("w-8")
let w12 = Single("w-12")

// height
let h6 = Single("h-6")
let h8 = Single("h-8")
let h12 = Single("h-12")

// -----------------
// Effects
// -----------------
let opacity50 = Single("opacity-50")
