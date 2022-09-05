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
let itemsCenter = Single("items-center")

// positioning
let absolute = Single("absolute")
let relative = Single("relative")
let fixed = Single("fixed")

let top0 = Single("top-0")
let left0 = Single("left-0")
let right0 = Single("right-0")
let bottom0 = Single("bottom-0")

let top20 = Single("top-20")
let left20 = Single("left-20")
let right20 = Single("right-20")
let bottom20 = Single("bottom-20")

let inset20 = Single("inset-20")
let insetX0 = Single("inset-x-0")
let insetY0 = Single("inset-y-0")

let visible = Single("visible")
let invisible = Single("invisible")

// overflow
let overflowAuto = Single("overflow-auto")
let overflowHidden = Single("overflow-hidden")
let overflowScroll = Single("overflow-scroll")
let overflowVisible = Single("overflow-visible")
let overflowXAuto = Single("overflow-x-auto")
let overflowYAuto = Single("overflow-y-auto")
let overflowXHidden = Single("overflow-x-hidden")
let overflowYHidden = Single("overflow-y-hidden")
let overflowXScroll = Single("overflow-x-scroll")
let overflowYScroll = Single("overflow-y-scroll")
let overflowXVisible = Single("overflow-x-visible")
let overflowYVisible = Single("overflow-y-visible")

// border
let border3 = Single("border-3") // CUSTOM in tailwind.config.js

let roundedFull = Single("rounded-full")
let rounded = Single("rounded")
let roundedLg = Single("rounded-lg")
let roundedMd = Single("rounded-md")
let roundedSm = Single("rounded-sm")
let roundedXl = Single("rounded-xl")
let rounded2Xl = Single("rounded-2xl")
let rounded3Xl = Single("rounded-3xl")
let roundedBXl = Single("rounded-b-xl")
let roundedB2Xl = Single("rounded-b-2xl")
let roundedB3Xl = Single("rounded-b-3xl")
let roundedLXl = Single("rounded-l-xl")
let roundedL2Xl = Single("rounded-l-2xl")
let roundedL3Xl = Single("rounded-l-3xl")
let roundedRXl = Single("rounded-r-xl")
let roundedR2Xl = Single("rounded-r-2xl")
let roundedR3Xl = Single("rounded-r-3xl")
let roundedTFull = Single("rounded-t-full")
let roundedT = Single("rounded-t")
let roundedTLg = Single("rounded-t-lg")
let roundedTMd = Single("rounded-t-md")
let roundedTSm = Single("rounded-t-sm")
let roundedTXl = Single("rounded-t-xl")
let roundedT2Xl = Single("rounded-t-2xl")
let roundedT3Xl = Single("rounded-t-3xl")

let roundedBrXl = Single("rounded-br-xl")
let roundedBlXl = Single("rounded-bl-xl")
let roundedTrXl = Single("rounded-tr-xl")
let roundedTlXl = Single("rounded-tl-xl")
let roundedBr2Xl = Single("rounded-br-2xl")
let roundedBl2Xl = Single("rounded-bl-2xl")
let roundedTr2Xl = Single("rounded-tr-2xl")
let roundedTl2Xl = Single("rounded-tl-2xl")
let roundedBr3Xl = Single("rounded-br-3xl")
let roundedBl3Xl = Single("rounded-bl-3xl")
let roundedTr3Xl = Single("rounded-tr-3xl")
let roundedTl3Xl = Single("rounded-tl-3xl")

let borderSolid = Single("border-solid")

let borderColorb18bba = Single(`border-[#b18bba]`)

// padding
let p0 = Single("p-0")
let px0_5 = Single("px-0.5")
let px1_25 = Single("px-1.25") // CUSTOM in tailwind.config.js
let px2 = Single("px-2")
let px2_5 = Single("px-2.5")

let py0_5 = Single("py-0.5")
let py1_25 = Single("py-1.25") // CUSTOM in tailwind.config.js

// margin
let m0 = Single("m-0")
let mx0_5 = Single("mx-0.5")
let mx1_25 = Single("mx-1.25") // CUSTOM in tailwind.config.js
let mx2 = Single("mx-2")

let my0 = Single("my-0")
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

// font-size
let textXxs = Single("text-xxs")

// -----------------
// Effects
// -----------------
let opacity50 = Single("opacity-50")

// -----------------
// color
// -----------------
let bgBlack1 = Single("bg-black1")
let bgBlack2 = Single("bg-black2")
let bgBlack3 = Single("bg-black3")
let bgGrey1 = Single("bg-grey1")
let bgGrey2 = Single("bg-grey2")
let bgGrey3 = Single("bg-grey3")
let bgWhite1 = Single("bg-white1")
let bgWhite2 = Single("bg-white2")
let bgWhite3 = Single("bg-white3")
let bgRed = Single("bg-red")
let bgOrange = Single("bg-orange")
let bgYellow = Single("bg-yellow")
let bgGreen = Single("bg-green")
let bgCyan = Single("bg-cyan")
let bgBlue = Single("bg-blue")
let bgPurple = Single("bg-purple")
let bgPink = Single("bg-pink")
let bgMagenta = Single("bg-magenta")

let textBlack1 = Single("text-black1")
let textBlack2 = Single("text-black2")
let textBlack3 = Single("text-black3")
let textGrey1 = Single("text-grey1")
let textGrey2 = Single("text-grey2")
let textGrey3 = Single("text-grey3")
let textWhite1 = Single("text-white1")
let textWhite2 = Single("text-white2")
let textWhite3 = Single("text-white3")
let textRed = Single("text-red")
let textOrange = Single("text-orange")
let textYellow = Single("text-yellow")
let textGreen = Single("text-green")
let textCyan = Single("text-cyan")
let textBlue = Single("text-blue")
let textPurple = Single("text-purple")
let textPink = Single("text-pink")
let textMagenta = Single("text-magenta")

let borderBlack1 = Single("border-black1")
let borderBlack2 = Single("border-black2")
let borderBlack3 = Single("border-black3")
let borderGrey1 = Single("border-grey1")
let borderGrey2 = Single("border-grey2")
let borderGrey3 = Single("border-grey3")
let borderWhite1 = Single("border-white1")
let borderWhite2 = Single("border-white2")
let borderWhite3 = Single("border-white3")
let borderRed = Single("border-red")
let borderOrange = Single("border-orange")
let borderYellow = Single("border-yellow")
let borderGreen = Single("border-green")
let borderCyan = Single("border-cyan")
let borderBlue = Single("border-blue")
let borderPurple = Single("border-purple")
let borderPink = Single("border-pink")
let borderMagenta = Single("border-magenta")
