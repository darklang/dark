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
let p0_5 = Single("p-0.5")
let p1 = Single("p-1")
let p1_25 = Single("p-1.25") // CUSTOM in tailwind.config.js
let p1_5 = Single("p-1.5")
let p2 = Single("p-2")
let p2_5 = Single("p-2.5")
let p3 = Single("p-3")
let p3_5 = Single("p-3.5")
let p4 = Single("p-4")
let p5 = Single("p-5")
let p6 = Single("p-6")
let p7 = Single("p-7")
let p8 = Single("p-8")
let p9 = Single("p-9")
let p10 = Single("p-10")

let px0 = Single("px-0")
let px0_5 = Single("px-0.5")
let px1 = Single("px-1")
let px1_25 = Single("px-1.25") // CUSTOM in tailwind.config.js
let px1_5 = Single("px-1.5")
let px2 = Single("px-2")
let px2_5 = Single("px-2.5")
let px3 = Single("px-3")
let px3_5 = Single("px-3.5")
let px4 = Single("px-4")
let px5 = Single("px-5")
let px6 = Single("px-6")
let px7 = Single("px-7")
let px8 = Single("px-8")
let px9 = Single("px-9")
let px10 = Single("px-10")

let pr0 = Single("pr-0")
let pr0_5 = Single("pr-0.5")
let pr1 = Single("pr-1")
let pr1_25 = Single("pr-1.25") // CUSTOM in tailwind.config.js
let pr1_5 = Single("pr-1.5")
let pr2 = Single("pr-2")
let pr2_5 = Single("pr-2.5")
let pr3 = Single("pr-3")
let pr3_5 = Single("pr-3.5")
let pr4 = Single("pr-4")
let pr5 = Single("pr-5")
let pr6 = Single("pr-6")
let pr7 = Single("pr-7")
let pr8 = Single("pr-8")
let pr9 = Single("pr-9")
let pr10 = Single("pr-10")

let pl0 = Single("pl-0")
let pl0_5 = Single("pl-0.5")
let pl1 = Single("pl-1")
let pl1_25 = Single("pl-1.25") // CUSTOM in tailwind.config.js
let pl1_5 = Single("pl-1.5")
let pl2 = Single("pl-2")
let pl2_5 = Single("pl-2.5")
let pl3 = Single("pl-3")
let pl3_5 = Single("pl-3.5")
let pl4 = Single("pl-4")
let pl5 = Single("pl-5")
let pl6 = Single("pl-6")
let pl7 = Single("pl-7")
let pl8 = Single("pl-8")
let pl9 = Single("pl-9")
let pl10 = Single("pl-10")

let py0 = Single("py-0")
let py0_5 = Single("py-0.5")
let py1 = Single("py-1")
let py1_25 = Single("py-1.25") // CUSTOM in tailwind.config.js
let py1_5 = Single("py-1.5")
let py2 = Single("py-2")
let py2_5 = Single("py-2.5")
let py3 = Single("py-3")
let py3_5 = Single("py-3.5")
let py4 = Single("py-4")
let py5 = Single("py-5")
let py6 = Single("py-6")
let py7 = Single("py-7")
let py8 = Single("py-8")
let py9 = Single("py-9")
let py10 = Single("py-10")

let pt0 = Single("pt-0")
let pt0_5 = Single("pt-0.5")
let pt1 = Single("pt-1")
let pt1_25 = Single("pt-1.25") // CUSTOM in tailwind.config.js
let pt1_5 = Single("pt-1.5")
let pt2 = Single("pt-2")
let pt2_5 = Single("pt-2.5")
let pt3 = Single("pt-3")
let pt3_5 = Single("pt-3.5")
let pt4 = Single("pt-4")
let pt5 = Single("pt-5")
let pt6 = Single("pt-6")
let pt7 = Single("pt-7")
let pt8 = Single("pt-8")
let pt9 = Single("pt-9")
let pt10 = Single("pt-10")

let pb0 = Single("pb-0")
let pb0_5 = Single("pb-0.5")
let pb1 = Single("pb-1")
let pb1_25 = Single("pb-1.25") // CUSTOM in tailwind.config.js
let pb1_5 = Single("pb-1.5")
let pb2 = Single("pb-2")
let pb2_5 = Single("pb-2.5")
let pb3 = Single("pb-3")
let pb3_5 = Single("pb-3.5")
let pb4 = Single("pb-4")
let pb5 = Single("pb-5")
let pb6 = Single("pb-6")
let pb7 = Single("pb-7")
let pb8 = Single("pb-8")
let pb9 = Single("pb-9")
let pb10 = Single("pb-10")

// margin
let m0 = Single("m-0")
let m0_5 = Single("m-0.5")
let m1 = Single("m-1")
let m1_25 = Single("m-1.25") // CUSTOM in tailwind.config.js
let m1_5 = Single("m-1.5")
let m2 = Single("m-2")
let m2_5 = Single("m-2.5")
let m3 = Single("m-3")
let m3_5 = Single("m-3.5")
let m4 = Single("m-4")
let m5 = Single("m-5")
let m6 = Single("m-6")
let m7 = Single("m-7")
let m8 = Single("m-8")
let m9 = Single("m-9")
let m10 = Single("m-10")

let mx0 = Single("mx-0")
let mx0_5 = Single("mx-0.5")
let mx1 = Single("mx-1")
let mx1_25 = Single("mx-1.25") // CUSTOM in tailwind.config.js
let mx1_5 = Single("mx-1.5")
let mx2 = Single("mx-2")
let mx2_5 = Single("mx-2.5")
let mx3 = Single("mx-3")
let mx3_5 = Single("mx-3.5")
let mx4 = Single("mx-4")
let mx5 = Single("mx-5")
let mx6 = Single("mx-6")
let mx7 = Single("mx-7")
let mx8 = Single("mx-8")
let mx9 = Single("mx-9")
let mx10 = Single("mx-10")

let mr0 = Single("mr-0")
let mr0_5 = Single("mr-0.5")
let mr1 = Single("mr-1")
let mr1_25 = Single("mr-1.25") // CUSTOM in tailwind.config.js
let mr1_5 = Single("mr-1.5")
let mr2 = Single("mr-2")
let mr2_5 = Single("mr-2.5")
let mr3 = Single("mr-3")
let mr3_5 = Single("mr-3.5")
let mr4 = Single("mr-4")
let mr5 = Single("mr-5")
let mr6 = Single("mr-6")
let mr7 = Single("mr-7")
let mr8 = Single("mr-8")
let mr9 = Single("mr-9")
let mr10 = Single("mr-10")

let ml0 = Single("ml-0")
let ml0_5 = Single("ml-0.5")
let ml1 = Single("ml-1")
let ml1_25 = Single("ml-1.25") // CUSTOM in tailwind.config.js
let ml1_5 = Single("ml-1.5")
let ml2 = Single("ml-2")
let ml2_5 = Single("ml-2.5")
let ml3 = Single("ml-3")
let ml3_5 = Single("ml-3.5")
let ml4 = Single("ml-4")
let ml5 = Single("ml-5")
let ml6 = Single("ml-6")
let ml7 = Single("ml-7")
let ml8 = Single("ml-8")
let ml9 = Single("ml-9")
let ml10 = Single("ml-10")

let my0 = Single("my-0")
let my0_5 = Single("my-0.5")
let my1 = Single("my-1")
let my1_25 = Single("my-1.25") // CUSTOM in tailwind.config.js
let my1_5 = Single("my-1.5")
let my2 = Single("my-2")
let my2_5 = Single("my-2.5")
let my3 = Single("my-3")
let my3_5 = Single("my-3.5")
let my4 = Single("my-4")
let my5 = Single("my-5")
let my6 = Single("my-6")
let my7 = Single("my-7")
let my8 = Single("my-8")
let my9 = Single("my-9")
let my10 = Single("my-10")

let mt0 = Single("mt-0")
let mt0_5 = Single("mt-0.5")
let mt1 = Single("mt-1")
let mt1_25 = Single("mt-1.25") // CUSTOM in tailwind.config.js
let mt1_5 = Single("mt-1.5")
let mt2 = Single("mt-2")
let mt2_5 = Single("mt-2.5")
let mt3 = Single("mt-3")
let mt3_5 = Single("mt-3.5")
let mt4 = Single("mt-4")
let mt5 = Single("mt-5")
let mt6 = Single("mt-6")
let mt7 = Single("mt-7")
let mt8 = Single("mt-8")
let mt9 = Single("mt-9")
let mt10 = Single("mt-10")

let mb0 = Single("mb-0")
let mb0_5 = Single("mb-0.5")
let mb1 = Single("mb-1")
let mb1_25 = Single("mb-1.25") // CUSTOM in tailwind.config.js
let mb1_5 = Single("mb-1.5")
let mb2 = Single("mb-2")
let mb2_5 = Single("mb-2.5")
let mb3 = Single("mb-3")
let mb3_5 = Single("mb-3.5")
let mb4 = Single("mb-4")
let mb5 = Single("mb-5")
let mb6 = Single("mb-6")
let mb7 = Single("mb-7")
let mb8 = Single("mb-8")
let mb9 = Single("mb-9")
let mb10 = Single("mb-10")

// -----------------
// Sizing
// -----------------

// width
let w6 = Single("w-6")
let w8 = Single("w-8")
let w12 = Single("w-12")

// height
let h0 = Single("h-0")
let h1 = Single("h-1")
let h2 = Single("h-2")
let h3 = Single("h-3")
let h4 = Single("h-4")
let h5 = Single("h-5")
let h6 = Single("h-6")
let h7 = Single("h-7")
let h8 = Single("h-8")
let h9 = Single("h-9")
let h10 = Single("h-10")
let h11 = Single("h-11")
let h12 = Single("h-12")

// font-size
let textXxs = Single("text-xxs")
let textXs = Single("text-xs")
let textSm = Single("text-sm")
let textBase = Single("text-base")
let textLg = Single("text-lg")
let textXl = Single("text-xl")
let textXxl = Single("text-2xl")
let textXxxl = Single("text-3xl")
let textXxxxl = Single("text-4xl")
let textXxxxxl = Single("text-5xl")
let textXxxxxxl = Single("text-6xl")
let textXxxxxxxl = Single("text-7xl")
let textXxxxxxxxl = Single("text-8xl")

//font-weight
let fontThin = Single("font-thin")
let fontExtralight = Single("font-extralight")
let fontLight = Single("font-light")
let fontNormal = Single("font-normal")
let fontMedium = Single("font-medium")
let fontSemibold = Single("font-semibold")
let fontBold = Single("font-bold")
let fontExtrabold = Single("font-extrabold")
let fontBlack = Single("font-black")

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
let bgGrey4 = Single("bg-grey4")
let bgGrey5 = Single("bg-grey5")
let bgGrey6 = Single("bg-grey6")
let bgGrey7 = Single("bg-grey7")
let bgGrey8 = Single("bg-grey8")
let bgGrey9 = Single("bg-grey9")

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

let hoverBgBlack1 = Single("hover:bgblack1")
let hoverBgBlack2 = Single("hover:bg-black2")
let hoverBgBlack3 = Single("hover:bg-black3")
let hoverBgGrey1 = Single("hover:bg-grey1")
let hoverBgGrey2 = Single("hover:bg-grey2")
let hoverBgGrey3 = Single("hover:bg-grey3")
let hoverBgGrey4 = Single("hover:bg-grey4")
let hoverBgGrey5 = Single("hover:bg-grey5")
let hoverBgGrey6 = Single("hover:bg-grey6")
let hoverBgGrey7 = Single("hover:bg-grey7")
let hoverBgGrey8 = Single("hover:bg-grey8")
let hoverBgGrey9 = Single("hover:bg-grey9")

let hoverBgWhite1 = Single("hover:bg-white1")
let hoverBgWhite2 = Single("hover:bg-white2")
let hoverBgWhite3 = Single("hover:bg-white3")
let hoverBgRed = Single("hover:bg-red")
let hoverBgOrange = Single("hover:bg-orange")
let hoverBgYellow = Single("hover:bg-yellow")
let hoverBgGreen = Single("hover:bg-green")
let hoverBgCyan = Single("hover:bg-cyan")
let hoverBgBlue = Single("hover:bg-blue")
let hoverBgPurple = Single("hover:bg-purple")
let hoverBgPink = Single("hover:bg-pink")
let hoverBgMagenta = Single("hover:bg-magenta")

let textBlack1 = Single("text-black1")
let textBlack2 = Single("text-black2")
let textBlack3 = Single("text-black3")
let textGrey1 = Single("text-grey1")
let textGrey2 = Single("text-grey2")
let textGrey3 = Single("text-grey3")
let textGrey4 = Single("text-grey4")
let textGrey5 = Single("text-grey5")
let textGrey6 = Single("text-grey6")
let textGrey7 = Single("text-grey7")
let textGrey8 = Single("text-grey8")
let textGrey9 = Single("text-grey9")
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
let borderGrey4 = Single("border-grey4")
let borderGrey5 = Single("border-grey5")
let borderGrey6 = Single("border-grey6")
let borderGrey7 = Single("border-grey7")
let borderGrey8 = Single("border-grey8")
let borderGrey9 = Single("border-grey9")
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

// -----------------
// cursors
// -----------------

let cursorAuto = Single("cursor-auto")
let cursorDefault = Single("cursor-default")
let cursorPointer = Single("cursor-pointer")
let cursorWait = Single("cursor-wait")
let cursorText = Single("cursor-text")
let cursorMove = Single("cursor-move")
let cursorNotAllowed = Single("cursor-not-allowed")
let cursorHelp = Single("cursor-help")

// -----------------
// vertical align
// -----------------

let alignBaseline = Single("align-baseline")
let alignTop = Single("align-top")
let alignMiddle = Single("align-middle")
let alignBottom = Single("align-bottom")
let alignTextTop = Single("align-text-top")
let alignTextBottom = Single("align-text-bottom")
