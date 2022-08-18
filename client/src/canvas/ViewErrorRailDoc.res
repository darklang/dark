open Prelude

// Dark
module E = FluidExpression
open PrettyDocs

@ocaml.doc(" [hintForFunction fn sendToRail] returns a (possibly noNode) DOM node that
 * provides a contextual hint about error-rail usage for the function [fn].
 *
 * The message in the node is customized based on the function return value
 * and the error rail status in [sendToRail].
 * The error rail status is an option because there are cases where the error rail
 * status has yet to be determined (eg autocomplete). Pass `None` for [sendToRail]
 * in such cases.
 ")
let hintForFunction = (
  fn: Prelude.function_,
  sendToRail: option<ProgramTypes.Expr.SendToRail.t>,
): Vdom.t<'a> => {
  let errorRail = Html.a(
    list{
      Attrs.class'("link"),
      Attrs.href("https://docs.darklang.com/discussion/error-handling#error-rail"),
      Attrs.target("_blank"),
    },
    list{Html.text("error rail")},
  )

  Option.unwrap(
    ~default=Html.noNode,
    switch sendToRail {
    | None =>
      // If we don't know if the function is on the rail, return a generic message:
      switch fn.fnReturnTipe {
      | TOption(_) =>
        Some(
          Html.p(
            list{},
            list{
              txt("By default, this function goes to the "),
              errorRail,
              txt(" on "),
              tag("code", list{txt("Nothing")}),
              txt(" and returns the unwrapped "),
              tag("var", list{txt("value")}),
              txt(" in "),
              tag("code", list{txt("Just "), tag("var", list{txt("value")})}),
              txt(" otherwise."),
            },
          ),
        )
      | TResult(_, _) =>
        Some(
          Html.p(
            list{},
            list{
              txt("By default, this function goes to the "),
              errorRail,
              txt(" on "),
              tag("code", list{txt("Error")}),
              txt(" and returns the unwrapped "),
              tag("var", list{txt("value")}),
              txt(" in "),
              tag("code", list{txt("Ok "), tag("var", list{txt("value")})}),
              txt(" otherwise."),
            },
          ),
        )
      | _ => None
      }
    | Some(sendToRail) =>
      // If we know if the function is on the rail, return a specific message:
      switch (fn.fnReturnTipe, sendToRail) {
      | (TOption(_), Rail) =>
        Some(
          Html.p(
            list{},
            list{
              txt("This function goes to the "),
              errorRail,
              txt(" on "),
              tag("code", list{txt("Nothing")}),
              txt(" and returns the unwrapped "),
              tag("var", list{txt("value")}),
              txt(" in "),
              tag("code", list{txt("Just "), tag("var", list{txt("value")})}),
              txt(" otherwise. Use the command "),
              tag("cmd", list{txt("take-function-off-rail")}),
              txt(" to handle the "),
              tag("code", list{txt("Nothing")}),
              txt(" case manually."),
            },
          ),
        )
      | (TOption(_), NoRail) =>
        Some(
          Html.p(
            list{},
            list{
              txt("This function is not on the "),
              errorRail,
              txt(", so you need to handle "),
              tag("code", list{txt("Just "), tag("var", list{txt("value")})}),
              txt(" and "),
              tag("code", list{txt("Nothing")}),
              txt(" manually. Alternatively, use the command "),
              tag("cmd", list{txt("put-function-on-rail")}),
              txt(" to let the error rail handle the result of this function."),
            },
          ),
        )
      | (TResult(_), Rail) =>
        Some(
          Html.p(
            list{},
            list{
              txt("This function goes to the "),
              errorRail,
              txt(" on "),
              tag("code", list{txt("Error _")}),
              txt(" and returns the unwrapped "),
              tag("var", list{txt("value")}),
              txt(" in "),
              tag("code", list{txt("Ok "), tag("var", list{txt("value")})}),
              txt(" otherwise. Use the command "),
              tag("cmd", list{txt("take-function-off-rail")}),
              txt(" to handle the "),
              tag("code", list{txt("Error "), tag("var", list{txt("errorMessage")})}),
              txt(" case."),
            },
          ),
        )
      | (TResult(_), NoRail) =>
        Some(
          Html.p(
            list{},
            list{
              txt("This function is not on the "),
              errorRail,
              txt(", so you need to handle "),
              tag("code", list{txt("Error "), tag("var", list{txt("errorMessage")})}),
              txt(" and "),
              tag("code", list{txt("Ok "), tag("var", list{txt("value")})}),
              txt(" manually. Alternatively, use "),
              tag("cmd", list{txt("put-function-on-rail")}),
              txt(" to let the ErrorRail handle the result of this function."),
            },
          ),
        )
      | (_, Rail | NoRail) => None
      }
    },
  )
}
