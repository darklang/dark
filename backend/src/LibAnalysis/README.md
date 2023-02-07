# LibAnalysis

LibAnalysis is the set of pure F# functionality that's used by the Analysis
webworker/Blazor. It's separated so that it can be tested directly in backend tests.


## Analysis

"Analysis" is the evaluation of Dark code from within the Editor itself, rather
than evaluating code against a backend server. This provides more immediate
feedback to Dark's users, while also reducing some demand from Dark's backend
servers.

LightTODO I just pasted these here together randomly, please review