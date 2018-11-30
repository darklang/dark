var sha2 = require('sha2');

const handleErrors = function (error){
	var errorName = null;
  var errorMsg = null;
  var errorStr = null;

   try {
     // OCaml errors come in a few constructs - get the data we can
     try { errorName = error[1][1].c; } catch (_) {}
     try { errorMsg = error[2][1].c; } catch (_) {}
     try { errorMsg = error[2].c; } catch (_) {}
     errorStr = `${errorName} - ${errorMsg}`;
   } catch (_) {
   	return null;
   }
   return {
      name: errorName,
      msg: errorMsg,
      str: errorStr,
      obj: error
   }
}

self.onmessage = function(e) {
  var result = {
    analysis: null,
    error: null
  };

  try {
    result.analysis = darkAnalysis.performAnalysis(e.data.params);
  }
  catch (error) {
    try {
      console.log("Worker has analysis error", error);
      result.error = handleErrors(error)
    }
    catch (error) {
      console.log ("Error handling error", error);
    }
  }
  try {
    postMessage(result);
  } catch (error) {
    console.log ("Error posting result", error, result);
  }
}


module.exports = {
  sha2: sha2,
};
