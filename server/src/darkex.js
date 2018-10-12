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

module.exports = {
  sha2: sha2,
  handleAnalysisErrors: handleErrors
};
