
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
  if (value !== null && value !== undefined && value.repeat === undefined) {
    value.repeat = false;
  }
	var result = runHelp(decoder, value);
	if (result.tag === 'ok'){
		return _elm_lang$core$Result$Ok(result.value)
  } else {
    console.log("result.tag: " + result.tag);
    console.log("result.type: " + result.type);
    console.log("result.value: " + Object.keys(result.value));

		return _elm_lang$core$Result$Err("foo");
  }
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _elm_lang$dom$Native_Dom = function() {

var fakeNode = {
	addEventListener: function() {},
	removeEventListener: function() {}
};

var onDocument = on(typeof document !== 'undefined' ? document : fakeNode);
var onWindow = on(typeof window !== 'undefined' ? window : fakeNode);

function on(node)
{
	return function(eventName, decoder, toTask)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

			function performTask(event)
			{
				var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
				if (result.ctor === 'Ok')
				{
					_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
				}
			}

			node.addEventListener(eventName, performTask);

			return function()
			{
				node.removeEventListener(eventName, performTask);
			};
		});
	};
}

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		rAF(function()
		{
			var node = document.getElementById(id);
			if (node === null)
			{
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


// FOCUS

function focus(id)
{
	return withNode(id, function(node) {
		node.focus();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function blur(id)
{
	return withNode(id, function(node) {
		node.blur();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SCROLLING

function getScrollTop(id)
{
	return withNode(id, function(node) {
		return node.scrollTop;
	});
}

function setScrollTop(id, desiredScrollTop)
{
	return withNode(id, function(node) {
		node.scrollTop = desiredScrollTop;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toBottom(id)
{
	return withNode(id, function(node) {
		node.scrollTop = node.scrollHeight;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function getScrollLeft(id)
{
	return withNode(id, function(node) {
		return node.scrollLeft;
	});
}

function setScrollLeft(id, desiredScrollLeft)
{
	return withNode(id, function(node) {
		node.scrollLeft = desiredScrollLeft;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toRight(id)
{
	return withNode(id, function(node) {
		node.scrollLeft = node.scrollWidth;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SIZE

function width(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollWidth;
			case 'VisibleContent':
				return node.clientWidth;
			case 'VisibleContentWithBorders':
				return node.offsetWidth;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.right - rect.left;
		}
	});
}

function height(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollHeight;
			case 'VisibleContent':
				return node.clientHeight;
			case 'VisibleContentWithBorders':
				return node.offsetHeight;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.bottom - rect.top;
		}
	});
}

return {
	onDocument: F3(onDocument),
	onWindow: F3(onWindow),

	focus: focus,
	blur: blur,

	getScrollTop: getScrollTop,
	setScrollTop: F2(setScrollTop),
	getScrollLeft: getScrollLeft,
	setScrollLeft: F2(setScrollLeft),
	toBottom: toBottom,
	toRight: toRight,

	height: F2(height),
	width: F2(width)
};

}();

var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _elm_lang$keyboard$Keyboard$onSelfMsg = F3(
	function (router, _p0, state) {
		var _p1 = _p0;
		var _p2 = A2(_elm_lang$core$Dict$get, _p1.category, state);
		if (_p2.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p1.keyCode));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p3) {
					return _elm_lang$core$Task$succeed(state);
				},
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p2._0.taggers)));
		}
	});
var _elm_lang$keyboard$Keyboard_ops = _elm_lang$keyboard$Keyboard_ops || {};
_elm_lang$keyboard$Keyboard_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p4) {
				return task2;
			},
			task1);
	});
var _elm_lang$keyboard$Keyboard$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$keyboard$Keyboard$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p5 = maybeValues;
		if (_p5.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p5._0});
		}
	});
var _elm_lang$keyboard$Keyboard$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p6 = subs;
			if (_p6.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p6._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p6._0._0,
					_elm_lang$keyboard$Keyboard$categorizeHelpHelp(_p6._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$keyboard$Keyboard$categorize = function (subs) {
	return A2(_elm_lang$keyboard$Keyboard$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$keyboard$Keyboard$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$keyboard$Keyboard$subscription = _elm_lang$core$Native_Platform.leaf('Keyboard');
var _elm_lang$keyboard$Keyboard$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$keyboard$Keyboard$Msg = F2(
	function (a, b) {
		return {category: a, keyCode: b};
	});
var _elm_lang$keyboard$Keyboard$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(
								A3(
									_elm_lang$dom$Dom_LowLevel$onDocument,
									category,
									_elm_lang$keyboard$Keyboard$keyCode,
									function (_p7) {
										return A2(
											_elm_lang$core$Platform$sendToSelf,
											router,
											A2(_elm_lang$keyboard$Keyboard$Msg, category, _p7));
									})));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p8, taggers, task) {
				var _p9 = _p8;
				return A2(
					_elm_lang$core$Task$map,
					A2(
						_elm_lang$core$Dict$insert,
						category,
						A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, _p9.pid)),
					task);
			});
		var leftStep = F3(
			function (category, _p10, task) {
				var _p11 = _p10;
				return A2(
					_elm_lang$keyboard$Keyboard_ops['&>'],
					_elm_lang$core$Process$kill(_p11.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$keyboard$Keyboard$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$keyboard$Keyboard$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$keyboard$Keyboard$presses = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keypress', tagger));
};
var _elm_lang$keyboard$Keyboard$downs = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keydown', tagger));
};
var _elm_lang$keyboard$Keyboard$ups = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keyup', tagger));
};
var _elm_lang$keyboard$Keyboard$subMap = F2(
	function (func, _p12) {
		var _p13 = _p12;
		return A2(
			_elm_lang$keyboard$Keyboard$MySub,
			_p13._0,
			function (_p14) {
				return func(
					_p13._1(_p14));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Keyboard'] = {pkg: 'elm-lang/keyboard', init: _elm_lang$keyboard$Keyboard$init, onEffects: _elm_lang$keyboard$Keyboard$onEffects, onSelfMsg: _elm_lang$keyboard$Keyboard$onSelfMsg, tag: 'sub', subMap: _elm_lang$keyboard$Keyboard$subMap};

var _SwiftsNamesake$proper_keyboard$Keyboard_Key$toChar = function (key) {
	var _p0 = key;
	switch (_p0.ctor) {
		case 'Spacebar':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr(' '));
		case 'Zero':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('0'));
		case 'One':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('1'));
		case 'Two':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('2'));
		case 'Three':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('3'));
		case 'Four':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('4'));
		case 'Five':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('5'));
		case 'Six':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('6'));
		case 'Seven':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('7'));
		case 'Eight':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('8'));
		case 'Nine':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('9'));
		case 'A':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('A'));
		case 'B':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('B'));
		case 'C':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('C'));
		case 'D':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('D'));
		case 'E':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('E'));
		case 'F':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('F'));
		case 'G':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('G'));
		case 'H':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('H'));
		case 'I':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('I'));
		case 'J':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('J'));
		case 'K':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('K'));
		case 'L':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('L'));
		case 'M':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('M'));
		case 'N':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('N'));
		case 'O':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('O'));
		case 'P':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('P'));
		case 'Q':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('Q'));
		case 'R':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('R'));
		case 'S':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('S'));
		case 'T':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('T'));
		case 'U':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('U'));
		case 'V':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('V'));
		case 'W':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('W'));
		case 'X':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('X'));
		case 'Y':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('Y'));
		case 'Z':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('Z'));
		case 'NumpadZero':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('0'));
		case 'NumpadOne':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('1'));
		case 'NumpadTwo':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('2'));
		case 'NumpadThree':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('3'));
		case 'NumpadFour':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('4'));
		case 'NumpadFive':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('5'));
		case 'NumpadSix':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('6'));
		case 'NumpadSeven':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('7'));
		case 'NumpadEight':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('8'));
		case 'NumpadNine':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('9'));
		case 'Multiply':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('*'));
		case 'Add':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('+'));
		case 'Subtract':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('-'));
		case 'Divide':
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_Utils.chr('/'));
		default:
			return _elm_lang$core$Maybe$Nothing;
	}
};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Unknown = function (a) {
	return {ctor: 'Unknown', _0: a};
};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Ambiguous = function (a) {
	return {ctor: 'Ambiguous', _0: a};
};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Divide = {ctor: 'Divide'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Decimal = {ctor: 'Decimal'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Subtract = {ctor: 'Subtract'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Add = {ctor: 'Add'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Multiply = {ctor: 'Multiply'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadNine = {ctor: 'NumpadNine'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadEight = {ctor: 'NumpadEight'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadSeven = {ctor: 'NumpadSeven'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadSix = {ctor: 'NumpadSix'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadFive = {ctor: 'NumpadFive'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadFour = {ctor: 'NumpadFour'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadThree = {ctor: 'NumpadThree'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadTwo = {ctor: 'NumpadTwo'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadOne = {ctor: 'NumpadOne'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadZero = {ctor: 'NumpadZero'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F12 = {ctor: 'F12'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F11 = {ctor: 'F11'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F10 = {ctor: 'F10'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F9 = {ctor: 'F9'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F8 = {ctor: 'F8'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F7 = {ctor: 'F7'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F6 = {ctor: 'F6'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F5 = {ctor: 'F5'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F4 = {ctor: 'F4'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F3 = {ctor: 'F3'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F2 = {ctor: 'F2'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F1 = {ctor: 'F1'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$ScrollLock = {ctor: 'ScrollLock'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumLock = {ctor: 'NumLock'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$ChromeSearch = {ctor: 'ChromeSearch'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Command = {ctor: 'Command'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Windows = {ctor: 'Windows'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$code = function (key) {
	var _p1 = key;
	switch (_p1.ctor) {
		case 'Backspace':
			return _elm_lang$core$Maybe$Just(8);
		case 'Tab':
			return _elm_lang$core$Maybe$Just(9);
		case 'Enter':
			return _elm_lang$core$Maybe$Just(13);
		case 'Shift':
			return _elm_lang$core$Maybe$Just(16);
		case 'Ctrl':
			return _elm_lang$core$Maybe$Just(17);
		case 'Alt':
			return _elm_lang$core$Maybe$Just(18);
		case 'PauseBreak':
			return _elm_lang$core$Maybe$Just(19);
		case 'CapsLock':
			return _elm_lang$core$Maybe$Just(20);
		case 'Escape':
			return _elm_lang$core$Maybe$Just(27);
		case 'Spacebar':
			return _elm_lang$core$Maybe$Just(32);
		case 'PageUp':
			return _elm_lang$core$Maybe$Just(33);
		case 'PageDown':
			return _elm_lang$core$Maybe$Just(34);
		case 'End':
			return _elm_lang$core$Maybe$Just(35);
		case 'Home':
			return _elm_lang$core$Maybe$Just(36);
		case 'Left':
			return _elm_lang$core$Maybe$Just(37);
		case 'Up':
			return _elm_lang$core$Maybe$Just(38);
		case 'Right':
			return _elm_lang$core$Maybe$Just(39);
		case 'Down':
			return _elm_lang$core$Maybe$Just(40);
		case 'PrintScreen':
			return _elm_lang$core$Maybe$Just(44);
		case 'Insert':
			return _elm_lang$core$Maybe$Just(45);
		case 'Delete':
			return _elm_lang$core$Maybe$Just(46);
		case 'Zero':
			return _elm_lang$core$Maybe$Just(48);
		case 'One':
			return _elm_lang$core$Maybe$Just(49);
		case 'Two':
			return _elm_lang$core$Maybe$Just(50);
		case 'Three':
			return _elm_lang$core$Maybe$Just(51);
		case 'Four':
			return _elm_lang$core$Maybe$Just(52);
		case 'Five':
			return _elm_lang$core$Maybe$Just(53);
		case 'Six':
			return _elm_lang$core$Maybe$Just(54);
		case 'Seven':
			return _elm_lang$core$Maybe$Just(55);
		case 'Eight':
			return _elm_lang$core$Maybe$Just(56);
		case 'Nine':
			return _elm_lang$core$Maybe$Just(57);
		case 'A':
			return _elm_lang$core$Maybe$Just(65);
		case 'B':
			return _elm_lang$core$Maybe$Just(66);
		case 'C':
			return _elm_lang$core$Maybe$Just(67);
		case 'D':
			return _elm_lang$core$Maybe$Just(68);
		case 'E':
			return _elm_lang$core$Maybe$Just(69);
		case 'F':
			return _elm_lang$core$Maybe$Just(70);
		case 'G':
			return _elm_lang$core$Maybe$Just(71);
		case 'H':
			return _elm_lang$core$Maybe$Just(72);
		case 'I':
			return _elm_lang$core$Maybe$Just(73);
		case 'J':
			return _elm_lang$core$Maybe$Just(74);
		case 'K':
			return _elm_lang$core$Maybe$Just(75);
		case 'L':
			return _elm_lang$core$Maybe$Just(76);
		case 'M':
			return _elm_lang$core$Maybe$Just(77);
		case 'N':
			return _elm_lang$core$Maybe$Just(78);
		case 'O':
			return _elm_lang$core$Maybe$Just(79);
		case 'P':
			return _elm_lang$core$Maybe$Just(80);
		case 'Q':
			return _elm_lang$core$Maybe$Just(81);
		case 'R':
			return _elm_lang$core$Maybe$Just(82);
		case 'S':
			return _elm_lang$core$Maybe$Just(83);
		case 'T':
			return _elm_lang$core$Maybe$Just(84);
		case 'U':
			return _elm_lang$core$Maybe$Just(85);
		case 'V':
			return _elm_lang$core$Maybe$Just(86);
		case 'W':
			return _elm_lang$core$Maybe$Just(87);
		case 'X':
			return _elm_lang$core$Maybe$Just(88);
		case 'Y':
			return _elm_lang$core$Maybe$Just(89);
		case 'Z':
			return _elm_lang$core$Maybe$Just(90);
		case 'Ambiguous':
			return A2(
				_elm_lang$core$List$all,
				A2(
					_elm_lang$core$Basics$flip,
					_elm_lang$core$List$member,
					{
						ctor: '::',
						_0: _SwiftsNamesake$proper_keyboard$Keyboard_Key$Windows,
						_1: {
							ctor: '::',
							_0: _SwiftsNamesake$proper_keyboard$Keyboard_Key$Command,
							_1: {
								ctor: '::',
								_0: _SwiftsNamesake$proper_keyboard$Keyboard_Key$ChromeSearch,
								_1: {ctor: '[]'}
							}
						}
					}),
				_p1._0) ? _elm_lang$core$Maybe$Just(91) : _elm_lang$core$Maybe$Nothing;
		case 'Windows':
			return _elm_lang$core$Maybe$Just(91);
		case 'Command':
			return _elm_lang$core$Maybe$Just(91);
		case 'ChromeSearch':
			return _elm_lang$core$Maybe$Just(91);
		case 'NumpadZero':
			return _elm_lang$core$Maybe$Just(96);
		case 'NumpadOne':
			return _elm_lang$core$Maybe$Just(97);
		case 'NumpadTwo':
			return _elm_lang$core$Maybe$Just(98);
		case 'NumpadThree':
			return _elm_lang$core$Maybe$Just(99);
		case 'NumpadFour':
			return _elm_lang$core$Maybe$Just(100);
		case 'NumpadFive':
			return _elm_lang$core$Maybe$Just(101);
		case 'NumpadSix':
			return _elm_lang$core$Maybe$Just(102);
		case 'NumpadSeven':
			return _elm_lang$core$Maybe$Just(103);
		case 'NumpadEight':
			return _elm_lang$core$Maybe$Just(104);
		case 'NumpadNine':
			return _elm_lang$core$Maybe$Just(105);
		case 'Multiply':
			return _elm_lang$core$Maybe$Just(106);
		case 'Add':
			return _elm_lang$core$Maybe$Just(107);
		case 'Subtract':
			return _elm_lang$core$Maybe$Just(109);
		case 'Decimal':
			return _elm_lang$core$Maybe$Just(110);
		case 'Divide':
			return _elm_lang$core$Maybe$Just(111);
		case 'F1':
			return _elm_lang$core$Maybe$Just(112);
		case 'F2':
			return _elm_lang$core$Maybe$Just(113);
		case 'F3':
			return _elm_lang$core$Maybe$Just(114);
		case 'F4':
			return _elm_lang$core$Maybe$Just(115);
		case 'F5':
			return _elm_lang$core$Maybe$Just(116);
		case 'F6':
			return _elm_lang$core$Maybe$Just(117);
		case 'F7':
			return _elm_lang$core$Maybe$Just(118);
		case 'F8':
			return _elm_lang$core$Maybe$Just(119);
		case 'F9':
			return _elm_lang$core$Maybe$Just(120);
		case 'F10':
			return _elm_lang$core$Maybe$Just(121);
		case 'F11':
			return _elm_lang$core$Maybe$Just(122);
		case 'F12':
			return _elm_lang$core$Maybe$Just(123);
		case 'NumLock':
			return _elm_lang$core$Maybe$Just(144);
		case 'ScrollLock':
			return _elm_lang$core$Maybe$Just(145);
		default:
			return _elm_lang$core$Maybe$Nothing;
	}
};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$PauseBreak = {ctor: 'PauseBreak'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$PrintScreen = {ctor: 'PrintScreen'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Insert = {ctor: 'Insert'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Nine = {ctor: 'Nine'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Eight = {ctor: 'Eight'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Seven = {ctor: 'Seven'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Six = {ctor: 'Six'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Five = {ctor: 'Five'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Four = {ctor: 'Four'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Three = {ctor: 'Three'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Two = {ctor: 'Two'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$One = {ctor: 'One'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Zero = {ctor: 'Zero'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Home = {ctor: 'Home'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$End = {ctor: 'End'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$PageDown = {ctor: 'PageDown'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$PageUp = {ctor: 'PageUp'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Delete = {ctor: 'Delete'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Backspace = {ctor: 'Backspace'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Enter = {ctor: 'Enter'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Escape = {ctor: 'Escape'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Spacebar = {ctor: 'Spacebar'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$CapsLock = {ctor: 'CapsLock'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Tab = {ctor: 'Tab'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Alt = {ctor: 'Alt'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Ctrl = function (a) {
	return {ctor: 'Ctrl', _0: a};
};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Shift = function (a) {
	return {ctor: 'Shift', _0: a};
};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Down = {ctor: 'Down'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Up = {ctor: 'Up'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Right = {ctor: 'Right'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Left = {ctor: 'Left'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Z = {ctor: 'Z'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Y = {ctor: 'Y'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$X = {ctor: 'X'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$W = {ctor: 'W'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$V = {ctor: 'V'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$U = {ctor: 'U'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$T = {ctor: 'T'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$S = {ctor: 'S'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$R = {ctor: 'R'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$Q = {ctor: 'Q'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$P = {ctor: 'P'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$O = {ctor: 'O'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$N = {ctor: 'N'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$M = {ctor: 'M'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$L = {ctor: 'L'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$K = {ctor: 'K'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$J = {ctor: 'J'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$I = {ctor: 'I'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$H = {ctor: 'H'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$G = {ctor: 'G'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$F = {ctor: 'F'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$E = {ctor: 'E'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$D = {ctor: 'D'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$C = {ctor: 'C'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$B = {ctor: 'B'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$A = {ctor: 'A'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$fromCode = function (code) {
	var _p2 = code;
	switch (_p2) {
		case 8:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Backspace;
		case 9:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Tab;
		case 13:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Enter;
		case 16:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Shift(_elm_lang$core$Maybe$Nothing);
		case 17:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Ctrl(_elm_lang$core$Maybe$Nothing);
		case 18:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Alt;
		case 19:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$PauseBreak;
		case 20:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$CapsLock;
		case 27:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Escape;
		case 32:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Spacebar;
		case 33:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$PageUp;
		case 34:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$PageDown;
		case 35:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$End;
		case 36:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Home;
		case 37:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Left;
		case 38:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Up;
		case 39:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Right;
		case 40:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Down;
		case 44:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$PrintScreen;
		case 45:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Insert;
		case 46:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Delete;
		case 48:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Zero;
		case 49:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$One;
		case 50:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Two;
		case 51:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Three;
		case 52:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Four;
		case 53:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Five;
		case 54:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Six;
		case 55:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Seven;
		case 56:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Eight;
		case 57:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Nine;
		case 65:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$A;
		case 66:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$B;
		case 67:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$C;
		case 68:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$D;
		case 69:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$E;
		case 70:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F;
		case 71:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$G;
		case 72:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$H;
		case 73:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$I;
		case 74:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$J;
		case 75:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$K;
		case 76:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$L;
		case 77:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$M;
		case 78:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$N;
		case 79:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$O;
		case 80:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$P;
		case 81:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Q;
		case 82:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$R;
		case 83:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$S;
		case 84:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$T;
		case 85:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$U;
		case 86:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$V;
		case 87:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$W;
		case 88:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$X;
		case 89:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Y;
		case 90:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Z;
		case 91:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Ambiguous(
				{
					ctor: '::',
					_0: _SwiftsNamesake$proper_keyboard$Keyboard_Key$Windows,
					_1: {
						ctor: '::',
						_0: _SwiftsNamesake$proper_keyboard$Keyboard_Key$Command,
						_1: {
							ctor: '::',
							_0: _SwiftsNamesake$proper_keyboard$Keyboard_Key$ChromeSearch,
							_1: {ctor: '[]'}
						}
					}
				});
		case 96:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadZero;
		case 97:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadOne;
		case 98:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadTwo;
		case 99:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadThree;
		case 100:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadFour;
		case 101:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadFive;
		case 102:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadSix;
		case 103:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadSeven;
		case 104:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadEight;
		case 105:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumpadNine;
		case 106:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Multiply;
		case 107:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Add;
		case 109:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Subtract;
		case 110:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Decimal;
		case 111:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Divide;
		case 112:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F1;
		case 113:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F2;
		case 114:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F3;
		case 115:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F4;
		case 116:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F5;
		case 117:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F6;
		case 118:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F7;
		case 119:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F8;
		case 120:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F9;
		case 121:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F10;
		case 122:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F11;
		case 123:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$F12;
		case 144:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$NumLock;
		case 145:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$ScrollLock;
		default:
			return _SwiftsNamesake$proper_keyboard$Keyboard_Key$Unknown(code);
	}
};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$RightHand = {ctor: 'RightHand'};
var _SwiftsNamesake$proper_keyboard$Keyboard_Key$LeftHand = {ctor: 'LeftHand'};

var _Gizra$elm_keyboard_event$Keyboard_Event$decodeKey = _elm_lang$core$Json_Decode$maybe(
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (key) {
			return _elm_lang$core$String$isEmpty(key) ? _elm_lang$core$Json_Decode$fail('empty key') : _elm_lang$core$Json_Decode$succeed(key);
		},
		A2(_elm_lang$core$Json_Decode$field, 'key', _elm_lang$core$Json_Decode$string)));
var _Gizra$elm_keyboard_event$Keyboard_Event$decodeNonZero = A2(
	_elm_lang$core$Json_Decode$andThen,
	function (code) {
		return _elm_lang$core$Native_Utils.eq(code, 0) ? _elm_lang$core$Json_Decode$fail('code was zero') : _elm_lang$core$Json_Decode$succeed(code);
	},
	_elm_lang$core$Json_Decode$int);
var _Gizra$elm_keyboard_event$Keyboard_Event$decodeKeyCode = _elm_lang$core$Json_Decode$oneOf(
	{
		ctor: '::',
		_0: A2(_elm_lang$core$Json_Decode$field, 'keyCode', _Gizra$elm_keyboard_event$Keyboard_Event$decodeNonZero),
		_1: {
			ctor: '::',
			_0: A2(_elm_lang$core$Json_Decode$field, 'which', _Gizra$elm_keyboard_event$Keyboard_Event$decodeNonZero),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$field, 'charCode', _Gizra$elm_keyboard_event$Keyboard_Event$decodeNonZero),
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Json_Decode$succeed(0),
					_1: {ctor: '[]'}
				}
			}
		}
	});
var _Gizra$elm_keyboard_event$Keyboard_Event$KeyboardEvent = F7(
	function (a, b, c, d, e, f, g) {
		return {altKey: a, ctrlKey: b, key: c, keyCode: d, metaKey: e, repeat: f, shiftKey: g};
	});
var _Gizra$elm_keyboard_event$Keyboard_Event$decodeKeyboardEvent = A8(
	_elm_lang$core$Json_Decode$map7,
	_Gizra$elm_keyboard_event$Keyboard_Event$KeyboardEvent,
	A2(_elm_lang$core$Json_Decode$field, 'altKey', _elm_lang$core$Json_Decode$bool),
	A2(_elm_lang$core$Json_Decode$field, 'ctrlKey', _elm_lang$core$Json_Decode$bool),
	_Gizra$elm_keyboard_event$Keyboard_Event$decodeKey,
	A2(_elm_lang$core$Json_Decode$map, _SwiftsNamesake$proper_keyboard$Keyboard_Key$fromCode, _Gizra$elm_keyboard_event$Keyboard_Event$decodeKeyCode),
	A2(_elm_lang$core$Json_Decode$field, 'metaKey', _elm_lang$core$Json_Decode$bool),
	A2(_elm_lang$core$Json_Decode$field, 'repeat', _elm_lang$core$Json_Decode$bool),
	A2(_elm_lang$core$Json_Decode$field, 'shiftKey', _elm_lang$core$Json_Decode$bool));
var _Gizra$elm_keyboard_event$Keyboard_Event$considerKeyboardEvent = function (func) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		function (event) {
			var _p0 = func(event);
			if (_p0.ctor === 'Just') {
				return _elm_lang$core$Json_Decode$succeed(_p0._0);
			} else {
				return _elm_lang$core$Json_Decode$fail('Ignoring keyboard event');
			}
		},
		_Gizra$elm_keyboard_event$Keyboard_Event$decodeKeyboardEvent);
};

var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode = _elm_lang$core$Json_Decode$succeed;
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$resolve = _elm_lang$core$Json_Decode$andThen(_elm_lang$core$Basics$identity);
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom = _elm_lang$core$Json_Decode$map2(
	F2(
		function (x, y) {
			return y(x);
		}));
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$hardcoded = function (_p0) {
	return _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom(
		_elm_lang$core$Json_Decode$succeed(_p0));
};
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder = F3(
	function (pathDecoder, valDecoder, fallback) {
		var nullOr = function (decoder) {
			return _elm_lang$core$Json_Decode$oneOf(
				{
					ctor: '::',
					_0: decoder,
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Json_Decode$null(fallback),
						_1: {ctor: '[]'}
					}
				});
		};
		var handleResult = function (input) {
			var _p1 = A2(_elm_lang$core$Json_Decode$decodeValue, pathDecoder, input);
			if (_p1.ctor === 'Ok') {
				var _p2 = A2(
					_elm_lang$core$Json_Decode$decodeValue,
					nullOr(valDecoder),
					_p1._0);
				if (_p2.ctor === 'Ok') {
					return _elm_lang$core$Json_Decode$succeed(_p2._0);
				} else {
					return _elm_lang$core$Json_Decode$fail(_p2._0);
				}
			} else {
				return _elm_lang$core$Json_Decode$succeed(fallback);
			}
		};
		return A2(_elm_lang$core$Json_Decode$andThen, handleResult, _elm_lang$core$Json_Decode$value);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalAt = F4(
	function (path, valDecoder, fallback, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder,
				A2(_elm_lang$core$Json_Decode$at, path, _elm_lang$core$Json_Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional = F4(
	function (key, valDecoder, fallback, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder,
				A2(_elm_lang$core$Json_Decode$field, key, _elm_lang$core$Json_Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt = F3(
	function (path, valDecoder, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A2(_elm_lang$core$Json_Decode$at, path, valDecoder),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A2(_elm_lang$core$Json_Decode$field, key, valDecoder),
			decoder);
	});

var _elm_lang$core$Set$foldr = F3(
	function (f, b, _p0) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (k, _p2, b) {
					return A2(f, k, b);
				}),
			b,
			_p1._0);
	});
var _elm_lang$core$Set$foldl = F3(
	function (f, b, _p3) {
		var _p4 = _p3;
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, _p5, b) {
					return A2(f, k, b);
				}),
			b,
			_p4._0);
	});
var _elm_lang$core$Set$toList = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$core$Dict$keys(_p7._0);
};
var _elm_lang$core$Set$size = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Dict$size(_p9._0);
};
var _elm_lang$core$Set$member = F2(
	function (k, _p10) {
		var _p11 = _p10;
		return A2(_elm_lang$core$Dict$member, k, _p11._0);
	});
var _elm_lang$core$Set$isEmpty = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$core$Dict$isEmpty(_p13._0);
};
var _elm_lang$core$Set$Set_elm_builtin = function (a) {
	return {ctor: 'Set_elm_builtin', _0: a};
};
var _elm_lang$core$Set$empty = _elm_lang$core$Set$Set_elm_builtin(_elm_lang$core$Dict$empty);
var _elm_lang$core$Set$singleton = function (k) {
	return _elm_lang$core$Set$Set_elm_builtin(
		A2(
			_elm_lang$core$Dict$singleton,
			k,
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Set$insert = F2(
	function (k, _p14) {
		var _p15 = _p14;
		return _elm_lang$core$Set$Set_elm_builtin(
			A3(
				_elm_lang$core$Dict$insert,
				k,
				{ctor: '_Tuple0'},
				_p15._0));
	});
var _elm_lang$core$Set$fromList = function (xs) {
	return A3(_elm_lang$core$List$foldl, _elm_lang$core$Set$insert, _elm_lang$core$Set$empty, xs);
};
var _elm_lang$core$Set$map = F2(
	function (f, s) {
		return _elm_lang$core$Set$fromList(
			A2(
				_elm_lang$core$List$map,
				f,
				_elm_lang$core$Set$toList(s)));
	});
var _elm_lang$core$Set$remove = F2(
	function (k, _p16) {
		var _p17 = _p16;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$remove, k, _p17._0));
	});
var _elm_lang$core$Set$union = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$union, _p20._0, _p21._0));
	});
var _elm_lang$core$Set$intersect = F2(
	function (_p23, _p22) {
		var _p24 = _p23;
		var _p25 = _p22;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$intersect, _p24._0, _p25._0));
	});
var _elm_lang$core$Set$diff = F2(
	function (_p27, _p26) {
		var _p28 = _p27;
		var _p29 = _p26;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$diff, _p28._0, _p29._0));
	});
var _elm_lang$core$Set$filter = F2(
	function (p, _p30) {
		var _p31 = _p30;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p32) {
						return p(k);
					}),
				_p31._0));
	});
var _elm_lang$core$Set$partition = F2(
	function (p, _p33) {
		var _p34 = _p33;
		var _p35 = A2(
			_elm_lang$core$Dict$partition,
			F2(
				function (k, _p36) {
					return p(k);
				}),
			_p34._0);
		var p1 = _p35._0;
		var p2 = _p35._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Set$Set_elm_builtin(p1),
			_1: _elm_lang$core$Set$Set_elm_builtin(p2)
		};
	});

var _elm_community$dict_extra$Dict_Extra$find = F2(
	function (predicate, dict) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, acc) {
					var _p0 = acc;
					if (_p0.ctor === 'Just') {
						return acc;
					} else {
						return A2(predicate, k, v) ? _elm_lang$core$Maybe$Just(
							{ctor: '_Tuple2', _0: k, _1: v}) : _elm_lang$core$Maybe$Nothing;
					}
				}),
			_elm_lang$core$Maybe$Nothing,
			dict);
	});
var _elm_community$dict_extra$Dict_Extra$invert = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldl,
		F3(
			function (k, v, acc) {
				return A3(_elm_lang$core$Dict$insert, v, k, acc);
			}),
		_elm_lang$core$Dict$empty,
		dict);
};
var _elm_community$dict_extra$Dict_Extra$filterMap = F2(
	function (f, dict) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, acc) {
					var _p1 = A2(f, k, v);
					if (_p1.ctor === 'Just') {
						return A3(_elm_lang$core$Dict$insert, k, _p1._0, acc);
					} else {
						return acc;
					}
				}),
			_elm_lang$core$Dict$empty,
			dict);
	});
var _elm_community$dict_extra$Dict_Extra$mapKeys = F2(
	function (keyMapper, dict) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, acc) {
					return A3(
						_elm_lang$core$Dict$insert,
						keyMapper(k),
						v,
						acc);
				}),
			_elm_lang$core$Dict$empty,
			dict);
	});
var _elm_community$dict_extra$Dict_Extra$keepOnly = F2(
	function (set, dict) {
		return A3(
			_elm_lang$core$Set$foldl,
			F2(
				function (k, acc) {
					return A2(
						_elm_lang$core$Maybe$withDefault,
						acc,
						A2(
							_elm_lang$core$Maybe$map,
							function (v) {
								return A3(_elm_lang$core$Dict$insert, k, v, acc);
							},
							A2(_elm_lang$core$Dict$get, k, dict)));
				}),
			_elm_lang$core$Dict$empty,
			set);
	});
var _elm_community$dict_extra$Dict_Extra$insertDedupe = F4(
	function (combine, key, value, dict) {
		var $with = function (mbValue) {
			var _p2 = mbValue;
			if (_p2.ctor === 'Just') {
				return _elm_lang$core$Maybe$Just(
					A2(combine, _p2._0, value));
			} else {
				return _elm_lang$core$Maybe$Just(value);
			}
		};
		return A3(_elm_lang$core$Dict$update, key, $with, dict);
	});
var _elm_community$dict_extra$Dict_Extra$removeMany = F2(
	function (set, dict) {
		return A3(_elm_lang$core$Set$foldl, _elm_lang$core$Dict$remove, dict, set);
	});
var _elm_community$dict_extra$Dict_Extra$removeWhen = F2(
	function (pred, dict) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, v) {
					return !A2(pred, k, v);
				}),
			dict);
	});
var _elm_community$dict_extra$Dict_Extra$fromListDedupeBy = F3(
	function (combine, keyfn, xs) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, acc) {
					return A4(
						_elm_community$dict_extra$Dict_Extra$insertDedupe,
						combine,
						keyfn(x),
						x,
						acc);
				}),
			_elm_lang$core$Dict$empty,
			xs);
	});
var _elm_community$dict_extra$Dict_Extra$fromListDedupe = F2(
	function (combine, xs) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p3, acc) {
					var _p4 = _p3;
					return A4(_elm_community$dict_extra$Dict_Extra$insertDedupe, combine, _p4._0, _p4._1, acc);
				}),
			_elm_lang$core$Dict$empty,
			xs);
	});
var _elm_community$dict_extra$Dict_Extra$fromListBy = F2(
	function (keyfn, xs) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, acc) {
					return A3(
						_elm_lang$core$Dict$insert,
						keyfn(x),
						x,
						acc);
				}),
			_elm_lang$core$Dict$empty,
			xs);
	});
var _elm_community$dict_extra$Dict_Extra$filterGroupBy = F2(
	function (keyfn, list) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					var _p5 = keyfn(x);
					if (_p5.ctor === 'Just') {
						return A3(
							_elm_lang$core$Dict$update,
							_p5._0,
							function (_p6) {
								return _elm_lang$core$Maybe$Just(
									A2(
										_elm_lang$core$Maybe$withDefault,
										{
											ctor: '::',
											_0: x,
											_1: {ctor: '[]'}
										},
										A2(
											_elm_lang$core$Maybe$map,
											F2(
												function (x, y) {
													return {ctor: '::', _0: x, _1: y};
												})(x),
											_p6)));
							},
							acc);
					} else {
						return acc;
					}
				}),
			_elm_lang$core$Dict$empty,
			list);
	});
var _elm_community$dict_extra$Dict_Extra$groupBy = F2(
	function (keyfn, list) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return A3(
						_elm_lang$core$Dict$update,
						keyfn(x),
						function (_p7) {
							return _elm_lang$core$Maybe$Just(
								A2(
									_elm_lang$core$Maybe$withDefault,
									{
										ctor: '::',
										_0: x,
										_1: {ctor: '[]'}
									},
									A2(
										_elm_lang$core$Maybe$map,
										F2(
											function (x, y) {
												return {ctor: '::', _0: x, _1: y};
											})(x),
										_p7)));
						},
						acc);
				}),
			_elm_lang$core$Dict$empty,
			list);
	});

var _elm_community$list_extra$List_Extra$greedyGroupsOfWithStep = F3(
	function (size, step, xs) {
		var okayXs = _elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(xs),
			0) > 0;
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		return (okayArgs && okayXs) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$greedyGroupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		var okayLength = _elm_lang$core$Native_Utils.eq(
			size,
			_elm_lang$core$List$length(group));
		return (okayArgs && okayLength) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$groupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$zip5 = _elm_lang$core$List$map5(
	F5(
		function (v0, v1, v2, v3, v4) {
			return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
		}));
var _elm_community$list_extra$List_Extra$zip4 = _elm_lang$core$List$map4(
	F4(
		function (v0, v1, v2, v3) {
			return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
		}));
var _elm_community$list_extra$List_Extra$zip3 = _elm_lang$core$List$map3(
	F3(
		function (v0, v1, v2) {
			return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
		}));
var _elm_community$list_extra$List_Extra$zip = _elm_lang$core$List$map2(
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}));
var _elm_community$list_extra$List_Extra$isPrefixOf = F2(
	function (prefix, xs) {
		var _p0 = {ctor: '_Tuple2', _0: prefix, _1: xs};
		if (_p0._0.ctor === '[]') {
			return true;
		} else {
			if (_p0._1.ctor === '[]') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.eq(_p0._0._0, _p0._1._0) && A2(_elm_community$list_extra$List_Extra$isPrefixOf, _p0._0._1, _p0._1._1);
			}
		}
	});
var _elm_community$list_extra$List_Extra$isSuffixOf = F2(
	function (suffix, xs) {
		return A2(
			_elm_community$list_extra$List_Extra$isPrefixOf,
			_elm_lang$core$List$reverse(suffix),
			_elm_lang$core$List$reverse(xs));
	});
var _elm_community$list_extra$List_Extra$selectSplit = function (xs) {
	var _p1 = xs;
	if (_p1.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p5 = _p1._1;
		var _p4 = _p1._0;
		return {
			ctor: '::',
			_0: {
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _p4,
				_2: _p5
			},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p2) {
					var _p3 = _p2;
					return {
						ctor: '_Tuple3',
						_0: {ctor: '::', _0: _p4, _1: _p3._0},
						_1: _p3._1,
						_2: _p3._2
					};
				},
				_elm_community$list_extra$List_Extra$selectSplit(_p5))
		};
	}
};
var _elm_community$list_extra$List_Extra$select = function (xs) {
	var _p6 = xs;
	if (_p6.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p10 = _p6._1;
		var _p9 = _p6._0;
		return {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _p9, _1: _p10},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p7) {
					var _p8 = _p7;
					return {
						ctor: '_Tuple2',
						_0: _p8._0,
						_1: {ctor: '::', _0: _p9, _1: _p8._1}
					};
				},
				_elm_community$list_extra$List_Extra$select(_p10))
		};
	}
};
var _elm_community$list_extra$List_Extra$tailsHelp = F2(
	function (e, list) {
		var _p11 = list;
		if (_p11.ctor === '::') {
			var _p12 = _p11._0;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: e, _1: _p12},
				_1: {ctor: '::', _0: _p12, _1: _p11._1}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _elm_community$list_extra$List_Extra$tails = A2(
	_elm_lang$core$List$foldr,
	_elm_community$list_extra$List_Extra$tailsHelp,
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$isInfixOf = F2(
	function (infix, xs) {
		return A2(
			_elm_lang$core$List$any,
			_elm_community$list_extra$List_Extra$isPrefixOf(infix),
			_elm_community$list_extra$List_Extra$tails(xs));
	});
var _elm_community$list_extra$List_Extra$inits = A2(
	_elm_lang$core$List$foldr,
	F2(
		function (e, acc) {
			return {
				ctor: '::',
				_0: {ctor: '[]'},
				_1: A2(
					_elm_lang$core$List$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(e),
					acc)
			};
		}),
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$groupWhileTransitively = F2(
	function (cmp, xs_) {
		var _p13 = xs_;
		if (_p13.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p13._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: {
						ctor: '::',
						_0: _p13._0,
						_1: {ctor: '[]'}
					},
					_1: {ctor: '[]'}
				};
			} else {
				var _p15 = _p13._0;
				var _p14 = A2(_elm_community$list_extra$List_Extra$groupWhileTransitively, cmp, _p13._1);
				if (_p14.ctor === '::') {
					return A2(cmp, _p15, _p13._1._0) ? {
						ctor: '::',
						_0: {ctor: '::', _0: _p15, _1: _p14._0},
						_1: _p14._1
					} : {
						ctor: '::',
						_0: {
							ctor: '::',
							_0: _p15,
							_1: {ctor: '[]'}
						},
						_1: _p14
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$stripPrefix = F2(
	function (prefix, xs) {
		var step = F2(
			function (e, m) {
				var _p16 = m;
				if (_p16.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_p16._0.ctor === '[]') {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						return _elm_lang$core$Native_Utils.eq(e, _p16._0._0) ? _elm_lang$core$Maybe$Just(_p16._0._1) : _elm_lang$core$Maybe$Nothing;
					}
				}
			});
		return A3(
			_elm_lang$core$List$foldl,
			step,
			_elm_lang$core$Maybe$Just(xs),
			prefix);
	});
var _elm_community$list_extra$List_Extra$dropWhileRight = function (p) {
	return A2(
		_elm_lang$core$List$foldr,
		F2(
			function (x, xs) {
				return (p(x) && _elm_lang$core$List$isEmpty(xs)) ? {ctor: '[]'} : {ctor: '::', _0: x, _1: xs};
			}),
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$takeWhileRight = function (p) {
	var step = F2(
		function (x, _p17) {
			var _p18 = _p17;
			var _p19 = _p18._0;
			return (p(x) && _p18._1) ? {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: x, _1: _p19},
				_1: true
			} : {ctor: '_Tuple2', _0: _p19, _1: false};
		});
	return function (_p20) {
		return _elm_lang$core$Tuple$first(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: {ctor: '[]'},
					_1: true
				},
				_p20));
	};
};
var _elm_community$list_extra$List_Extra$splitAt = F2(
	function (n, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_lang$core$List$take, n, xs),
			_1: A2(_elm_lang$core$List$drop, n, xs)
		};
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying_ = F3(
	function (listOflengths, list, accu) {
		groupsOfVarying_:
		while (true) {
			var _p21 = {ctor: '_Tuple2', _0: listOflengths, _1: list};
			if (((_p21.ctor === '_Tuple2') && (_p21._0.ctor === '::')) && (_p21._1.ctor === '::')) {
				var _p22 = A2(_elm_community$list_extra$List_Extra$splitAt, _p21._0._0, list);
				var head = _p22._0;
				var tail = _p22._1;
				var _v11 = _p21._0._1,
					_v12 = tail,
					_v13 = {ctor: '::', _0: head, _1: accu};
				listOflengths = _v11;
				list = _v12;
				accu = _v13;
				continue groupsOfVarying_;
			} else {
				return _elm_lang$core$List$reverse(accu);
			}
		}
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying = F2(
	function (listOflengths, list) {
		return A3(
			_elm_community$list_extra$List_Extra$groupsOfVarying_,
			listOflengths,
			list,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$unfoldr = F2(
	function (f, seed) {
		var _p23 = f(seed);
		if (_p23.ctor === 'Nothing') {
			return {ctor: '[]'};
		} else {
			return {
				ctor: '::',
				_0: _p23._0._0,
				_1: A2(_elm_community$list_extra$List_Extra$unfoldr, f, _p23._0._1)
			};
		}
	});
var _elm_community$list_extra$List_Extra$scanr1 = F2(
	function (f, xs_) {
		var _p24 = xs_;
		if (_p24.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p24._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: _p24._0,
					_1: {ctor: '[]'}
				};
			} else {
				var _p25 = A2(_elm_community$list_extra$List_Extra$scanr1, f, _p24._1);
				if (_p25.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, _p24._0, _p25._0),
						_1: _p25
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanr = F3(
	function (f, acc, xs_) {
		var _p26 = xs_;
		if (_p26.ctor === '[]') {
			return {
				ctor: '::',
				_0: acc,
				_1: {ctor: '[]'}
			};
		} else {
			var _p27 = A3(_elm_community$list_extra$List_Extra$scanr, f, acc, _p26._1);
			if (_p27.ctor === '::') {
				return {
					ctor: '::',
					_0: A2(f, _p26._0, _p27._0),
					_1: _p27
				};
			} else {
				return {ctor: '[]'};
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanl1 = F2(
	function (f, xs_) {
		var _p28 = xs_;
		if (_p28.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			return A3(_elm_lang$core$List$scanl, f, _p28._0, _p28._1);
		}
	});
var _elm_community$list_extra$List_Extra$indexedFoldr = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p29) {
				var _p30 = _p29;
				var _p31 = _p30._0;
				return {
					ctor: '_Tuple2',
					_0: _p31 - 1,
					_1: A3(func, _p31, x, _p30._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$length(list) - 1,
					_1: acc
				},
				list));
	});
var _elm_community$list_extra$List_Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p32) {
				var _p33 = _p32;
				var _p34 = _p33._0;
				return {
					ctor: '_Tuple2',
					_0: _p34 + 1,
					_1: A3(func, _p34, x, _p33._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				step,
				{ctor: '_Tuple2', _0: 0, _1: acc},
				list));
	});
var _elm_community$list_extra$List_Extra$foldr1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p35 = m;
						if (_p35.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, x, _p35._0);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldr, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$foldl1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p36 = m;
						if (_p36.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, _p36._0, x);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldl, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$interweaveHelp = F3(
	function (l1, l2, acc) {
		interweaveHelp:
		while (true) {
			var _p37 = {ctor: '_Tuple2', _0: l1, _1: l2};
			_v24_1:
			do {
				if (_p37._0.ctor === '::') {
					if (_p37._1.ctor === '::') {
						var _v25 = _p37._0._1,
							_v26 = _p37._1._1,
							_v27 = A2(
							_elm_lang$core$Basics_ops['++'],
							acc,
							{
								ctor: '::',
								_0: _p37._0._0,
								_1: {
									ctor: '::',
									_0: _p37._1._0,
									_1: {ctor: '[]'}
								}
							});
						l1 = _v25;
						l2 = _v26;
						acc = _v27;
						continue interweaveHelp;
					} else {
						break _v24_1;
					}
				} else {
					if (_p37._1.ctor === '[]') {
						break _v24_1;
					} else {
						return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._1);
					}
				}
			} while(false);
			return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._0);
		}
	});
var _elm_community$list_extra$List_Extra$interweave = F2(
	function (l1, l2) {
		return A3(
			_elm_community$list_extra$List_Extra$interweaveHelp,
			l1,
			l2,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$permutations = function (xs_) {
	var _p38 = xs_;
	if (_p38.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		var f = function (_p39) {
			var _p40 = _p39;
			return A2(
				_elm_lang$core$List$map,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(_p40._0),
				_elm_community$list_extra$List_Extra$permutations(_p40._1));
		};
		return A2(
			_elm_lang$core$List$concatMap,
			f,
			_elm_community$list_extra$List_Extra$select(_p38));
	}
};
var _elm_community$list_extra$List_Extra$isPermutationOf = F2(
	function (permut, xs) {
		return A2(
			_elm_lang$core$List$member,
			permut,
			_elm_community$list_extra$List_Extra$permutations(xs));
	});
var _elm_community$list_extra$List_Extra$subsequencesNonEmpty = function (xs) {
	var _p41 = xs;
	if (_p41.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p42 = _p41._0;
		var f = F2(
			function (ys, r) {
				return {
					ctor: '::',
					_0: ys,
					_1: {
						ctor: '::',
						_0: {ctor: '::', _0: _p42, _1: ys},
						_1: r
					}
				};
			});
		return {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _p42,
				_1: {ctor: '[]'}
			},
			_1: A3(
				_elm_lang$core$List$foldr,
				f,
				{ctor: '[]'},
				_elm_community$list_extra$List_Extra$subsequencesNonEmpty(_p41._1))
		};
	}
};
var _elm_community$list_extra$List_Extra$subsequences = function (xs) {
	return {
		ctor: '::',
		_0: {ctor: '[]'},
		_1: _elm_community$list_extra$List_Extra$subsequencesNonEmpty(xs)
	};
};
var _elm_community$list_extra$List_Extra$isSubsequenceOf = F2(
	function (subseq, xs) {
		return A2(
			_elm_lang$core$List$member,
			subseq,
			_elm_community$list_extra$List_Extra$subsequences(xs));
	});
var _elm_community$list_extra$List_Extra$transpose = function (ll) {
	transpose:
	while (true) {
		var _p43 = ll;
		if (_p43.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p43._0.ctor === '[]') {
				var _v32 = _p43._1;
				ll = _v32;
				continue transpose;
			} else {
				var _p44 = _p43._1;
				var tails = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$tail, _p44);
				var heads = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$head, _p44);
				return {
					ctor: '::',
					_0: {ctor: '::', _0: _p43._0._0, _1: heads},
					_1: _elm_community$list_extra$List_Extra$transpose(
						{ctor: '::', _0: _p43._0._1, _1: tails})
				};
			}
		}
	}
};
var _elm_community$list_extra$List_Extra$intercalate = function (xs) {
	return function (_p45) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$intersperse, xs, _p45));
	};
};
var _elm_community$list_extra$List_Extra$filterNot = F2(
	function (pred, list) {
		return A2(
			_elm_lang$core$List$filter,
			function (_p46) {
				return !pred(_p46);
			},
			list);
	});
var _elm_community$list_extra$List_Extra$removeAt = F2(
	function (index, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return l;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p47 = tail;
			if (_p47.ctor === 'Nothing') {
				return l;
			} else {
				return A2(_elm_lang$core$List$append, head, _p47._0);
			}
		}
	});
var _elm_community$list_extra$List_Extra$stableSortWith = F2(
	function (pred, list) {
		var predWithIndex = F2(
			function (_p49, _p48) {
				var _p50 = _p49;
				var _p51 = _p48;
				var result = A2(pred, _p50._0, _p51._0);
				var _p52 = result;
				if (_p52.ctor === 'EQ') {
					return A2(_elm_lang$core$Basics$compare, _p50._1, _p51._1);
				} else {
					return result;
				}
			});
		var listWithIndex = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, a) {
					return {ctor: '_Tuple2', _0: a, _1: i};
				}),
			list);
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(_elm_lang$core$List$sortWith, predWithIndex, listWithIndex));
	});
var _elm_community$list_extra$List_Extra$setAt = F3(
	function (index, value, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p53 = tail;
			if (_p53.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(
					A2(
						_elm_lang$core$List$append,
						head,
						{ctor: '::', _0: value, _1: _p53._0}));
			}
		}
	});
var _elm_community$list_extra$List_Extra$remove = F2(
	function (x, xs) {
		var _p54 = xs;
		if (_p54.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p56 = _p54._1;
			var _p55 = _p54._0;
			return _elm_lang$core$Native_Utils.eq(x, _p55) ? _p56 : {
				ctor: '::',
				_0: _p55,
				_1: A2(_elm_community$list_extra$List_Extra$remove, x, _p56)
			};
		}
	});
var _elm_community$list_extra$List_Extra$updateIfIndex = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, x) {
					return predicate(i) ? update(x) : x;
				}),
			list);
	});
var _elm_community$list_extra$List_Extra$updateAt = F3(
	function (index, update, list) {
		return ((_elm_lang$core$Native_Utils.cmp(index, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(
			index,
			_elm_lang$core$List$length(list)) > -1)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
			A3(
				_elm_community$list_extra$List_Extra$updateIfIndex,
				F2(
					function (x, y) {
						return _elm_lang$core$Native_Utils.eq(x, y);
					})(index),
				update,
				list));
	});
var _elm_community$list_extra$List_Extra$updateIf = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$map,
			function (item) {
				return predicate(item) ? update(item) : item;
			},
			list);
	});
var _elm_community$list_extra$List_Extra$replaceIf = F3(
	function (predicate, replacement, list) {
		return A3(
			_elm_community$list_extra$List_Extra$updateIf,
			predicate,
			_elm_lang$core$Basics$always(replacement),
			list);
	});
var _elm_community$list_extra$List_Extra$findIndices = function (p) {
	return function (_p57) {
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(
				_elm_lang$core$List$filter,
				function (_p58) {
					var _p59 = _p58;
					return p(_p59._1);
				},
				A2(
					_elm_lang$core$List$indexedMap,
					F2(
						function (v0, v1) {
							return {ctor: '_Tuple2', _0: v0, _1: v1};
						}),
					_p57)));
	};
};
var _elm_community$list_extra$List_Extra$findIndex = function (p) {
	return function (_p60) {
		return _elm_lang$core$List$head(
			A2(_elm_community$list_extra$List_Extra$findIndices, p, _p60));
	};
};
var _elm_community$list_extra$List_Extra$splitWhen = F2(
	function (predicate, list) {
		return A2(
			_elm_lang$core$Maybe$map,
			function (i) {
				return A2(_elm_community$list_extra$List_Extra$splitAt, i, list);
			},
			A2(_elm_community$list_extra$List_Extra$findIndex, predicate, list));
	});
var _elm_community$list_extra$List_Extra$elemIndices = function (x) {
	return _elm_community$list_extra$List_Extra$findIndices(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$elemIndex = function (x) {
	return _elm_community$list_extra$List_Extra$findIndex(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			var _p61 = list;
			if (_p61.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p62 = _p61._0;
				if (predicate(_p62)) {
					return _elm_lang$core$Maybe$Just(_p62);
				} else {
					var _v41 = predicate,
						_v42 = _p61._1;
					predicate = _v41;
					list = _v42;
					continue find;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$notMember = function (x) {
	return function (_p63) {
		return !A2(_elm_lang$core$List$member, x, _p63);
	};
};
var _elm_community$list_extra$List_Extra$andThen = _elm_lang$core$List$concatMap;
var _elm_community$list_extra$List_Extra$lift2 = F3(
	function (f, la, lb) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return {
							ctor: '::',
							_0: A2(f, a, b),
							_1: {ctor: '[]'}
						};
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift3 = F4(
	function (f, la, lb, lc) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return {
									ctor: '::',
									_0: A3(f, a, b, c),
									_1: {ctor: '[]'}
								};
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift4 = F5(
	function (f, la, lb, lc, ld) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return A2(
									_elm_community$list_extra$List_Extra$andThen,
									function (d) {
										return {
											ctor: '::',
											_0: A4(f, a, b, c, d),
											_1: {ctor: '[]'}
										};
									},
									ld);
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$andMap = F2(
	function (l, fl) {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (x, y) {
					return x(y);
				}),
			fl,
			l);
	});
var _elm_community$list_extra$List_Extra$uniqueHelp = F3(
	function (f, existing, remaining) {
		uniqueHelp:
		while (true) {
			var _p64 = remaining;
			if (_p64.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				var _p66 = _p64._1;
				var _p65 = _p64._0;
				var computedFirst = f(_p65);
				if (A2(_elm_lang$core$Set$member, computedFirst, existing)) {
					var _v44 = f,
						_v45 = existing,
						_v46 = _p66;
					f = _v44;
					existing = _v45;
					remaining = _v46;
					continue uniqueHelp;
				} else {
					return {
						ctor: '::',
						_0: _p65,
						_1: A3(
							_elm_community$list_extra$List_Extra$uniqueHelp,
							f,
							A2(_elm_lang$core$Set$insert, computedFirst, existing),
							_p66)
					};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$uniqueBy = F2(
	function (f, list) {
		return A3(_elm_community$list_extra$List_Extra$uniqueHelp, f, _elm_lang$core$Set$empty, list);
	});
var _elm_community$list_extra$List_Extra$allDifferentBy = F2(
	function (f, list) {
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$List$length(list),
			_elm_lang$core$List$length(
				A2(_elm_community$list_extra$List_Extra$uniqueBy, f, list)));
	});
var _elm_community$list_extra$List_Extra$allDifferent = function (list) {
	return A2(_elm_community$list_extra$List_Extra$allDifferentBy, _elm_lang$core$Basics$identity, list);
};
var _elm_community$list_extra$List_Extra$unique = function (list) {
	return A3(_elm_community$list_extra$List_Extra$uniqueHelp, _elm_lang$core$Basics$identity, _elm_lang$core$Set$empty, list);
};
var _elm_community$list_extra$List_Extra$dropWhile = F2(
	function (predicate, list) {
		dropWhile:
		while (true) {
			var _p67 = list;
			if (_p67.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				if (predicate(_p67._0)) {
					var _v48 = predicate,
						_v49 = _p67._1;
					predicate = _v48;
					list = _v49;
					continue dropWhile;
				} else {
					return list;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				var _p68 = list;
				if (_p68.ctor === '[]') {
					return _elm_lang$core$List$reverse(memo);
				} else {
					var _p69 = _p68._0;
					if (predicate(_p69)) {
						var _v51 = {ctor: '::', _0: _p69, _1: memo},
							_v52 = _p68._1;
						memo = _v51;
						list = _v52;
						continue takeWhileMemo;
					} else {
						return _elm_lang$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$span = F2(
	function (p, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_community$list_extra$List_Extra$takeWhile, p, xs),
			_1: A2(_elm_community$list_extra$List_Extra$dropWhile, p, xs)
		};
	});
var _elm_community$list_extra$List_Extra$break = function (p) {
	return _elm_community$list_extra$List_Extra$span(
		function (_p70) {
			return !p(_p70);
		});
};
var _elm_community$list_extra$List_Extra$groupWhile = F2(
	function (eq, xs_) {
		var _p71 = xs_;
		if (_p71.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p73 = _p71._0;
			var _p72 = A2(
				_elm_community$list_extra$List_Extra$span,
				eq(_p73),
				_p71._1);
			var ys = _p72._0;
			var zs = _p72._1;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: _p73, _1: ys},
				_1: A2(_elm_community$list_extra$List_Extra$groupWhile, eq, zs)
			};
		}
	});
var _elm_community$list_extra$List_Extra$group = _elm_community$list_extra$List_Extra$groupWhile(
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		}));
var _elm_community$list_extra$List_Extra$minimumBy = F2(
	function (f, ls) {
		var minBy = F2(
			function (x, _p74) {
				var _p75 = _p74;
				var _p76 = _p75._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p76) < 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p75._0, _1: _p76};
			});
		var _p77 = ls;
		if (_p77.ctor === '::') {
			if (_p77._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p77._0);
			} else {
				var _p78 = _p77._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							minBy,
							{
								ctor: '_Tuple2',
								_0: _p78,
								_1: f(_p78)
							},
							_p77._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$maximumBy = F2(
	function (f, ls) {
		var maxBy = F2(
			function (x, _p79) {
				var _p80 = _p79;
				var _p81 = _p80._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p81) > 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p80._0, _1: _p81};
			});
		var _p82 = ls;
		if (_p82.ctor === '::') {
			if (_p82._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p82._0);
			} else {
				var _p83 = _p82._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							maxBy,
							{
								ctor: '_Tuple2',
								_0: _p83,
								_1: f(_p83)
							},
							_p82._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$uncons = function (xs) {
	var _p84 = xs;
	if (_p84.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: _p84._0, _1: _p84._1});
	}
};
var _elm_community$list_extra$List_Extra$swapAt = F3(
	function (index1, index2, l) {
		swapAt:
		while (true) {
			if (_elm_lang$core$Native_Utils.eq(index1, index2)) {
				return _elm_lang$core$Maybe$Just(l);
			} else {
				if (_elm_lang$core$Native_Utils.cmp(index1, index2) > 0) {
					var _v59 = index2,
						_v60 = index1,
						_v61 = l;
					index1 = _v59;
					index2 = _v60;
					l = _v61;
					continue swapAt;
				} else {
					if (_elm_lang$core$Native_Utils.cmp(index1, 0) < 0) {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						var _p85 = A2(_elm_community$list_extra$List_Extra$splitAt, index1, l);
						var part1 = _p85._0;
						var tail1 = _p85._1;
						var _p86 = A2(_elm_community$list_extra$List_Extra$splitAt, index2 - index1, tail1);
						var head2 = _p86._0;
						var tail2 = _p86._1;
						return A3(
							_elm_lang$core$Maybe$map2,
							F2(
								function (_p88, _p87) {
									var _p89 = _p88;
									var _p90 = _p87;
									return _elm_lang$core$List$concat(
										{
											ctor: '::',
											_0: part1,
											_1: {
												ctor: '::',
												_0: {ctor: '::', _0: _p90._0, _1: _p89._1},
												_1: {
													ctor: '::',
													_0: {ctor: '::', _0: _p89._0, _1: _p90._1},
													_1: {ctor: '[]'}
												}
											}
										});
								}),
							_elm_community$list_extra$List_Extra$uncons(head2),
							_elm_community$list_extra$List_Extra$uncons(tail2));
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$iterate = F2(
	function (f, x) {
		var _p91 = f(x);
		if (_p91.ctor === 'Just') {
			return {
				ctor: '::',
				_0: x,
				_1: A2(_elm_community$list_extra$List_Extra$iterate, f, _p91._0)
			};
		} else {
			return {
				ctor: '::',
				_0: x,
				_1: {ctor: '[]'}
			};
		}
	});
var _elm_community$list_extra$List_Extra$getAt = F2(
	function (idx, xs) {
		return (_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, idx, xs));
	});
var _elm_community$list_extra$List_Extra_ops = _elm_community$list_extra$List_Extra_ops || {};
_elm_community$list_extra$List_Extra_ops['!!'] = _elm_lang$core$Basics$flip(_elm_community$list_extra$List_Extra$getAt);
var _elm_community$list_extra$List_Extra$init = function () {
	var maybe = F2(
		function (d, f) {
			return function (_p92) {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					d,
					A2(_elm_lang$core$Maybe$map, f, _p92));
			};
		});
	return A2(
		_elm_lang$core$List$foldr,
		function (x) {
			return function (_p93) {
				return _elm_lang$core$Maybe$Just(
					A3(
						maybe,
						{ctor: '[]'},
						F2(
							function (x, y) {
								return {ctor: '::', _0: x, _1: y};
							})(x),
						_p93));
			};
		},
		_elm_lang$core$Maybe$Nothing);
}();
var _elm_community$list_extra$List_Extra$last = _elm_community$list_extra$List_Extra$foldl1(
	_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));

var _elm_community$maybe_extra$Maybe_Extra$foldrValues = F2(
	function (item, list) {
		var _p0 = item;
		if (_p0.ctor === 'Nothing') {
			return list;
		} else {
			return {ctor: '::', _0: _p0._0, _1: list};
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$values = A2(
	_elm_lang$core$List$foldr,
	_elm_community$maybe_extra$Maybe_Extra$foldrValues,
	{ctor: '[]'});
var _elm_community$maybe_extra$Maybe_Extra$filter = F2(
	function (f, m) {
		var _p1 = A2(_elm_lang$core$Maybe$map, f, m);
		if ((_p1.ctor === 'Just') && (_p1._0 === true)) {
			return m;
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$traverseArray = function (f) {
	var step = F2(
		function (e, acc) {
			var _p2 = f(e);
			if (_p2.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return A2(
					_elm_lang$core$Maybe$map,
					_elm_lang$core$Array$push(_p2._0),
					acc);
			}
		});
	return A2(
		_elm_lang$core$Array$foldl,
		step,
		_elm_lang$core$Maybe$Just(_elm_lang$core$Array$empty));
};
var _elm_community$maybe_extra$Maybe_Extra$combineArray = _elm_community$maybe_extra$Maybe_Extra$traverseArray(_elm_lang$core$Basics$identity);
var _elm_community$maybe_extra$Maybe_Extra$traverse = function (f) {
	var step = F2(
		function (e, acc) {
			var _p3 = f(e);
			if (_p3.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return A2(
					_elm_lang$core$Maybe$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(_p3._0),
					acc);
			}
		});
	return A2(
		_elm_lang$core$List$foldr,
		step,
		_elm_lang$core$Maybe$Just(
			{ctor: '[]'}));
};
var _elm_community$maybe_extra$Maybe_Extra$combine = _elm_community$maybe_extra$Maybe_Extra$traverse(_elm_lang$core$Basics$identity);
var _elm_community$maybe_extra$Maybe_Extra$toArray = function (m) {
	var _p4 = m;
	if (_p4.ctor === 'Nothing') {
		return _elm_lang$core$Array$empty;
	} else {
		return A2(_elm_lang$core$Array$repeat, 1, _p4._0);
	}
};
var _elm_community$maybe_extra$Maybe_Extra$toList = function (m) {
	var _p5 = m;
	if (_p5.ctor === 'Nothing') {
		return {ctor: '[]'};
	} else {
		return {
			ctor: '::',
			_0: _p5._0,
			_1: {ctor: '[]'}
		};
	}
};
var _elm_community$maybe_extra$Maybe_Extra$orElse = F2(
	function (ma, mb) {
		var _p6 = mb;
		if (_p6.ctor === 'Nothing') {
			return ma;
		} else {
			return mb;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$orElseLazy = F2(
	function (fma, mb) {
		var _p7 = mb;
		if (_p7.ctor === 'Nothing') {
			return fma(
				{ctor: '_Tuple0'});
		} else {
			return mb;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$orLazy = F2(
	function (ma, fmb) {
		var _p8 = ma;
		if (_p8.ctor === 'Nothing') {
			return fmb(
				{ctor: '_Tuple0'});
		} else {
			return ma;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$or = F2(
	function (ma, mb) {
		var _p9 = ma;
		if (_p9.ctor === 'Nothing') {
			return mb;
		} else {
			return ma;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$prev = _elm_lang$core$Maybe$map2(_elm_lang$core$Basics$always);
var _elm_community$maybe_extra$Maybe_Extra$next = _elm_lang$core$Maybe$map2(
	_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));
var _elm_community$maybe_extra$Maybe_Extra$andMap = _elm_lang$core$Maybe$map2(
	F2(
		function (x, y) {
			return y(x);
		}));
var _elm_community$maybe_extra$Maybe_Extra$unpack = F3(
	function (d, f, m) {
		var _p10 = m;
		if (_p10.ctor === 'Nothing') {
			return d(
				{ctor: '_Tuple0'});
		} else {
			return f(_p10._0);
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$unwrap = F3(
	function (d, f, m) {
		var _p11 = m;
		if (_p11.ctor === 'Nothing') {
			return d;
		} else {
			return f(_p11._0);
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$isJust = function (m) {
	var _p12 = m;
	if (_p12.ctor === 'Nothing') {
		return false;
	} else {
		return true;
	}
};
var _elm_community$maybe_extra$Maybe_Extra$isNothing = function (m) {
	var _p13 = m;
	if (_p13.ctor === 'Nothing') {
		return true;
	} else {
		return false;
	}
};
var _elm_community$maybe_extra$Maybe_Extra$join = function (mx) {
	var _p14 = mx;
	if (_p14.ctor === 'Just') {
		return _p14._0;
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_community$maybe_extra$Maybe_Extra_ops = _elm_community$maybe_extra$Maybe_Extra_ops || {};
_elm_community$maybe_extra$Maybe_Extra_ops['?'] = F2(
	function (mx, x) {
		return A2(_elm_lang$core$Maybe$withDefault, x, mx);
	});

//import Maybe, Native.List //

var _elm_lang$core$Native_Regex = function() {

function escape(str)
{
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}
function caseInsensitive(re)
{
	return new RegExp(re.source, 'gi');
}
function regex(raw)
{
	return new RegExp(raw, 'g');
}

function contains(re, string)
{
	return string.match(re) !== null;
}

function find(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex === re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		out.push({
			match: result[0],
			submatches: _elm_lang$core$Native_List.fromArray(subs),
			index: result.index,
			number: number
		});
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

function replace(n, re, replacer, string)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		return replacer({
			match: match,
			submatches: _elm_lang$core$Native_List.fromArray(submatches),
			index: arguments[arguments.length - 2],
			number: count
		});
	}
	return string.replace(re, jsReplacer);
}

function split(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	if (n === Infinity)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(re));
	}
	var string = str;
	var result;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		if (!(result = re.exec(string))) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

return {
	regex: regex,
	caseInsensitive: caseInsensitive,
	escape: escape,

	contains: F2(contains),
	find: F3(find),
	replace: F4(replace),
	split: F3(split)
};

}();

var _elm_lang$core$Regex$split = _elm_lang$core$Native_Regex.split;
var _elm_lang$core$Regex$replace = _elm_lang$core$Native_Regex.replace;
var _elm_lang$core$Regex$find = _elm_lang$core$Native_Regex.find;
var _elm_lang$core$Regex$contains = _elm_lang$core$Native_Regex.contains;
var _elm_lang$core$Regex$caseInsensitive = _elm_lang$core$Native_Regex.caseInsensitive;
var _elm_lang$core$Regex$regex = _elm_lang$core$Native_Regex.regex;
var _elm_lang$core$Regex$escape = _elm_lang$core$Native_Regex.escape;
var _elm_lang$core$Regex$Match = F4(
	function (a, b, c, d) {
		return {match: a, submatches: b, index: c, number: d};
	});
var _elm_lang$core$Regex$Regex = {ctor: 'Regex'};
var _elm_lang$core$Regex$AtMost = function (a) {
	return {ctor: 'AtMost', _0: a};
};
var _elm_lang$core$Regex$All = {ctor: 'All'};

var _elm_lang$core$Native_Bitwise = function() {

return {
	and: F2(function and(a, b) { return a & b; }),
	or: F2(function or(a, b) { return a | b; }),
	xor: F2(function xor(a, b) { return a ^ b; }),
	complement: function complement(a) { return ~a; },
	shiftLeftBy: F2(function(offset, a) { return a << offset; }),
	shiftRightBy: F2(function(offset, a) { return a >> offset; }),
	shiftRightZfBy: F2(function(offset, a) { return a >>> offset; })
};

}();

var _elm_lang$core$Bitwise$shiftRightZfBy = _elm_lang$core$Native_Bitwise.shiftRightZfBy;
var _elm_lang$core$Bitwise$shiftRightBy = _elm_lang$core$Native_Bitwise.shiftRightBy;
var _elm_lang$core$Bitwise$shiftLeftBy = _elm_lang$core$Native_Bitwise.shiftLeftBy;
var _elm_lang$core$Bitwise$complement = _elm_lang$core$Native_Bitwise.complement;
var _elm_lang$core$Bitwise$xor = _elm_lang$core$Native_Bitwise.xor;
var _elm_lang$core$Bitwise$or = _elm_lang$core$Native_Bitwise.or;
var _elm_lang$core$Bitwise$and = _elm_lang$core$Native_Bitwise.and;

var _elm_community$string_extra$String_Extra$accentRegex = function () {
	var matches = {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: '[à-æ]', _1: 'a'},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: '[À-Æ]', _1: 'A'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'ç', _1: 'c'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'Ç', _1: 'C'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: '[è-ë]', _1: 'e'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: '[È-Ë]', _1: 'E'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: '[ì-ï]', _1: 'i'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: '[Ì-Ï]', _1: 'I'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'ñ', _1: 'n'},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'Ñ', _1: 'N'},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: '[ò-ö]', _1: 'o'},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: '[Ò-Ö]', _1: 'O'},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: '[ù-ü]', _1: 'u'},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: '[Ù-Ü]', _1: 'U'},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'ý', _1: 'y'},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'ÿ', _1: 'y'},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Ý', _1: 'Y'},
																		_1: {ctor: '[]'}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	};
	return A2(
		_elm_lang$core$List$map,
		function (_p0) {
			var _p1 = _p0;
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Regex$regex(_p1._0),
				_1: _p1._1
			};
		},
		matches);
}();
var _elm_community$string_extra$String_Extra$removeAccents = function (string) {
	if (_elm_lang$core$String$isEmpty(string)) {
		return string;
	} else {
		var do_regex_to_remove_acents = function (_p2) {
			var _p3 = _p2;
			return A3(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_p3._0,
				function (_p4) {
					return _p3._1;
				});
		};
		return A3(_elm_lang$core$List$foldl, do_regex_to_remove_acents, string, _elm_community$string_extra$String_Extra$accentRegex);
	}
};
var _elm_community$string_extra$String_Extra$nonEmpty = function (string) {
	return _elm_lang$core$String$isEmpty(string) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(string);
};
var _elm_community$string_extra$String_Extra$replacementCodePoint = 65533;
var _elm_community$string_extra$String_Extra$toCodePoints = function (string) {
	var allCodeUnits = A2(
		_elm_lang$core$List$map,
		_elm_lang$core$Char$toCode,
		_elm_lang$core$String$toList(string));
	var combineAndReverse = F2(
		function (codeUnits, accumulated) {
			combineAndReverse:
			while (true) {
				var _p5 = codeUnits;
				if (_p5.ctor === '[]') {
					return accumulated;
				} else {
					var _p9 = _p5._0;
					var _p8 = _p5._1;
					if ((_elm_lang$core$Native_Utils.cmp(_p9, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 55295) < 1)) {
						var _v3 = _p8,
							_v4 = {ctor: '::', _0: _p9, _1: accumulated};
						codeUnits = _v3;
						accumulated = _v4;
						continue combineAndReverse;
					} else {
						if ((_elm_lang$core$Native_Utils.cmp(_p9, 55296) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 56319) < 1)) {
							var _p6 = _p8;
							if (_p6.ctor === '[]') {
								return {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
							} else {
								var _p7 = _p6._0;
								if ((_elm_lang$core$Native_Utils.cmp(_p7, 56320) > -1) && (_elm_lang$core$Native_Utils.cmp(_p7, 57343) < 1)) {
									var codePoint = (65536 + ((_p9 - 55296) * 1024)) + (_p7 - 56320);
									var _v6 = _p6._1,
										_v7 = {ctor: '::', _0: codePoint, _1: accumulated};
									codeUnits = _v6;
									accumulated = _v7;
									continue combineAndReverse;
								} else {
									var _v8 = _p8,
										_v9 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
									codeUnits = _v8;
									accumulated = _v9;
									continue combineAndReverse;
								}
							}
						} else {
							if ((_elm_lang$core$Native_Utils.cmp(_p9, 57344) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 65535) < 1)) {
								var _v10 = _p8,
									_v11 = {ctor: '::', _0: _p9, _1: accumulated};
								codeUnits = _v10;
								accumulated = _v11;
								continue combineAndReverse;
							} else {
								var _v12 = _p8,
									_v13 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
								codeUnits = _v12;
								accumulated = _v13;
								continue combineAndReverse;
							}
						}
					}
				}
			}
		});
	return _elm_lang$core$List$reverse(
		A2(
			combineAndReverse,
			allCodeUnits,
			{ctor: '[]'}));
};
var _elm_community$string_extra$String_Extra$fromCodePoints = function (allCodePoints) {
	var splitAndReverse = F2(
		function (codePoints, accumulated) {
			splitAndReverse:
			while (true) {
				var _p10 = codePoints;
				if (_p10.ctor === '[]') {
					return accumulated;
				} else {
					var _p12 = _p10._1;
					var _p11 = _p10._0;
					if ((_elm_lang$core$Native_Utils.cmp(_p11, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 55295) < 1)) {
						var _v15 = _p12,
							_v16 = {ctor: '::', _0: _p11, _1: accumulated};
						codePoints = _v15;
						accumulated = _v16;
						continue splitAndReverse;
					} else {
						if ((_elm_lang$core$Native_Utils.cmp(_p11, 65536) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 1114111) < 1)) {
							var subtracted = _p11 - 65536;
							var leading = (subtracted >> 10) + 55296;
							var trailing = (subtracted & 1023) + 56320;
							var _v17 = _p12,
								_v18 = {
								ctor: '::',
								_0: trailing,
								_1: {ctor: '::', _0: leading, _1: accumulated}
							};
							codePoints = _v17;
							accumulated = _v18;
							continue splitAndReverse;
						} else {
							if ((_elm_lang$core$Native_Utils.cmp(_p11, 57344) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 65535) < 1)) {
								var _v19 = _p12,
									_v20 = {ctor: '::', _0: _p11, _1: accumulated};
								codePoints = _v19;
								accumulated = _v20;
								continue splitAndReverse;
							} else {
								var _v21 = _p12,
									_v22 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
								codePoints = _v21;
								accumulated = _v22;
								continue splitAndReverse;
							}
						}
					}
				}
			}
		});
	var allCodeUnits = _elm_lang$core$List$reverse(
		A2(
			splitAndReverse,
			allCodePoints,
			{ctor: '[]'}));
	return _elm_lang$core$String$fromList(
		A2(_elm_lang$core$List$map, _elm_lang$core$Char$fromCode, allCodeUnits));
};
var _elm_community$string_extra$String_Extra$fromFloat = _elm_lang$core$Basics$toString;
var _elm_community$string_extra$String_Extra$fromInt = _elm_lang$core$Basics$toString;
var _elm_community$string_extra$String_Extra$leftOfBack = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				A2(_elm_lang$core$Basics$flip, _elm_lang$core$String$left, string),
				_elm_lang$core$List$head(
					_elm_lang$core$List$reverse(
						A2(_elm_lang$core$String$indexes, pattern, string)))));
	});
var _elm_community$string_extra$String_Extra$rightOfBack = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (_p13) {
					return A3(
						_elm_lang$core$Basics$flip,
						_elm_lang$core$String$dropLeft,
						string,
						A2(
							F2(
								function (x, y) {
									return x + y;
								}),
							_elm_lang$core$String$length(pattern),
							_p13));
				},
				_elm_lang$core$List$head(
					_elm_lang$core$List$reverse(
						A2(_elm_lang$core$String$indexes, pattern, string)))));
	});
var _elm_community$string_extra$String_Extra$firstResultHelp = F2(
	function ($default, list) {
		firstResultHelp:
		while (true) {
			var _p14 = list;
			if (_p14.ctor === '[]') {
				return $default;
			} else {
				if (_p14._0.ctor === 'Just') {
					return _p14._0._0;
				} else {
					var _v24 = $default,
						_v25 = _p14._1;
					$default = _v24;
					list = _v25;
					continue firstResultHelp;
				}
			}
		}
	});
var _elm_community$string_extra$String_Extra$firstResult = function (list) {
	return A2(_elm_community$string_extra$String_Extra$firstResultHelp, '', list);
};
var _elm_community$string_extra$String_Extra$leftOf = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$String$join,
			'',
			A2(
				_elm_lang$core$List$map,
				function (_p15) {
					return _elm_community$string_extra$String_Extra$firstResult(
						function (_) {
							return _.submatches;
						}(_p15));
				},
				A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'^(.*?)',
							_elm_lang$core$Regex$escape(pattern))),
					string)));
	});
var _elm_community$string_extra$String_Extra$rightOf = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$String$join,
			'',
			A2(
				_elm_lang$core$List$map,
				function (_p16) {
					return _elm_community$string_extra$String_Extra$firstResult(
						function (_) {
							return _.submatches;
						}(_p16));
				},
				A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Regex$escape(pattern),
							'(.*)$')),
					string)));
	});
var _elm_community$string_extra$String_Extra$pluralize = F3(
	function (singular, plural, count) {
		return _elm_lang$core$Native_Utils.eq(count, 1) ? A2(_elm_lang$core$Basics_ops['++'], '1 ', singular) : A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Basics$toString(count),
			A2(_elm_lang$core$Basics_ops['++'], ' ', plural));
	});
var _elm_community$string_extra$String_Extra$stripTags = function (string) {
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('<\\/?[^>]+>'),
		_elm_lang$core$Basics$always(''),
		string);
};
var _elm_community$string_extra$String_Extra$toSentenceHelper = F3(
	function (lastPart, sentence, list) {
		toSentenceHelper:
		while (true) {
			var _p17 = list;
			if (_p17.ctor === '[]') {
				return sentence;
			} else {
				if (_p17._1.ctor === '[]') {
					return A2(
						_elm_lang$core$Basics_ops['++'],
						sentence,
						A2(_elm_lang$core$Basics_ops['++'], lastPart, _p17._0));
				} else {
					var _v27 = lastPart,
						_v28 = A2(
						_elm_lang$core$Basics_ops['++'],
						sentence,
						A2(_elm_lang$core$Basics_ops['++'], ', ', _p17._0)),
						_v29 = _p17._1;
					lastPart = _v27;
					sentence = _v28;
					list = _v29;
					continue toSentenceHelper;
				}
			}
		}
	});
var _elm_community$string_extra$String_Extra$toSentenceBaseCase = function (list) {
	var _p18 = list;
	_v30_2:
	do {
		if (_p18.ctor === '::') {
			if (_p18._1.ctor === '[]') {
				return _p18._0;
			} else {
				if (_p18._1._1.ctor === '[]') {
					return A2(
						_elm_lang$core$Basics_ops['++'],
						_p18._0,
						A2(_elm_lang$core$Basics_ops['++'], ' and ', _p18._1._0));
				} else {
					break _v30_2;
				}
			}
		} else {
			break _v30_2;
		}
	} while(false);
	return '';
};
var _elm_community$string_extra$String_Extra$toSentenceOxford = function (list) {
	var _p19 = list;
	if (((_p19.ctor === '::') && (_p19._1.ctor === '::')) && (_p19._1._1.ctor === '::')) {
		return A3(
			_elm_community$string_extra$String_Extra$toSentenceHelper,
			', and ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_p19._0,
				A2(_elm_lang$core$Basics_ops['++'], ', ', _p19._1._0)),
			{ctor: '::', _0: _p19._1._1._0, _1: _p19._1._1._1});
	} else {
		return _elm_community$string_extra$String_Extra$toSentenceBaseCase(list);
	}
};
var _elm_community$string_extra$String_Extra$toSentence = function (list) {
	var _p20 = list;
	if (((_p20.ctor === '::') && (_p20._1.ctor === '::')) && (_p20._1._1.ctor === '::')) {
		return A3(
			_elm_community$string_extra$String_Extra$toSentenceHelper,
			' and ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_p20._0,
				A2(_elm_lang$core$Basics_ops['++'], ', ', _p20._1._0)),
			{ctor: '::', _0: _p20._1._1._0, _1: _p20._1._1._1});
	} else {
		return _elm_community$string_extra$String_Extra$toSentenceBaseCase(list);
	}
};
var _elm_community$string_extra$String_Extra$ellipsisWith = F3(
	function (howLong, append, string) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$String$length(string),
			howLong) < 1) ? string : A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$String$left,
				howLong - _elm_lang$core$String$length(append),
				string),
			append);
	});
var _elm_community$string_extra$String_Extra$ellipsis = F2(
	function (howLong, string) {
		return A3(_elm_community$string_extra$String_Extra$ellipsisWith, howLong, '...', string);
	});
var _elm_community$string_extra$String_Extra$countOccurrences = F2(
	function (needle, haystack) {
		return (_elm_lang$core$Native_Utils.eq(
			_elm_lang$core$String$length(needle),
			0) || _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$String$length(haystack),
			0)) ? 0 : _elm_lang$core$List$length(
			A2(_elm_lang$core$String$indexes, needle, haystack));
	});
var _elm_community$string_extra$String_Extra$unindent = function (multilineSting) {
	var isNotWhitespace = function ($char) {
		return (!_elm_lang$core$Native_Utils.eq(
			$char,
			_elm_lang$core$Native_Utils.chr(' '))) && (!_elm_lang$core$Native_Utils.eq(
			$char,
			_elm_lang$core$Native_Utils.chr('\t')));
	};
	var countLeadingWhitespace = F2(
		function (count, line) {
			countLeadingWhitespace:
			while (true) {
				var _p21 = _elm_lang$core$String$uncons(line);
				if (_p21.ctor === 'Nothing') {
					return count;
				} else {
					var _p23 = _p21._0._1;
					var _p22 = _p21._0._0;
					switch (_p22.valueOf()) {
						case ' ':
							var _v35 = count + 1,
								_v36 = _p23;
							count = _v35;
							line = _v36;
							continue countLeadingWhitespace;
						case '\t':
							var _v37 = count + 1,
								_v38 = _p23;
							count = _v37;
							line = _v38;
							continue countLeadingWhitespace;
						default:
							return count;
					}
				}
			}
		});
	var lines = _elm_lang$core$String$lines(multilineSting);
	var minLead = A2(
		_elm_lang$core$Maybe$withDefault,
		0,
		_elm_lang$core$List$minimum(
			A2(
				_elm_lang$core$List$map,
				countLeadingWhitespace(0),
				A2(
					_elm_lang$core$List$filter,
					_elm_lang$core$String$any(isNotWhitespace),
					lines))));
	return A2(
		_elm_lang$core$String$join,
		'\n',
		A2(
			_elm_lang$core$List$map,
			_elm_lang$core$String$dropLeft(minLead),
			lines));
};
var _elm_community$string_extra$String_Extra$dasherize = function (string) {
	return _elm_lang$core$String$toLower(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('[_-\\s]+'),
			_elm_lang$core$Basics$always('-'),
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([A-Z])'),
				function (_p24) {
					return A2(
						_elm_lang$core$String$append,
						'-',
						function (_) {
							return _.match;
						}(_p24));
				},
				_elm_lang$core$String$trim(string))));
};
var _elm_community$string_extra$String_Extra$underscored = function (string) {
	return _elm_lang$core$String$toLower(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('[_-\\s]+'),
			_elm_lang$core$Basics$always('_'),
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([a-z\\d])([A-Z]+)'),
				function (_p25) {
					return A2(
						_elm_lang$core$String$join,
						'_',
						A2(
							_elm_lang$core$List$filterMap,
							_elm_lang$core$Basics$identity,
							function (_) {
								return _.submatches;
							}(_p25)));
				},
				_elm_lang$core$String$trim(string))));
};
var _elm_community$string_extra$String_Extra$unsurround = F2(
	function (wrap, string) {
		if (A2(_elm_lang$core$String$startsWith, wrap, string) && A2(_elm_lang$core$String$endsWith, wrap, string)) {
			var length = _elm_lang$core$String$length(wrap);
			return A2(
				_elm_lang$core$String$dropRight,
				length,
				A2(_elm_lang$core$String$dropLeft, length, string));
		} else {
			return string;
		}
	});
var _elm_community$string_extra$String_Extra$unquote = function (string) {
	return A2(_elm_community$string_extra$String_Extra$unsurround, '\"', string);
};
var _elm_community$string_extra$String_Extra$surround = F2(
	function (wrap, string) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			wrap,
			A2(_elm_lang$core$Basics_ops['++'], string, wrap));
	});
var _elm_community$string_extra$String_Extra$quote = function (string) {
	return A2(_elm_community$string_extra$String_Extra$surround, '\"', string);
};
var _elm_community$string_extra$String_Extra$camelize = function (string) {
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('[-_\\s]+(.)?'),
		function (_p26) {
			var _p27 = _p26;
			var _p28 = _p27.submatches;
			if ((_p28.ctor === '::') && (_p28._0.ctor === 'Just')) {
				return _elm_lang$core$String$toUpper(_p28._0._0);
			} else {
				return '';
			}
		},
		_elm_lang$core$String$trim(string));
};
var _elm_community$string_extra$String_Extra$isBlank = function (string) {
	return A2(
		_elm_lang$core$Regex$contains,
		_elm_lang$core$Regex$regex('^\\s*$'),
		string);
};
var _elm_community$string_extra$String_Extra$clean = function (string) {
	return _elm_lang$core$String$trim(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('\\s\\s+'),
			_elm_lang$core$Basics$always(' '),
			string));
};
var _elm_community$string_extra$String_Extra$softBreakRegexp = function (width) {
	return _elm_lang$core$Regex$regex(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'.{1,',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(width),
				'}(\\s+|$)|\\S+?(\\s+|$)')));
};
var _elm_community$string_extra$String_Extra$softEllipsis = F2(
	function (howLong, string) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$String$length(string),
			howLong) < 1) ? string : A3(
			_elm_lang$core$Basics$flip,
			_elm_lang$core$String$append,
			'...',
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([\\.,;:\\s])+$'),
				_elm_lang$core$Basics$always(''),
				A2(
					_elm_lang$core$String$join,
					'',
					A2(
						_elm_lang$core$List$map,
						function (_) {
							return _.match;
						},
						A3(
							_elm_lang$core$Regex$find,
							_elm_lang$core$Regex$AtMost(1),
							_elm_community$string_extra$String_Extra$softBreakRegexp(howLong),
							string)))));
	});
var _elm_community$string_extra$String_Extra$softBreak = F2(
	function (width, string) {
		return (_elm_lang$core$Native_Utils.cmp(width, 0) < 1) ? {ctor: '[]'} : A2(
			_elm_lang$core$List$map,
			function (_) {
				return _.match;
			},
			A3(
				_elm_lang$core$Regex$find,
				_elm_lang$core$Regex$All,
				_elm_community$string_extra$String_Extra$softBreakRegexp(width),
				string));
	});
var _elm_community$string_extra$String_Extra$softWrapWith = F3(
	function (width, separator, string) {
		return A2(
			_elm_lang$core$String$join,
			separator,
			A2(_elm_community$string_extra$String_Extra$softBreak, width, string));
	});
var _elm_community$string_extra$String_Extra$softWrap = F2(
	function (width, string) {
		return A3(_elm_community$string_extra$String_Extra$softWrapWith, width, '\n', string);
	});
var _elm_community$string_extra$String_Extra$breaker = F3(
	function (width, string, acc) {
		breaker:
		while (true) {
			var _p29 = string;
			if (_p29 === '') {
				return _elm_lang$core$List$reverse(acc);
			} else {
				var _v42 = width,
					_v43 = A2(_elm_lang$core$String$dropLeft, width, string),
					_v44 = {
					ctor: '::',
					_0: A3(_elm_lang$core$String$slice, 0, width, string),
					_1: acc
				};
				width = _v42;
				string = _v43;
				acc = _v44;
				continue breaker;
			}
		}
	});
var _elm_community$string_extra$String_Extra$break = F2(
	function (width, string) {
		return (_elm_lang$core$Native_Utils.eq(width, 0) || _elm_lang$core$Native_Utils.eq(string, '')) ? {
			ctor: '::',
			_0: string,
			_1: {ctor: '[]'}
		} : A3(
			_elm_community$string_extra$String_Extra$breaker,
			width,
			string,
			{ctor: '[]'});
	});
var _elm_community$string_extra$String_Extra$wrapWith = F3(
	function (width, separator, string) {
		return A2(
			_elm_lang$core$String$join,
			separator,
			A2(_elm_community$string_extra$String_Extra$break, width, string));
	});
var _elm_community$string_extra$String_Extra$wrap = F2(
	function (width, string) {
		return A3(_elm_community$string_extra$String_Extra$wrapWith, width, '\n', string);
	});
var _elm_community$string_extra$String_Extra$replaceSlice = F4(
	function (substitution, start, end, string) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			A3(_elm_lang$core$String$slice, 0, start, string),
			A2(
				_elm_lang$core$Basics_ops['++'],
				substitution,
				A3(
					_elm_lang$core$String$slice,
					end,
					_elm_lang$core$String$length(string),
					string)));
	});
var _elm_community$string_extra$String_Extra$insertAt = F3(
	function (insert, pos, string) {
		return A4(_elm_community$string_extra$String_Extra$replaceSlice, insert, pos, pos, string);
	});
var _elm_community$string_extra$String_Extra$replace = F3(
	function (search, substitution, string) {
		return A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex(
				_elm_lang$core$Regex$escape(search)),
			function (_p30) {
				return substitution;
			},
			string);
	});
var _elm_community$string_extra$String_Extra$changeCase = F2(
	function (mutator, word) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (_p31) {
					var _p32 = _p31;
					return A2(
						_elm_lang$core$String$cons,
						mutator(_p32._0),
						_p32._1);
				},
				_elm_lang$core$String$uncons(word)));
	});
var _elm_community$string_extra$String_Extra$toSentenceCase = function (word) {
	return A2(_elm_community$string_extra$String_Extra$changeCase, _elm_lang$core$Char$toUpper, word);
};
var _elm_community$string_extra$String_Extra$toTitleCase = function (ws) {
	var uppercaseMatch = A3(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('\\w+'),
		function (_p33) {
			return _elm_community$string_extra$String_Extra$toSentenceCase(
				function (_) {
					return _.match;
				}(_p33));
		});
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('^([a-z])|\\s+([a-z])'),
		function (_p34) {
			return uppercaseMatch(
				function (_) {
					return _.match;
				}(_p34));
		},
		ws);
};
var _elm_community$string_extra$String_Extra$classify = function (string) {
	return _elm_community$string_extra$String_Extra$toSentenceCase(
		A3(
			_elm_community$string_extra$String_Extra$replace,
			' ',
			'',
			_elm_community$string_extra$String_Extra$camelize(
				A4(
					_elm_lang$core$Regex$replace,
					_elm_lang$core$Regex$All,
					_elm_lang$core$Regex$regex('[\\W_]'),
					_elm_lang$core$Basics$always(' '),
					string))));
};
var _elm_community$string_extra$String_Extra$humanize = function (string) {
	return _elm_community$string_extra$String_Extra$toSentenceCase(
		_elm_lang$core$String$toLower(
			_elm_lang$core$String$trim(
				A4(
					_elm_lang$core$Regex$replace,
					_elm_lang$core$Regex$All,
					_elm_lang$core$Regex$regex('_id$|[-_\\s]+'),
					_elm_lang$core$Basics$always(' '),
					A4(
						_elm_lang$core$Regex$replace,
						_elm_lang$core$Regex$All,
						_elm_lang$core$Regex$regex('[A-Z]'),
						function (_p35) {
							return A2(
								_elm_lang$core$String$append,
								'-',
								function (_) {
									return _.match;
								}(_p35));
						},
						string)))));
};
var _elm_community$string_extra$String_Extra$decapitalize = function (word) {
	return A2(_elm_community$string_extra$String_Extra$changeCase, _elm_lang$core$Char$toLower, word);
};

var _elm_lang$dom$Dom$blur = _elm_lang$dom$Native_Dom.blur;
var _elm_lang$dom$Dom$focus = _elm_lang$dom$Native_Dom.focus;
var _elm_lang$dom$Dom$NotFound = function (a) {
	return {ctor: 'NotFound', _0: a};
};

var _elm_lang$dom$Dom_Size$width = _elm_lang$dom$Native_Dom.width;
var _elm_lang$dom$Dom_Size$height = _elm_lang$dom$Native_Dom.height;
var _elm_lang$dom$Dom_Size$VisibleContentWithBordersAndMargins = {ctor: 'VisibleContentWithBordersAndMargins'};
var _elm_lang$dom$Dom_Size$VisibleContentWithBorders = {ctor: 'VisibleContentWithBorders'};
var _elm_lang$dom$Dom_Size$VisibleContent = {ctor: 'VisibleContent'};
var _elm_lang$dom$Dom_Size$Content = {ctor: 'Content'};

var _elm_lang$dom$Dom_Scroll$toX = _elm_lang$dom$Native_Dom.setScrollLeft;
var _elm_lang$dom$Dom_Scroll$x = _elm_lang$dom$Native_Dom.getScrollLeft;
var _elm_lang$dom$Dom_Scroll$toRight = _elm_lang$dom$Native_Dom.toRight;
var _elm_lang$dom$Dom_Scroll$toLeft = function (id) {
	return A2(_elm_lang$dom$Dom_Scroll$toX, id, 0);
};
var _elm_lang$dom$Dom_Scroll$toY = _elm_lang$dom$Native_Dom.setScrollTop;
var _elm_lang$dom$Dom_Scroll$y = _elm_lang$dom$Native_Dom.getScrollTop;
var _elm_lang$dom$Dom_Scroll$toBottom = _elm_lang$dom$Native_Dom.toBottom;
var _elm_lang$dom$Dom_Scroll$toTop = function (id) {
	return A2(_elm_lang$dom$Dom_Scroll$toY, id, 0);
};

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$virtual_dom$Native_Debug = function() {


// IMPORT / EXPORT

function unsafeCoerce(value)
{
	return value;
}

var upload = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	var element = document.createElement('input');
	element.setAttribute('type', 'file');
	element.setAttribute('accept', 'text/json');
	element.style.display = 'none';
	element.addEventListener('change', function(event)
	{
		var fileReader = new FileReader();
		fileReader.onload = function(e)
		{
			callback(_elm_lang$core$Native_Scheduler.succeed(e.target.result));
		};
		fileReader.readAsText(event.target.files[0]);
		document.body.removeChild(element);
	});
	document.body.appendChild(element);
	element.click();
});

function download(historyLength, json)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var fileName = 'history-' + historyLength + '.txt';
		var jsonString = JSON.stringify(json);
		var mime = 'text/plain;charset=utf-8';
		var done = _elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0);

		// for IE10+
		if (navigator.msSaveBlob)
		{
			navigator.msSaveBlob(new Blob([jsonString], {type: mime}), fileName);
			return callback(done);
		}

		// for HTML5
		var element = document.createElement('a');
		element.setAttribute('href', 'data:' + mime + ',' + encodeURIComponent(jsonString));
		element.setAttribute('download', fileName);
		element.style.display = 'none';
		document.body.appendChild(element);
		element.click();
		document.body.removeChild(element);
		callback(done);
	});
}


// POPOUT

function messageToString(value)
{
	switch (typeof value)
	{
		case 'boolean':
			return value ? 'True' : 'False';
		case 'number':
			return value + '';
		case 'string':
			return '"' + addSlashes(value, false) + '"';
	}
	if (value instanceof String)
	{
		return '\'' + addSlashes(value, true) + '\'';
	}
	if (typeof value !== 'object' || value === null || !('ctor' in value))
	{
		return '…';
	}

	var ctorStarter = value.ctor.substring(0, 5);
	if (ctorStarter === '_Tupl' || ctorStarter === '_Task')
	{
		return '…'
	}
	if (['_Array', '<decoder>', '_Process', '::', '[]', 'Set_elm_builtin', 'RBNode_elm_builtin', 'RBEmpty_elm_builtin'].indexOf(value.ctor) >= 0)
	{
		return '…';
	}

	var keys = Object.keys(value);
	switch (keys.length)
	{
		case 1:
			return value.ctor;
		case 2:
			return value.ctor + ' ' + messageToString(value._0);
		default:
			return value.ctor + ' … ' + messageToString(value[keys[keys.length - 1]]);
	}
}


function primitive(str)
{
	return { ctor: 'Primitive', _0: str };
}


function init(value)
{
	var type = typeof value;

	if (type === 'boolean')
	{
		return {
			ctor: 'Constructor',
			_0: _elm_lang$core$Maybe$Just(value ? 'True' : 'False'),
			_1: true,
			_2: _elm_lang$core$Native_List.Nil
		};
	}

	if (type === 'number')
	{
		return primitive(value + '');
	}

	if (type === 'string')
	{
		return { ctor: 'S', _0: '"' + addSlashes(value, false) + '"' };
	}

	if (value instanceof String)
	{
		return { ctor: 'S', _0: "'" + addSlashes(value, true) + "'" };
	}

	if (value instanceof Date)
	{
		return primitive('<' + value.toString() + '>');
	}

	if (value === null)
	{
		return primitive('XXX');
	}

	if (type === 'object' && 'ctor' in value)
	{
		var ctor = value.ctor;

		if (ctor === '::' || ctor === '[]')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'ListSeq'},
				_1: true,
				_2: A2(_elm_lang$core$List$map, init, value)
			};
		}

		if (ctor === 'Set_elm_builtin')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'SetSeq'},
				_1: true,
				_2: A3(_elm_lang$core$Set$foldr, initCons, _elm_lang$core$Native_List.Nil, value)
			};
		}

		if (ctor === 'RBNode_elm_builtin' || ctor == 'RBEmpty_elm_builtin')
		{
			return {
				ctor: 'Dictionary',
				_0: true,
				_1: A3(_elm_lang$core$Dict$foldr, initKeyValueCons, _elm_lang$core$Native_List.Nil, value)
			};
		}

		if (ctor === '_Array')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'ArraySeq'},
				_1: true,
				_2: A3(_elm_lang$core$Array$foldr, initCons, _elm_lang$core$Native_List.Nil, value)
			};
		}

		var ctorStarter = value.ctor.substring(0, 5);
		if (ctorStarter === '_Task')
		{
			return primitive('<task>');
		}

		if (ctor === '<decoder>')
		{
			return primitive(ctor);
		}

		if (ctor === '_Process')
		{
			return primitive('<process>');
		}

		var list = _elm_lang$core$Native_List.Nil;
		for (var i in value)
		{
			if (i === 'ctor') continue;
			list = _elm_lang$core$Native_List.Cons(init(value[i]), list);
		}
		return {
			ctor: 'Constructor',
			_0: ctorStarter === '_Tupl' ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(ctor),
			_1: true,
			_2: _elm_lang$core$List$reverse(list)
		};
	}

	if (type === 'object')
	{
		var dict = _elm_lang$core$Dict$empty;
		for (var i in value)
		{
			dict = A3(_elm_lang$core$Dict$insert, i, init(value[i]), dict);
		}
		return { ctor: 'Record', _0: true, _1: dict };
	}

	return primitive('XXX');
}

var initCons = F2(initConsHelp);

function initConsHelp(value, list)
{
	return _elm_lang$core$Native_List.Cons(init(value), list);
}

var initKeyValueCons = F3(initKeyValueConsHelp);

function initKeyValueConsHelp(key, value, list)
{
	return _elm_lang$core$Native_List.Cons(
		_elm_lang$core$Native_Utils.Tuple2(init(key), init(value)),
		list
	);
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	upload: upload,
	download: F2(download),
	unsafeCoerce: unsafeCoerce,
	messageToString: messageToString,
	init: init
}

}();

var _elm_lang$virtual_dom$VirtualDom_Helpers$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom_Helpers$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom_Helpers$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom_Helpers$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom_Helpers$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom_Helpers$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom_Helpers$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom_Helpers$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom_Helpers$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom_Helpers$onClick = function (msg) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$virtual_dom$VirtualDom_Helpers$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom_Helpers$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom_Helpers$id = _elm_lang$virtual_dom$VirtualDom_Helpers$attribute('id');
var _elm_lang$virtual_dom$VirtualDom_Helpers$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom_Helpers$class = function (name) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$property,
		'className',
		_elm_lang$core$Json_Encode$string(name));
};
var _elm_lang$virtual_dom$VirtualDom_Helpers$href = function (name) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$property,
		'href',
		_elm_lang$core$Json_Encode$string(name));
};
var _elm_lang$virtual_dom$VirtualDom_Helpers$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom_Helpers$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom_Helpers$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom_Helpers$div = _elm_lang$virtual_dom$VirtualDom_Helpers$node('div');
var _elm_lang$virtual_dom$VirtualDom_Helpers$span = _elm_lang$virtual_dom$VirtualDom_Helpers$node('span');
var _elm_lang$virtual_dom$VirtualDom_Helpers$a = _elm_lang$virtual_dom$VirtualDom_Helpers$node('a');
var _elm_lang$virtual_dom$VirtualDom_Helpers$h1 = _elm_lang$virtual_dom$VirtualDom_Helpers$node('h1');
var _elm_lang$virtual_dom$VirtualDom_Helpers$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Helpers$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom_Helpers$Property = {ctor: 'Property'};

var _elm_lang$virtual_dom$VirtualDom_Expando$purple = _elm_lang$virtual_dom$VirtualDom_Helpers$style(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'color', _1: 'rgb(136, 19, 145)'},
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$blue = _elm_lang$virtual_dom$VirtualDom_Helpers$style(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'color', _1: 'rgb(28, 0, 207)'},
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$red = _elm_lang$virtual_dom$VirtualDom_Helpers$style(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'color', _1: 'rgb(196, 26, 22)'},
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$leftPad = function (maybeKey) {
	var _p0 = maybeKey;
	if (_p0.ctor === 'Nothing') {
		return _elm_lang$virtual_dom$VirtualDom_Helpers$style(
			{ctor: '[]'});
	} else {
		return _elm_lang$virtual_dom$VirtualDom_Helpers$style(
			{
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'padding-left', _1: '4ch'},
				_1: {ctor: '[]'}
			});
	}
};
var _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow = function (arrow) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$span,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$style(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'color', _1: '#777'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'padding-left', _1: '2ch'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'width', _1: '2ch'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'display', _1: 'inline-block'},
								_1: {ctor: '[]'}
							}
						}
					}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(arrow),
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Expando$lineStarter = F3(
	function (maybeKey, maybeIsClosed, description) {
		var arrow = function () {
			var _p1 = maybeIsClosed;
			if (_p1.ctor === 'Nothing') {
				return _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow('');
			} else {
				if (_p1._0 === true) {
					return _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow('▸');
				} else {
					return _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow('▾');
				}
			}
		}();
		var _p2 = maybeKey;
		if (_p2.ctor === 'Nothing') {
			return {ctor: '::', _0: arrow, _1: description};
		} else {
			return {
				ctor: '::',
				_0: arrow,
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Expando$purple,
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p2._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' = '),
						_1: description
					}
				}
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTinyRecord = F3(
	function (length, starter, entries) {
		var _p3 = entries;
		if (_p3.ctor === '[]') {
			return {
				ctor: '_Tuple2',
				_0: length + 1,
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('}'),
					_1: {ctor: '[]'}
				}
			};
		} else {
			var _p5 = _p3._0;
			var nextLength = (length + _elm_lang$core$String$length(_p5)) + 1;
			if (_elm_lang$core$Native_Utils.cmp(nextLength, 18) > 0) {
				return {
					ctor: '_Tuple2',
					_0: length + 2,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('…}'),
						_1: {ctor: '[]'}
					}
				};
			} else {
				var _p4 = A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTinyRecord, nextLength, ',', _p3._1);
				var finalLength = _p4._0;
				var otherNodes = _p4._1;
				return {
					ctor: '_Tuple2',
					_0: finalLength,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$purple,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p5),
									_1: {ctor: '[]'}
								}),
							_1: otherNodes
						}
					}
				};
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$elideMiddle = function (str) {
	return (_elm_lang$core$Native_Utils.cmp(
		_elm_lang$core$String$length(str),
		18) < 1) ? str : A2(
		_elm_lang$core$Basics_ops['++'],
		A2(_elm_lang$core$String$left, 8, str),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'...',
			A2(_elm_lang$core$String$right, 8, str)));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp = function (str) {
	return {
		ctor: '_Tuple2',
		_0: _elm_lang$core$String$length(str),
		_1: {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(str),
			_1: {ctor: '[]'}
		}
	};
};
var _elm_lang$virtual_dom$VirtualDom_Expando$updateIndex = F3(
	function (n, func, list) {
		var _p6 = list;
		if (_p6.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p8 = _p6._1;
			var _p7 = _p6._0;
			return (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) ? {
				ctor: '::',
				_0: func(_p7),
				_1: _p8
			} : {
				ctor: '::',
				_0: _p7,
				_1: A3(_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex, n - 1, func, _p8)
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$seqTypeToString = F2(
	function (n, seqType) {
		var _p9 = seqType;
		switch (_p9.ctor) {
			case 'ListSeq':
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'List(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(n),
						')'));
			case 'SetSeq':
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'Set(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(n),
						')'));
			default:
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'Array(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(n),
						')'));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTiny = function (value) {
	var _p10 = value;
	switch (_p10.ctor) {
		case 'S':
			var str = _elm_lang$virtual_dom$VirtualDom_Expando$elideMiddle(_p10._0);
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$String$length(str),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Expando$red,
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(str),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			};
		case 'Primitive':
			var _p11 = _p10._0;
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$String$length(_p11),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Expando$blue,
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p11),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			};
		case 'Sequence':
			return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
				A2(
					_elm_lang$virtual_dom$VirtualDom_Expando$seqTypeToString,
					_elm_lang$core$List$length(_p10._2),
					_p10._0));
		case 'Dictionary':
			return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'Dict(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(
							_elm_lang$core$List$length(_p10._1)),
						')')));
		case 'Record':
			return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecord(_p10._1);
		default:
			if (_p10._2.ctor === '[]') {
				return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
					A2(_elm_lang$core$Maybe$withDefault, 'Unit', _p10._0));
			} else {
				return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
					function () {
						var _p12 = _p10._0;
						if (_p12.ctor === 'Nothing') {
							return A2(
								_elm_lang$core$Basics_ops['++'],
								'Tuple(',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(
										_elm_lang$core$List$length(_p10._2)),
									')'));
						} else {
							return A2(_elm_lang$core$Basics_ops['++'], _p12._0, ' …');
						}
					}());
			}
	}
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecord = function (record) {
	return _elm_lang$core$Dict$isEmpty(record) ? {
		ctor: '_Tuple2',
		_0: 2,
		_1: {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('{}'),
			_1: {ctor: '[]'}
		}
	} : A3(
		_elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecordHelp,
		0,
		'{ ',
		_elm_lang$core$Dict$toList(record));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecordHelp = F3(
	function (length, starter, entries) {
		var _p13 = entries;
		if (_p13.ctor === '[]') {
			return {
				ctor: '_Tuple2',
				_0: length + 2,
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' }'),
					_1: {ctor: '[]'}
				}
			};
		} else {
			var _p16 = _p13._0._0;
			var _p14 = _elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTiny(_p13._0._1);
			var valueLen = _p14._0;
			var valueNodes = _p14._1;
			var fieldLen = _elm_lang$core$String$length(_p16);
			var newLength = ((length + fieldLen) + valueLen) + 5;
			if (_elm_lang$core$Native_Utils.cmp(newLength, 60) > 0) {
				return {
					ctor: '_Tuple2',
					_0: length + 4,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', … }'),
						_1: {ctor: '[]'}
					}
				};
			} else {
				var _p15 = A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecordHelp, newLength, ', ', _p13._1);
				var finalLength = _p15._0;
				var otherNodes = _p15._1;
				return {
					ctor: '_Tuple2',
					_0: finalLength,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$purple,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p16),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' = '),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$virtual_dom$VirtualDom_Helpers$span,
										{ctor: '[]'},
										valueNodes),
									_1: otherNodes
								}
							}
						}
					}
				};
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTiny = function (value) {
	var _p17 = value;
	if (_p17.ctor === 'Record') {
		return A3(
			_elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTinyRecord,
			0,
			'{',
			_elm_lang$core$Dict$keys(_p17._1));
	} else {
		return _elm_lang$virtual_dom$VirtualDom_Expando$viewTiny(value);
	}
};
var _elm_lang$virtual_dom$VirtualDom_Expando$Constructor = F3(
	function (a, b, c) {
		return {ctor: 'Constructor', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Record = F2(
	function (a, b) {
		return {ctor: 'Record', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Dictionary = F2(
	function (a, b) {
		return {ctor: 'Dictionary', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Sequence = F3(
	function (a, b, c) {
		return {ctor: 'Sequence', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$initHelp = F2(
	function (isOuter, expando) {
		var _p18 = expando;
		switch (_p18.ctor) {
			case 'S':
				return expando;
			case 'Primitive':
				return expando;
			case 'Sequence':
				var _p20 = _p18._0;
				var _p19 = _p18._2;
				return isOuter ? A3(
					_elm_lang$virtual_dom$VirtualDom_Expando$Sequence,
					_p20,
					false,
					A2(
						_elm_lang$core$List$map,
						_elm_lang$virtual_dom$VirtualDom_Expando$initHelp(false),
						_p19)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(_p19),
					8) < 1) ? A3(_elm_lang$virtual_dom$VirtualDom_Expando$Sequence, _p20, false, _p19) : expando);
			case 'Dictionary':
				var _p23 = _p18._1;
				return isOuter ? A2(
					_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary,
					false,
					A2(
						_elm_lang$core$List$map,
						function (_p21) {
							var _p22 = _p21;
							return {
								ctor: '_Tuple2',
								_0: _p22._0,
								_1: A2(_elm_lang$virtual_dom$VirtualDom_Expando$initHelp, false, _p22._1)
							};
						},
						_p23)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(_p23),
					8) < 1) ? A2(_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary, false, _p23) : expando);
			case 'Record':
				var _p25 = _p18._1;
				return isOuter ? A2(
					_elm_lang$virtual_dom$VirtualDom_Expando$Record,
					false,
					A2(
						_elm_lang$core$Dict$map,
						F2(
							function (_p24, v) {
								return A2(_elm_lang$virtual_dom$VirtualDom_Expando$initHelp, false, v);
							}),
						_p25)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$Dict$size(_p25),
					4) < 1) ? A2(_elm_lang$virtual_dom$VirtualDom_Expando$Record, false, _p25) : expando);
			default:
				var _p27 = _p18._0;
				var _p26 = _p18._2;
				return isOuter ? A3(
					_elm_lang$virtual_dom$VirtualDom_Expando$Constructor,
					_p27,
					false,
					A2(
						_elm_lang$core$List$map,
						_elm_lang$virtual_dom$VirtualDom_Expando$initHelp(false),
						_p26)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(_p26),
					4) < 1) ? A3(_elm_lang$virtual_dom$VirtualDom_Expando$Constructor, _p27, false, _p26) : expando);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$init = function (value) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Expando$initHelp,
		true,
		_elm_lang$virtual_dom$Native_Debug.init(value));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp = F2(
	function (old, $new) {
		var _p28 = {ctor: '_Tuple2', _0: old, _1: $new};
		_v12_6:
		do {
			if (_p28.ctor === '_Tuple2') {
				switch (_p28._1.ctor) {
					case 'S':
						return $new;
					case 'Primitive':
						return $new;
					case 'Sequence':
						if (_p28._0.ctor === 'Sequence') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Sequence,
								_p28._1._0,
								_p28._0._1,
								A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp, _p28._0._2, _p28._1._2));
						} else {
							break _v12_6;
						}
					case 'Dictionary':
						if (_p28._0.ctor === 'Dictionary') {
							return A2(_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary, _p28._0._0, _p28._1._1);
						} else {
							break _v12_6;
						}
					case 'Record':
						if (_p28._0.ctor === 'Record') {
							return A2(
								_elm_lang$virtual_dom$VirtualDom_Expando$Record,
								_p28._0._0,
								A2(
									_elm_lang$core$Dict$map,
									_elm_lang$virtual_dom$VirtualDom_Expando$mergeDictHelp(_p28._0._1),
									_p28._1._1));
						} else {
							break _v12_6;
						}
					default:
						if (_p28._0.ctor === 'Constructor') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Constructor,
								_p28._1._0,
								_p28._0._1,
								A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp, _p28._0._2, _p28._1._2));
						} else {
							break _v12_6;
						}
				}
			} else {
				break _v12_6;
			}
		} while(false);
		return $new;
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$mergeDictHelp = F3(
	function (oldDict, key, value) {
		var _p29 = A2(_elm_lang$core$Dict$get, key, oldDict);
		if (_p29.ctor === 'Nothing') {
			return value;
		} else {
			return A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp, _p29._0, value);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp = F2(
	function (olds, news) {
		var _p30 = {ctor: '_Tuple2', _0: olds, _1: news};
		if (_p30._0.ctor === '[]') {
			return news;
		} else {
			if (_p30._1.ctor === '[]') {
				return news;
			} else {
				return {
					ctor: '::',
					_0: A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp, _p30._0._0, _p30._1._0),
					_1: A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp, _p30._0._1, _p30._1._1)
				};
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$merge = F2(
	function (value, expando) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp,
			expando,
			_elm_lang$virtual_dom$Native_Debug.init(value));
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$update = F2(
	function (msg, value) {
		var _p31 = value;
		switch (_p31.ctor) {
			case 'S':
				return _elm_lang$core$Native_Utils.crashCase(
					'VirtualDom.Expando',
					{
						start: {line: 168, column: 3},
						end: {line: 235, column: 50}
					},
					_p31)('No messages for primitives');
			case 'Primitive':
				return _elm_lang$core$Native_Utils.crashCase(
					'VirtualDom.Expando',
					{
						start: {line: 168, column: 3},
						end: {line: 235, column: 50}
					},
					_p31)('No messages for primitives');
			case 'Sequence':
				var _p39 = _p31._2;
				var _p38 = _p31._0;
				var _p37 = _p31._1;
				var _p34 = msg;
				switch (_p34.ctor) {
					case 'Toggle':
						return A3(_elm_lang$virtual_dom$VirtualDom_Expando$Sequence, _p38, !_p37, _p39);
					case 'Index':
						if (_p34._0.ctor === 'None') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Sequence,
								_p38,
								_p37,
								A3(
									_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
									_p34._1,
									_elm_lang$virtual_dom$VirtualDom_Expando$update(_p34._2),
									_p39));
						} else {
							return _elm_lang$core$Native_Utils.crashCase(
								'VirtualDom.Expando',
								{
									start: {line: 176, column: 7},
									end: {line: 188, column: 46}
								},
								_p34)('No redirected indexes on sequences');
						}
					default:
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 176, column: 7},
								end: {line: 188, column: 46}
							},
							_p34)('No field on sequences');
				}
			case 'Dictionary':
				var _p51 = _p31._1;
				var _p50 = _p31._0;
				var _p40 = msg;
				switch (_p40.ctor) {
					case 'Toggle':
						return A2(_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary, !_p50, _p51);
					case 'Index':
						var _p48 = _p40._2;
						var _p47 = _p40._1;
						var _p41 = _p40._0;
						switch (_p41.ctor) {
							case 'None':
								return _elm_lang$core$Native_Utils.crashCase(
									'VirtualDom.Expando',
									{
										start: {line: 196, column: 11},
										end: {line: 206, column: 81}
									},
									_p41)('must have redirect for dictionaries');
							case 'Key':
								return A2(
									_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary,
									_p50,
									A3(
										_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
										_p47,
										function (_p43) {
											var _p44 = _p43;
											return {
												ctor: '_Tuple2',
												_0: A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, _p48, _p44._0),
												_1: _p44._1
											};
										},
										_p51));
							default:
								return A2(
									_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary,
									_p50,
									A3(
										_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
										_p47,
										function (_p45) {
											var _p46 = _p45;
											return {
												ctor: '_Tuple2',
												_0: _p46._0,
												_1: A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, _p48, _p46._1)
											};
										},
										_p51));
						}
					default:
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 191, column: 7},
								end: {line: 209, column: 50}
							},
							_p40)('no field for dictionaries');
				}
			case 'Record':
				var _p55 = _p31._1;
				var _p54 = _p31._0;
				var _p52 = msg;
				switch (_p52.ctor) {
					case 'Toggle':
						return A2(_elm_lang$virtual_dom$VirtualDom_Expando$Record, !_p54, _p55);
					case 'Index':
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 212, column: 7},
								end: {line: 220, column: 77}
							},
							_p52)('No index for records');
					default:
						return A2(
							_elm_lang$virtual_dom$VirtualDom_Expando$Record,
							_p54,
							A3(
								_elm_lang$core$Dict$update,
								_p52._0,
								_elm_lang$virtual_dom$VirtualDom_Expando$updateField(_p52._1),
								_p55));
				}
			default:
				var _p61 = _p31._2;
				var _p60 = _p31._0;
				var _p59 = _p31._1;
				var _p56 = msg;
				switch (_p56.ctor) {
					case 'Toggle':
						return A3(_elm_lang$virtual_dom$VirtualDom_Expando$Constructor, _p60, !_p59, _p61);
					case 'Index':
						if (_p56._0.ctor === 'None') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Constructor,
								_p60,
								_p59,
								A3(
									_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
									_p56._1,
									_elm_lang$virtual_dom$VirtualDom_Expando$update(_p56._2),
									_p61));
						} else {
							return _elm_lang$core$Native_Utils.crashCase(
								'VirtualDom.Expando',
								{
									start: {line: 223, column: 7},
									end: {line: 235, column: 50}
								},
								_p56)('No redirected indexes on sequences');
						}
					default:
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 223, column: 7},
								end: {line: 235, column: 50}
							},
							_p56)('No field for constructors');
				}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$updateField = F2(
	function (msg, maybeExpando) {
		var _p62 = maybeExpando;
		if (_p62.ctor === 'Nothing') {
			return _elm_lang$core$Native_Utils.crashCase(
				'VirtualDom.Expando',
				{
					start: {line: 253, column: 3},
					end: {line: 258, column: 32}
				},
				_p62)('key does not exist');
		} else {
			return _elm_lang$core$Maybe$Just(
				A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, msg, _p62._0));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Primitive = function (a) {
	return {ctor: 'Primitive', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Expando$S = function (a) {
	return {ctor: 'S', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Expando$ArraySeq = {ctor: 'ArraySeq'};
var _elm_lang$virtual_dom$VirtualDom_Expando$SetSeq = {ctor: 'SetSeq'};
var _elm_lang$virtual_dom$VirtualDom_Expando$ListSeq = {ctor: 'ListSeq'};
var _elm_lang$virtual_dom$VirtualDom_Expando$Field = F2(
	function (a, b) {
		return {ctor: 'Field', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Index = F3(
	function (a, b, c) {
		return {ctor: 'Index', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Toggle = {ctor: 'Toggle'};
var _elm_lang$virtual_dom$VirtualDom_Expando$Value = {ctor: 'Value'};
var _elm_lang$virtual_dom$VirtualDom_Expando$Key = {ctor: 'Key'};
var _elm_lang$virtual_dom$VirtualDom_Expando$None = {ctor: 'None'};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorEntry = F2(
	function (index, value) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$map,
			A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, index),
			A2(
				_elm_lang$virtual_dom$VirtualDom_Expando$view,
				_elm_lang$core$Maybe$Just(
					_elm_lang$core$Basics$toString(index)),
				value));
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$view = F2(
	function (maybeKey, expando) {
		var _p64 = expando;
		switch (_p64.ctor) {
			case 'S':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Nothing,
						{
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$red,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p64._0),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}));
			case 'Primitive':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Nothing,
						{
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$blue,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p64._0),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}));
			case 'Sequence':
				return A4(_elm_lang$virtual_dom$VirtualDom_Expando$viewSequence, maybeKey, _p64._0, _p64._1, _p64._2);
			case 'Dictionary':
				return A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewDictionary, maybeKey, _p64._0, _p64._1);
			case 'Record':
				return A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewRecord, maybeKey, _p64._0, _p64._1);
			default:
				return A4(_elm_lang$virtual_dom$VirtualDom_Expando$viewConstructor, maybeKey, _p64._0, _p64._1, _p64._2);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructor = F4(
	function (maybeKey, maybeName, isClosed, valueList) {
		var _p65 = function () {
			var _p66 = valueList;
			if (_p66.ctor === '[]') {
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Maybe$Nothing,
					_1: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$div,
						{ctor: '[]'},
						{ctor: '[]'})
				};
			} else {
				if (_p66._1.ctor === '[]') {
					var _p67 = _p66._0;
					switch (_p67.ctor) {
						case 'S':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Nothing,
								_1: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'})
							};
						case 'Primitive':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Nothing,
								_1: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'})
							};
						case 'Sequence':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewSequenceOpen(_p67._2))
							};
						case 'Dictionary':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryOpen(_p67._1))
							};
						case 'Record':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewRecordOpen(_p67._1))
							};
						default:
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorOpen(_p67._2))
							};
					}
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(isClosed),
						_1: isClosed ? A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$div,
							{ctor: '[]'},
							{ctor: '[]'}) : _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorOpen(valueList)
					};
				}
			}
		}();
		var maybeIsClosed = _p65._0;
		var openHtml = _p65._1;
		var tinyArgs = A2(
			_elm_lang$core$List$map,
			function (_p68) {
				return _elm_lang$core$Tuple$second(
					_elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTiny(_p68));
			},
			valueList);
		var description = function () {
			var _p69 = {ctor: '_Tuple2', _0: maybeName, _1: tinyArgs};
			if (_p69._0.ctor === 'Nothing') {
				if (_p69._1.ctor === '[]') {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('()'),
						_1: {ctor: '[]'}
					};
				} else {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('( '),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{ctor: '[]'},
								_p69._1._0),
							_1: A3(
								_elm_lang$core$List$foldr,
								F2(
									function (args, rest) {
										return {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', '),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$virtual_dom$VirtualDom_Helpers$span,
													{ctor: '[]'},
													args),
												_1: rest
											}
										};
									}),
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' )'),
									_1: {ctor: '[]'}
								},
								_p69._1._1)
						}
					};
				}
			} else {
				if (_p69._1.ctor === '[]') {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p69._0._0),
						_1: {ctor: '[]'}
					};
				} else {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
							A2(_elm_lang$core$Basics_ops['++'], _p69._0._0, ' ')),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{ctor: '[]'},
								_p69._1._0),
							_1: A3(
								_elm_lang$core$List$foldr,
								F2(
									function (args, rest) {
										return {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' '),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$virtual_dom$VirtualDom_Helpers$span,
													{ctor: '[]'},
													args),
												_1: rest
											}
										};
									}),
								{ctor: '[]'},
								_p69._1._1)
						}
					};
				}
			}
		}();
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter, maybeKey, maybeIsClosed, description)),
				_1: {
					ctor: '::',
					_0: openHtml,
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorOpen = function (valueList) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(_elm_lang$core$List$indexedMap, _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorEntry, valueList));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryOpen = function (keyValuePairs) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(_elm_lang$core$List$indexedMap, _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryEntry, keyValuePairs));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryEntry = F2(
	function (index, _p70) {
		var _p71 = _p70;
		var _p74 = _p71._1;
		var _p73 = _p71._0;
		var _p72 = _p73;
		switch (_p72.ctor) {
			case 'S':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$map,
					A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Value, index),
					A2(
						_elm_lang$virtual_dom$VirtualDom_Expando$view,
						_elm_lang$core$Maybe$Just(_p72._0),
						_p74));
			case 'Primitive':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$map,
					A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Value, index),
					A2(
						_elm_lang$virtual_dom$VirtualDom_Expando$view,
						_elm_lang$core$Maybe$Just(_p72._0),
						_p74));
			default:
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$map,
							A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Key, index),
							A2(
								_elm_lang$virtual_dom$VirtualDom_Expando$view,
								_elm_lang$core$Maybe$Just('key'),
								_p73)),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$map,
								A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Value, index),
								A2(
									_elm_lang$virtual_dom$VirtualDom_Expando$view,
									_elm_lang$core$Maybe$Just('value'),
									_p74)),
							_1: {ctor: '[]'}
						}
					});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewRecordOpen = function (record) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(
			_elm_lang$core$List$map,
			_elm_lang$virtual_dom$VirtualDom_Expando$viewRecordEntry,
			_elm_lang$core$Dict$toList(record)));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewRecordEntry = function (_p75) {
	var _p76 = _p75;
	var _p77 = _p76._0;
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$map,
		_elm_lang$virtual_dom$VirtualDom_Expando$Field(_p77),
		A2(
			_elm_lang$virtual_dom$VirtualDom_Expando$view,
			_elm_lang$core$Maybe$Just(_p77),
			_p76._1));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewSequenceOpen = function (values) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(_elm_lang$core$List$indexedMap, _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorEntry, values));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionary = F3(
	function (maybeKey, isClosed, keyValuePairs) {
		var starter = A2(
			_elm_lang$core$Basics_ops['++'],
			'Dict(',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(
					_elm_lang$core$List$length(keyValuePairs)),
				')'));
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Just(isClosed),
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
							_1: {ctor: '[]'}
						})),
				_1: {
					ctor: '::',
					_0: isClosed ? _elm_lang$virtual_dom$VirtualDom_Helpers$text('') : _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryOpen(keyValuePairs),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewRecord = F3(
	function (maybeKey, isClosed, record) {
		var _p78 = isClosed ? {
			ctor: '_Tuple3',
			_0: _elm_lang$core$Tuple$second(
				_elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecord(record)),
			_1: _elm_lang$virtual_dom$VirtualDom_Helpers$text(''),
			_2: _elm_lang$virtual_dom$VirtualDom_Helpers$text('')
		} : {
			ctor: '_Tuple3',
			_0: {
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('{'),
				_1: {ctor: '[]'}
			},
			_1: _elm_lang$virtual_dom$VirtualDom_Expando$viewRecordOpen(record),
			_2: A2(
				_elm_lang$virtual_dom$VirtualDom_Helpers$div,
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(
						_elm_lang$core$Maybe$Just(
							{ctor: '_Tuple0'})),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('}'),
					_1: {ctor: '[]'}
				})
		};
		var start = _p78._0;
		var middle = _p78._1;
		var end = _p78._2;
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Just(isClosed),
						start)),
				_1: {
					ctor: '::',
					_0: middle,
					_1: {
						ctor: '::',
						_0: end,
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewSequence = F4(
	function (maybeKey, seqType, isClosed, valueList) {
		var starter = A2(
			_elm_lang$virtual_dom$VirtualDom_Expando$seqTypeToString,
			_elm_lang$core$List$length(valueList),
			seqType);
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Just(isClosed),
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
							_1: {ctor: '[]'}
						})),
				_1: {
					ctor: '::',
					_0: isClosed ? _elm_lang$virtual_dom$VirtualDom_Helpers$text('') : _elm_lang$virtual_dom$VirtualDom_Expando$viewSequenceOpen(valueList),
					_1: {ctor: '[]'}
				}
			});
	});

var _elm_lang$virtual_dom$VirtualDom_Report$some = function (list) {
	return !_elm_lang$core$List$isEmpty(list);
};
var _elm_lang$virtual_dom$VirtualDom_Report$TagChanges = F4(
	function (a, b, c, d) {
		return {removed: a, changed: b, added: c, argsMatch: d};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$emptyTagChanges = function (argsMatch) {
	return A4(
		_elm_lang$virtual_dom$VirtualDom_Report$TagChanges,
		{ctor: '[]'},
		{ctor: '[]'},
		{ctor: '[]'},
		argsMatch);
};
var _elm_lang$virtual_dom$VirtualDom_Report$hasTagChanges = function (tagChanges) {
	return _elm_lang$core$Native_Utils.eq(
		tagChanges,
		A4(
			_elm_lang$virtual_dom$VirtualDom_Report$TagChanges,
			{ctor: '[]'},
			{ctor: '[]'},
			{ctor: '[]'},
			true));
};
var _elm_lang$virtual_dom$VirtualDom_Report$SomethingChanged = function (a) {
	return {ctor: 'SomethingChanged', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Report$MessageChanged = F2(
	function (a, b) {
		return {ctor: 'MessageChanged', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$VersionChanged = F2(
	function (a, b) {
		return {ctor: 'VersionChanged', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$CorruptHistory = {ctor: 'CorruptHistory'};
var _elm_lang$virtual_dom$VirtualDom_Report$UnionChange = F2(
	function (a, b) {
		return {ctor: 'UnionChange', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$AliasChange = function (a) {
	return {ctor: 'AliasChange', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Report$Fine = {ctor: 'Fine'};
var _elm_lang$virtual_dom$VirtualDom_Report$Risky = {ctor: 'Risky'};
var _elm_lang$virtual_dom$VirtualDom_Report$Impossible = {ctor: 'Impossible'};
var _elm_lang$virtual_dom$VirtualDom_Report$worstCase = F2(
	function (status, statusList) {
		worstCase:
		while (true) {
			var _p0 = statusList;
			if (_p0.ctor === '[]') {
				return status;
			} else {
				switch (_p0._0.ctor) {
					case 'Impossible':
						return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
					case 'Risky':
						var _v1 = _elm_lang$virtual_dom$VirtualDom_Report$Risky,
							_v2 = _p0._1;
						status = _v1;
						statusList = _v2;
						continue worstCase;
					default:
						var _v3 = status,
							_v4 = _p0._1;
						status = _v3;
						statusList = _v4;
						continue worstCase;
				}
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Report$evaluateChange = function (change) {
	var _p1 = change;
	if (_p1.ctor === 'AliasChange') {
		return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
	} else {
		return ((!_p1._1.argsMatch) || (_elm_lang$virtual_dom$VirtualDom_Report$some(_p1._1.changed) || _elm_lang$virtual_dom$VirtualDom_Report$some(_p1._1.removed))) ? _elm_lang$virtual_dom$VirtualDom_Report$Impossible : (_elm_lang$virtual_dom$VirtualDom_Report$some(_p1._1.added) ? _elm_lang$virtual_dom$VirtualDom_Report$Risky : _elm_lang$virtual_dom$VirtualDom_Report$Fine);
	}
};
var _elm_lang$virtual_dom$VirtualDom_Report$evaluate = function (report) {
	var _p2 = report;
	switch (_p2.ctor) {
		case 'CorruptHistory':
			return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
		case 'VersionChanged':
			return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
		case 'MessageChanged':
			return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
		default:
			return A2(
				_elm_lang$virtual_dom$VirtualDom_Report$worstCase,
				_elm_lang$virtual_dom$VirtualDom_Report$Fine,
				A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Report$evaluateChange, _p2._0));
	}
};

var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict = F2(
	function (f, dict) {
		return _elm_lang$core$Json_Encode$object(
			_elm_lang$core$Dict$toList(
				A2(
					_elm_lang$core$Dict$map,
					F2(
						function (key, value) {
							return f(value);
						}),
					dict)));
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeUnion = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'args',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p1.args))
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'tags',
					_1: A2(
						_elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict,
						function (_p2) {
							return _elm_lang$core$Json_Encode$list(
								A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p2));
						},
						_p1.tags)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeAlias = function (_p3) {
	var _p4 = _p3;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'args',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p4.args))
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string(_p4.tipe)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeTypes = function (_p5) {
	var _p6 = _p5;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'message',
				_1: _elm_lang$core$Json_Encode$string(_p6.message)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'aliases',
					_1: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict, _elm_lang$virtual_dom$VirtualDom_Metadata$encodeAlias, _p6.aliases)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'unions',
						_1: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict, _elm_lang$virtual_dom$VirtualDom_Metadata$encodeUnion, _p6.unions)
					},
					_1: {ctor: '[]'}
				}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeVersions = function (_p7) {
	var _p8 = _p7;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'elm',
				_1: _elm_lang$core$Json_Encode$string(_p8.elm)
			},
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encode = function (_p9) {
	var _p10 = _p9;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'versions',
				_1: _elm_lang$virtual_dom$VirtualDom_Metadata$encodeVersions(_p10.versions)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'types',
					_1: _elm_lang$virtual_dom$VirtualDom_Metadata$encodeTypes(_p10.types)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkTag = F4(
	function (tag, old, $new, changes) {
		return _elm_lang$core$Native_Utils.eq(old, $new) ? changes : _elm_lang$core$Native_Utils.update(
			changes,
			{
				changed: {ctor: '::', _0: tag, _1: changes.changed}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$addTag = F3(
	function (tag, _p11, changes) {
		return _elm_lang$core$Native_Utils.update(
			changes,
			{
				added: {ctor: '::', _0: tag, _1: changes.added}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$removeTag = F3(
	function (tag, _p12, changes) {
		return _elm_lang$core$Native_Utils.update(
			changes,
			{
				removed: {ctor: '::', _0: tag, _1: changes.removed}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkUnion = F4(
	function (name, old, $new, changes) {
		var tagChanges = A6(
			_elm_lang$core$Dict$merge,
			_elm_lang$virtual_dom$VirtualDom_Metadata$removeTag,
			_elm_lang$virtual_dom$VirtualDom_Metadata$checkTag,
			_elm_lang$virtual_dom$VirtualDom_Metadata$addTag,
			old.tags,
			$new.tags,
			_elm_lang$virtual_dom$VirtualDom_Report$emptyTagChanges(
				_elm_lang$core$Native_Utils.eq(old.args, $new.args)));
		return _elm_lang$virtual_dom$VirtualDom_Report$hasTagChanges(tagChanges) ? changes : {
			ctor: '::',
			_0: A2(_elm_lang$virtual_dom$VirtualDom_Report$UnionChange, name, tagChanges),
			_1: changes
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkAlias = F4(
	function (name, old, $new, changes) {
		return (_elm_lang$core$Native_Utils.eq(old.tipe, $new.tipe) && _elm_lang$core$Native_Utils.eq(old.args, $new.args)) ? changes : {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Report$AliasChange(name),
			_1: changes
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$ignore = F3(
	function (key, value, report) {
		return report;
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkTypes = F2(
	function (old, $new) {
		return (!_elm_lang$core$Native_Utils.eq(old.message, $new.message)) ? A2(_elm_lang$virtual_dom$VirtualDom_Report$MessageChanged, old.message, $new.message) : _elm_lang$virtual_dom$VirtualDom_Report$SomethingChanged(
			A6(
				_elm_lang$core$Dict$merge,
				_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
				_elm_lang$virtual_dom$VirtualDom_Metadata$checkUnion,
				_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
				old.unions,
				$new.unions,
				A6(
					_elm_lang$core$Dict$merge,
					_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
					_elm_lang$virtual_dom$VirtualDom_Metadata$checkAlias,
					_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
					old.aliases,
					$new.aliases,
					{ctor: '[]'})));
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$check = F2(
	function (old, $new) {
		return (!_elm_lang$core$Native_Utils.eq(old.versions.elm, $new.versions.elm)) ? A2(_elm_lang$virtual_dom$VirtualDom_Report$VersionChanged, old.versions.elm, $new.versions.elm) : A2(_elm_lang$virtual_dom$VirtualDom_Metadata$checkTypes, old.types, $new.types);
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$hasProblem = F2(
	function (tipe, _p13) {
		var _p14 = _p13;
		return A2(_elm_lang$core$String$contains, _p14._1, tipe) ? _elm_lang$core$Maybe$Just(_p14._0) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$Metadata = F2(
	function (a, b) {
		return {versions: a, types: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$Versions = function (a) {
	return {elm: a};
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeVersions = A2(
	_elm_lang$core$Json_Decode$map,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Versions,
	A2(_elm_lang$core$Json_Decode$field, 'elm', _elm_lang$core$Json_Decode$string));
var _elm_lang$virtual_dom$VirtualDom_Metadata$Types = F3(
	function (a, b, c) {
		return {message: a, aliases: b, unions: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$Alias = F2(
	function (a, b) {
		return {args: a, tipe: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeAlias = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Alias,
	A2(
		_elm_lang$core$Json_Decode$field,
		'args',
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string)),
	A2(_elm_lang$core$Json_Decode$field, 'type', _elm_lang$core$Json_Decode$string));
var _elm_lang$virtual_dom$VirtualDom_Metadata$Union = F2(
	function (a, b) {
		return {args: a, tags: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeUnion = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Union,
	A2(
		_elm_lang$core$Json_Decode$field,
		'args',
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string)),
	A2(
		_elm_lang$core$Json_Decode$field,
		'tags',
		_elm_lang$core$Json_Decode$dict(
			_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string))));
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeTypes = A4(
	_elm_lang$core$Json_Decode$map3,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Types,
	A2(_elm_lang$core$Json_Decode$field, 'message', _elm_lang$core$Json_Decode$string),
	A2(
		_elm_lang$core$Json_Decode$field,
		'aliases',
		_elm_lang$core$Json_Decode$dict(_elm_lang$virtual_dom$VirtualDom_Metadata$decodeAlias)),
	A2(
		_elm_lang$core$Json_Decode$field,
		'unions',
		_elm_lang$core$Json_Decode$dict(_elm_lang$virtual_dom$VirtualDom_Metadata$decodeUnion)));
var _elm_lang$virtual_dom$VirtualDom_Metadata$decoder = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Metadata,
	A2(_elm_lang$core$Json_Decode$field, 'versions', _elm_lang$virtual_dom$VirtualDom_Metadata$decodeVersions),
	A2(_elm_lang$core$Json_Decode$field, 'types', _elm_lang$virtual_dom$VirtualDom_Metadata$decodeTypes));
var _elm_lang$virtual_dom$VirtualDom_Metadata$Error = F2(
	function (a, b) {
		return {message: a, problems: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$ProblemType = F2(
	function (a, b) {
		return {name: a, problems: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$VirtualDom = {ctor: 'VirtualDom'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Program = {ctor: 'Program'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Request = {ctor: 'Request'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Socket = {ctor: 'Socket'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Process = {ctor: 'Process'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Task = {ctor: 'Task'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Decoder = {ctor: 'Decoder'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Function = {ctor: 'Function'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$problemTable = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Function, _1: '->'},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Decoder, _1: 'Json.Decode.Decoder'},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Task, _1: 'Task.Task'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Process, _1: 'Process.Id'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Socket, _1: 'WebSocket.LowLevel.WebSocket'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Request, _1: 'Http.Request'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Program, _1: 'Platform.Program'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$VirtualDom, _1: 'VirtualDom.Node'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$VirtualDom, _1: 'VirtualDom.Attribute'},
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$findProblems = function (tipe) {
	return A2(
		_elm_lang$core$List$filterMap,
		_elm_lang$virtual_dom$VirtualDom_Metadata$hasProblem(tipe),
		_elm_lang$virtual_dom$VirtualDom_Metadata$problemTable);
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$collectBadAliases = F3(
	function (name, _p15, list) {
		var _p16 = _p15;
		var _p17 = _elm_lang$virtual_dom$VirtualDom_Metadata$findProblems(_p16.tipe);
		if (_p17.ctor === '[]') {
			return list;
		} else {
			return {
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$ProblemType, name, _p17),
				_1: list
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$collectBadUnions = F3(
	function (name, _p18, list) {
		var _p19 = _p18;
		var _p20 = A2(
			_elm_lang$core$List$concatMap,
			_elm_lang$virtual_dom$VirtualDom_Metadata$findProblems,
			_elm_lang$core$List$concat(
				_elm_lang$core$Dict$values(_p19.tags)));
		if (_p20.ctor === '[]') {
			return list;
		} else {
			return {
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$ProblemType, name, _p20),
				_1: list
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$isPortable = function (_p21) {
	var _p22 = _p21;
	var _p24 = _p22.types;
	var badAliases = A3(
		_elm_lang$core$Dict$foldl,
		_elm_lang$virtual_dom$VirtualDom_Metadata$collectBadAliases,
		{ctor: '[]'},
		_p24.aliases);
	var _p23 = A3(_elm_lang$core$Dict$foldl, _elm_lang$virtual_dom$VirtualDom_Metadata$collectBadUnions, badAliases, _p24.unions);
	if (_p23.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			A2(_elm_lang$virtual_dom$VirtualDom_Metadata$Error, _p24.message, _p23));
	}
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$decode = function (value) {
	var _p25 = A2(_elm_lang$core$Json_Decode$decodeValue, _elm_lang$virtual_dom$VirtualDom_Metadata$decoder, value);
	if (_p25.ctor === 'Err') {
		return _elm_lang$core$Native_Utils.crashCase(
			'VirtualDom.Metadata',
			{
				start: {line: 229, column: 3},
				end: {line: 239, column: 20}
			},
			_p25)('Compiler is generating bad metadata. Report this at <https://github.com/elm-lang/virtual-dom/issues>.');
	} else {
		var _p28 = _p25._0;
		var _p27 = _elm_lang$virtual_dom$VirtualDom_Metadata$isPortable(_p28);
		if (_p27.ctor === 'Nothing') {
			return _elm_lang$core$Result$Ok(_p28);
		} else {
			return _elm_lang$core$Result$Err(_p27._0);
		}
	}
};

var _elm_lang$virtual_dom$VirtualDom_History$viewMessage = F3(
	function (currentIndex, index, msg) {
		var messageName = _elm_lang$virtual_dom$Native_Debug.messageToString(msg);
		var className = _elm_lang$core$Native_Utils.eq(currentIndex, index) ? 'messages-entry messages-entry-selected' : 'messages-entry';
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class(className),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$on,
						'click',
						_elm_lang$core$Json_Decode$succeed(index)),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$span,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('messages-entry-content'),
						_1: {
							ctor: '::',
							_0: A2(_elm_lang$virtual_dom$VirtualDom_Helpers$attribute, 'title', messageName),
							_1: {ctor: '[]'}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(messageName),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('messages-entry-index'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
								_elm_lang$core$Basics$toString(index)),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_History$consMsg = F3(
	function (currentIndex, msg, _p0) {
		var _p1 = _p0;
		var _p2 = _p1._0;
		return {
			ctor: '_Tuple2',
			_0: _p2 - 1,
			_1: {
				ctor: '::',
				_0: A4(_elm_lang$virtual_dom$VirtualDom_Helpers$lazy3, _elm_lang$virtual_dom$VirtualDom_History$viewMessage, currentIndex, _p2, msg),
				_1: _p1._1
			}
		};
	});
var _elm_lang$virtual_dom$VirtualDom_History$viewSnapshot = F3(
	function (currentIndex, index, _p3) {
		var _p4 = _p3;
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{ctor: '[]'},
			_elm_lang$core$Tuple$second(
				A3(
					_elm_lang$core$Array$foldl,
					_elm_lang$virtual_dom$VirtualDom_History$consMsg(currentIndex),
					{
						ctor: '_Tuple2',
						_0: index - 1,
						_1: {ctor: '[]'}
					},
					_p4.messages)));
	});
var _elm_lang$virtual_dom$VirtualDom_History$undone = function (getResult) {
	var _p5 = getResult;
	if (_p5.ctor === 'Done') {
		return {ctor: '_Tuple2', _0: _p5._1, _1: _p5._0};
	} else {
		return _elm_lang$core$Native_Utils.crashCase(
			'VirtualDom.History',
			{
				start: {line: 195, column: 3},
				end: {line: 200, column: 39}
			},
			_p5)('Bug in History.get');
	}
};
var _elm_lang$virtual_dom$VirtualDom_History$elmToJs = _elm_lang$virtual_dom$Native_Debug.unsafeCoerce;
var _elm_lang$virtual_dom$VirtualDom_History$encodeHelp = F2(
	function (snapshot, allMessages) {
		return A3(
			_elm_lang$core$Array$foldl,
			F2(
				function (elm, msgs) {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_History$elmToJs(elm),
						_1: msgs
					};
				}),
			allMessages,
			snapshot.messages);
	});
var _elm_lang$virtual_dom$VirtualDom_History$encode = function (_p7) {
	var _p8 = _p7;
	var recentJson = A2(
		_elm_lang$core$List$map,
		_elm_lang$virtual_dom$VirtualDom_History$elmToJs,
		_elm_lang$core$List$reverse(_p8.recent.messages));
	return _elm_lang$core$Json_Encode$list(
		A3(_elm_lang$core$Array$foldr, _elm_lang$virtual_dom$VirtualDom_History$encodeHelp, recentJson, _p8.snapshots));
};
var _elm_lang$virtual_dom$VirtualDom_History$jsToElm = _elm_lang$virtual_dom$Native_Debug.unsafeCoerce;
var _elm_lang$virtual_dom$VirtualDom_History$initialModel = function (_p9) {
	var _p10 = _p9;
	var _p11 = A2(_elm_lang$core$Array$get, 0, _p10.snapshots);
	if (_p11.ctor === 'Just') {
		return _p11._0.model;
	} else {
		return _p10.recent.model;
	}
};
var _elm_lang$virtual_dom$VirtualDom_History$size = function (history) {
	return history.numMessages;
};
var _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize = 64;
var _elm_lang$virtual_dom$VirtualDom_History$consSnapshot = F3(
	function (currentIndex, snapshot, _p12) {
		var _p13 = _p12;
		var _p14 = _p13._0;
		var nextIndex = _p14 - _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize;
		var currentIndexHelp = ((_elm_lang$core$Native_Utils.cmp(nextIndex, currentIndex) < 1) && (_elm_lang$core$Native_Utils.cmp(currentIndex, _p14) < 0)) ? currentIndex : -1;
		return {
			ctor: '_Tuple2',
			_0: _p14 - _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize,
			_1: {
				ctor: '::',
				_0: A4(_elm_lang$virtual_dom$VirtualDom_Helpers$lazy3, _elm_lang$virtual_dom$VirtualDom_History$viewSnapshot, currentIndexHelp, _p14, snapshot),
				_1: _p13._1
			}
		};
	});
var _elm_lang$virtual_dom$VirtualDom_History$viewSnapshots = F2(
	function (currentIndex, snapshots) {
		var highIndex = _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize * _elm_lang$core$Array$length(snapshots);
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{ctor: '[]'},
			_elm_lang$core$Tuple$second(
				A3(
					_elm_lang$core$Array$foldr,
					_elm_lang$virtual_dom$VirtualDom_History$consSnapshot(currentIndex),
					{
						ctor: '_Tuple2',
						_0: highIndex,
						_1: {ctor: '[]'}
					},
					snapshots)));
	});
var _elm_lang$virtual_dom$VirtualDom_History$view = F2(
	function (maybeIndex, _p15) {
		var _p16 = _p15;
		var _p17 = function () {
			var _p18 = maybeIndex;
			if (_p18.ctor === 'Nothing') {
				return {ctor: '_Tuple2', _0: -1, _1: 'debugger-sidebar-messages'};
			} else {
				return {ctor: '_Tuple2', _0: _p18._0, _1: 'debugger-sidebar-messages-paused'};
			}
		}();
		var index = _p17._0;
		var className = _p17._1;
		var oldStuff = A3(_elm_lang$virtual_dom$VirtualDom_Helpers$lazy2, _elm_lang$virtual_dom$VirtualDom_History$viewSnapshots, index, _p16.snapshots);
		var newStuff = _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				_elm_lang$virtual_dom$VirtualDom_History$consMsg(index),
				{
					ctor: '_Tuple2',
					_0: _p16.numMessages - 1,
					_1: {ctor: '[]'}
				},
				_p16.recent.messages));
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class(className),
				_1: {ctor: '[]'}
			},
			{ctor: '::', _0: oldStuff, _1: newStuff});
	});
var _elm_lang$virtual_dom$VirtualDom_History$History = F3(
	function (a, b, c) {
		return {snapshots: a, recent: b, numMessages: c};
	});
var _elm_lang$virtual_dom$VirtualDom_History$RecentHistory = F3(
	function (a, b, c) {
		return {model: a, messages: b, numMessages: c};
	});
var _elm_lang$virtual_dom$VirtualDom_History$empty = function (model) {
	return A3(
		_elm_lang$virtual_dom$VirtualDom_History$History,
		_elm_lang$core$Array$empty,
		A3(
			_elm_lang$virtual_dom$VirtualDom_History$RecentHistory,
			model,
			{ctor: '[]'},
			0),
		0);
};
var _elm_lang$virtual_dom$VirtualDom_History$Snapshot = F2(
	function (a, b) {
		return {model: a, messages: b};
	});
var _elm_lang$virtual_dom$VirtualDom_History$addRecent = F3(
	function (msg, newModel, _p19) {
		var _p20 = _p19;
		var _p23 = _p20.numMessages;
		var _p22 = _p20.model;
		var _p21 = _p20.messages;
		return _elm_lang$core$Native_Utils.eq(_p23, _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize) ? {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Maybe$Just(
				A2(
					_elm_lang$virtual_dom$VirtualDom_History$Snapshot,
					_p22,
					_elm_lang$core$Array$fromList(_p21))),
			_1: A3(
				_elm_lang$virtual_dom$VirtualDom_History$RecentHistory,
				newModel,
				{
					ctor: '::',
					_0: msg,
					_1: {ctor: '[]'}
				},
				1)
		} : {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Maybe$Nothing,
			_1: A3(
				_elm_lang$virtual_dom$VirtualDom_History$RecentHistory,
				_p22,
				{ctor: '::', _0: msg, _1: _p21},
				_p23 + 1)
		};
	});
var _elm_lang$virtual_dom$VirtualDom_History$add = F3(
	function (msg, model, _p24) {
		var _p25 = _p24;
		var _p28 = _p25.snapshots;
		var _p27 = _p25.numMessages;
		var _p26 = A3(_elm_lang$virtual_dom$VirtualDom_History$addRecent, msg, model, _p25.recent);
		if (_p26._0.ctor === 'Just') {
			return A3(
				_elm_lang$virtual_dom$VirtualDom_History$History,
				A2(_elm_lang$core$Array$push, _p26._0._0, _p28),
				_p26._1,
				_p27 + 1);
		} else {
			return A3(_elm_lang$virtual_dom$VirtualDom_History$History, _p28, _p26._1, _p27 + 1);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_History$decoder = F2(
	function (initialModel, update) {
		var addMessage = F2(
			function (rawMsg, _p29) {
				var _p30 = _p29;
				var _p31 = _p30._0;
				var msg = _elm_lang$virtual_dom$VirtualDom_History$jsToElm(rawMsg);
				return {
					ctor: '_Tuple2',
					_0: A2(update, msg, _p31),
					_1: A3(_elm_lang$virtual_dom$VirtualDom_History$add, msg, _p31, _p30._1)
				};
			});
		var updateModel = function (rawMsgs) {
			return A3(
				_elm_lang$core$List$foldl,
				addMessage,
				{
					ctor: '_Tuple2',
					_0: initialModel,
					_1: _elm_lang$virtual_dom$VirtualDom_History$empty(initialModel)
				},
				rawMsgs);
		};
		return A2(
			_elm_lang$core$Json_Decode$map,
			updateModel,
			_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$value));
	});
var _elm_lang$virtual_dom$VirtualDom_History$Done = F2(
	function (a, b) {
		return {ctor: 'Done', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_History$Stepping = F2(
	function (a, b) {
		return {ctor: 'Stepping', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_History$getHelp = F3(
	function (update, msg, getResult) {
		var _p32 = getResult;
		if (_p32.ctor === 'Done') {
			return getResult;
		} else {
			var _p34 = _p32._0;
			var _p33 = _p32._1;
			return _elm_lang$core$Native_Utils.eq(_p34, 0) ? A2(
				_elm_lang$virtual_dom$VirtualDom_History$Done,
				msg,
				_elm_lang$core$Tuple$first(
					A2(update, msg, _p33))) : A2(
				_elm_lang$virtual_dom$VirtualDom_History$Stepping,
				_p34 - 1,
				_elm_lang$core$Tuple$first(
					A2(update, msg, _p33)));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_History$get = F3(
	function (update, index, _p35) {
		var _p36 = _p35;
		var _p39 = _p36.recent;
		var snapshotMax = _p36.numMessages - _p39.numMessages;
		if (_elm_lang$core$Native_Utils.cmp(index, snapshotMax) > -1) {
			return _elm_lang$virtual_dom$VirtualDom_History$undone(
				A3(
					_elm_lang$core$List$foldr,
					_elm_lang$virtual_dom$VirtualDom_History$getHelp(update),
					A2(_elm_lang$virtual_dom$VirtualDom_History$Stepping, index - snapshotMax, _p39.model),
					_p39.messages));
		} else {
			var _p37 = A2(_elm_lang$core$Array$get, (index / _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize) | 0, _p36.snapshots);
			if (_p37.ctor === 'Nothing') {
				return _elm_lang$core$Native_Utils.crashCase(
					'VirtualDom.History',
					{
						start: {line: 165, column: 7},
						end: {line: 171, column: 95}
					},
					_p37)('UI should only let you ask for real indexes!');
			} else {
				return _elm_lang$virtual_dom$VirtualDom_History$undone(
					A3(
						_elm_lang$core$Array$foldr,
						_elm_lang$virtual_dom$VirtualDom_History$getHelp(update),
						A2(
							_elm_lang$virtual_dom$VirtualDom_History$Stepping,
							A2(_elm_lang$core$Basics$rem, index, _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize),
							_p37._0.model),
						_p37._0.messages));
			}
		}
	});

var _elm_lang$virtual_dom$VirtualDom_Overlay$styles = A3(
	_elm_lang$virtual_dom$VirtualDom_Helpers$node,
	'style',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('\n\n.elm-overlay {\n  position: fixed;\n  top: 0;\n  left: 0;\n  width: 100%;\n  height: 100%;\n  color: white;\n  pointer-events: none;\n  font-family: \'Trebuchet MS\', \'Lucida Grande\', \'Bitstream Vera Sans\', \'Helvetica Neue\', sans-serif;\n}\n\n.elm-overlay-resume {\n  width: 100%;\n  height: 100%;\n  cursor: pointer;\n  text-align: center;\n  pointer-events: auto;\n  background-color: rgba(200, 200, 200, 0.7);\n}\n\n.elm-overlay-resume-words {\n  position: absolute;\n  top: calc(50% - 40px);\n  font-size: 80px;\n  line-height: 80px;\n  height: 80px;\n  width: 100%;\n}\n\n.elm-mini-controls {\n  position: fixed;\n  bottom: 0;\n  right: 6px;\n  border-radius: 4px;\n  background-color: rgb(61, 61, 61);\n  font-family: monospace;\n  pointer-events: auto;\n}\n\n.elm-mini-controls-button {\n  padding: 6px;\n  cursor: pointer;\n  text-align: center;\n  min-width: 24ch;\n}\n\n.elm-mini-controls-import-export {\n  padding: 4px 0;\n  font-size: 0.8em;\n  text-align: center;\n  background-color: rgb(50, 50, 50);\n}\n\n.elm-overlay-message {\n  position: absolute;\n  width: 600px;\n  height: 100%;\n  padding-left: calc(50% - 300px);\n  padding-right: calc(50% - 300px);\n  background-color: rgba(200, 200, 200, 0.7);\n  pointer-events: auto;\n}\n\n.elm-overlay-message-title {\n  font-size: 36px;\n  height: 80px;\n  background-color: rgb(50, 50, 50);\n  padding-left: 22px;\n  vertical-align: middle;\n  line-height: 80px;\n}\n\n.elm-overlay-message-details {\n  padding: 8px 20px;\n  overflow-y: auto;\n  max-height: calc(100% - 156px);\n  background-color: rgb(61, 61, 61);\n}\n\n.elm-overlay-message-details-type {\n  font-size: 1.5em;\n}\n\n.elm-overlay-message-details ul {\n  list-style-type: none;\n  padding-left: 20px;\n}\n\n.elm-overlay-message-details ul ul {\n  list-style-type: disc;\n  padding-left: 2em;\n}\n\n.elm-overlay-message-details li {\n  margin: 8px 0;\n}\n\n.elm-overlay-message-buttons {\n  height: 60px;\n  line-height: 60px;\n  text-align: right;\n  background-color: rgb(50, 50, 50);\n}\n\n.elm-overlay-message-buttons button {\n  margin-right: 20px;\n}\n\n'),
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$button = F2(
	function (msg, label) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$span,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(msg),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'cursor', _1: 'pointer'},
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(label),
				_1: {ctor: '[]'}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewImportExport = F3(
	function (props, importMsg, exportMsg) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			props,
			{
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$button, importMsg, 'Import'),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' / '),
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$button, exportMsg, 'Export'),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewMiniControls = F2(
	function (config, numMsgs) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-mini-controls'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(config.open),
						_1: {
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-mini-controls-button'),
							_1: {ctor: '[]'}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'Explore History (',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(numMsgs),
									')'))),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Overlay$viewImportExport,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-mini-controls-import-export'),
							_1: {ctor: '[]'}
						},
						config.importHistory,
						config.exportHistory),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$addCommas = function (items) {
	var _p0 = items;
	if (_p0.ctor === '[]') {
		return '';
	} else {
		if (_p0._1.ctor === '[]') {
			return _p0._0;
		} else {
			if (_p0._1._1.ctor === '[]') {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_p0._0,
					A2(_elm_lang$core$Basics_ops['++'], ' and ', _p0._1._0));
			} else {
				return A2(
					_elm_lang$core$String$join,
					', ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_p0._1,
						{
							ctor: '::',
							_0: A2(_elm_lang$core$Basics_ops['++'], ' and ', _p0._0),
							_1: {ctor: '[]'}
						}));
			}
		}
	}
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$problemToString = function (problem) {
	var _p1 = problem;
	switch (_p1.ctor) {
		case 'Function':
			return 'functions';
		case 'Decoder':
			return 'JSON decoders';
		case 'Task':
			return 'tasks';
		case 'Process':
			return 'processes';
		case 'Socket':
			return 'web sockets';
		case 'Request':
			return 'HTTP requests';
		case 'Program':
			return 'programs';
		default:
			return 'virtual DOM values';
	}
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$goodNews2 = '\nfunction can pattern match on that data and call whatever functions, JSON\ndecoders, etc. you need. This makes the code much more explicit and easy to\nfollow for other readers (or you in a few months!)\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$goodNews1 = '\nThe good news is that having values like this in your message type is not\nso great in the long run. You are better off using simpler data, like\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode = function (name) {
	return A3(
		_elm_lang$virtual_dom$VirtualDom_Helpers$node,
		'code',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(name),
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewMention = F2(
	function (tags, verbed) {
		var _p2 = A2(
			_elm_lang$core$List$map,
			_elm_lang$virtual_dom$VirtualDom_Overlay$viewCode,
			_elm_lang$core$List$reverse(tags));
		if (_p2.ctor === '[]') {
			return _elm_lang$virtual_dom$VirtualDom_Helpers$text('');
		} else {
			if (_p2._1.ctor === '[]') {
				return A3(
					_elm_lang$virtual_dom$VirtualDom_Helpers$node,
					'li',
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(verbed),
						_1: {
							ctor: '::',
							_0: _p2._0,
							_1: {
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('.'),
								_1: {ctor: '[]'}
							}
						}
					});
			} else {
				if (_p2._1._1.ctor === '[]') {
					return A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'li',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(verbed),
							_1: {
								ctor: '::',
								_0: _p2._1._0,
								_1: {
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' and '),
									_1: {
										ctor: '::',
										_0: _p2._0,
										_1: {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('.'),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						});
				} else {
					return A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'li',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(verbed),
							_1: A2(
								_elm_lang$core$Basics_ops['++'],
								A2(
									_elm_lang$core$List$intersperse,
									_elm_lang$virtual_dom$VirtualDom_Helpers$text(', '),
									_elm_lang$core$List$reverse(_p2._1)),
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', and '),
									_1: {
										ctor: '::',
										_0: _p2._0,
										_1: {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('.'),
											_1: {ctor: '[]'}
										}
									}
								})
						});
				}
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewChange = function (change) {
	return A3(
		_elm_lang$virtual_dom$VirtualDom_Helpers$node,
		'li',
		{ctor: '[]'},
		function () {
			var _p3 = change;
			if (_p3.ctor === 'AliasChange') {
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-details-type'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p3._0),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			} else {
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-details-type'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p3._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A3(
							_elm_lang$virtual_dom$VirtualDom_Helpers$node,
							'ul',
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMention, _p3._1.removed, 'Removed '),
								_1: {
									ctor: '::',
									_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMention, _p3._1.changed, 'Changed '),
									_1: {
										ctor: '::',
										_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMention, _p3._1.added, 'Added '),
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {
							ctor: '::',
							_0: _p3._1.argsMatch ? _elm_lang$virtual_dom$VirtualDom_Helpers$text('') : _elm_lang$virtual_dom$VirtualDom_Helpers$text('This may be due to the fact that the type variable names changed.'),
							_1: {ctor: '[]'}
						}
					}
				};
			}
		}());
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewProblemType = function (_p4) {
	var _p5 = _p4;
	return A3(
		_elm_lang$virtual_dom$VirtualDom_Helpers$node,
		'li',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p5.name),
			_1: {
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
					A2(
						_elm_lang$core$Basics_ops['++'],
						' can contain ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$virtual_dom$VirtualDom_Overlay$addCommas(
								A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Overlay$problemToString, _p5.problems)),
							'.'))),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewBadMetadata = function (_p6) {
	var _p7 = _p6;
	return {
		ctor: '::',
		_0: A3(
			_elm_lang$virtual_dom$VirtualDom_Helpers$node,
			'p',
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('The '),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p7.message),
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' type of your program cannot be reliably serialized for history files.'),
						_1: {ctor: '[]'}
					}
				}
			}),
		_1: {
			ctor: '::',
			_0: A3(
				_elm_lang$virtual_dom$VirtualDom_Helpers$node,
				'p',
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Functions cannot be serialized, nor can values that contain functions. This is a problem in these places:'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A3(
					_elm_lang$virtual_dom$VirtualDom_Helpers$node,
					'ul',
					{ctor: '[]'},
					A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Overlay$viewProblemType, _p7.problems)),
				_1: {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'p',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_elm_lang$virtual_dom$VirtualDom_Overlay$goodNews1),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$a,
									{
										ctor: '::',
										_0: _elm_lang$virtual_dom$VirtualDom_Helpers$href('https://guide.elm-lang.org/types/union_types.html'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('union types'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', in your messages. From there, your '),
									_1: {
										ctor: '::',
										_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode('update'),
										_1: {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_elm_lang$virtual_dom$VirtualDom_Overlay$goodNews2),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}),
					_1: {ctor: '[]'}
				}
			}
		}
	};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$explanationRisky = '\nThis history seems old. It will work with this program, but some\nmessages have been added since the history was created:\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$explanationBad = '\nThe messages in this history do not match the messages handled by your\nprogram. I noticed changes in the following types:\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewReport = F2(
	function (isBad, report) {
		var _p8 = report;
		switch (_p8.ctor) {
			case 'CorruptHistory':
				return {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Looks like this history file is corrupt. I cannot understand it.'),
					_1: {ctor: '[]'}
				};
			case 'VersionChanged':
				return {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'This history was created with Elm ',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_p8._0,
								A2(
									_elm_lang$core$Basics_ops['++'],
									', but you are using Elm ',
									A2(_elm_lang$core$Basics_ops['++'], _p8._1, ' right now.'))))),
					_1: {ctor: '[]'}
				};
			case 'MessageChanged':
				return {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
						A2(_elm_lang$core$Basics_ops['++'], 'To import some other history, the overall message type must', ' be the same. The old history has ')),
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p8._0),
						_1: {
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' messages, but the new program works with '),
							_1: {
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p8._1),
								_1: {
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' messages.'),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				};
			default:
				return {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'p',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
								isBad ? _elm_lang$virtual_dom$VirtualDom_Overlay$explanationBad : _elm_lang$virtual_dom$VirtualDom_Overlay$explanationRisky),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A3(
							_elm_lang$virtual_dom$VirtualDom_Helpers$node,
							'ul',
							{ctor: '[]'},
							A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Overlay$viewChange, _p8._0)),
						_1: {ctor: '[]'}
					}
				};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewResume = function (config) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-resume'),
			_1: {
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(config.resume),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$virtual_dom$VirtualDom_Helpers$div,
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-resume-words'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Click to Resume'),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$uploadDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}),
	A2(_elm_lang$core$Json_Decode$field, 'metadata', _elm_lang$virtual_dom$VirtualDom_Metadata$decoder),
	A2(_elm_lang$core$Json_Decode$field, 'history', _elm_lang$core$Json_Decode$value));
var _elm_lang$virtual_dom$VirtualDom_Overlay$close = F2(
	function (msg, state) {
		var _p9 = state;
		switch (_p9.ctor) {
			case 'None':
				return _elm_lang$core$Maybe$Nothing;
			case 'BadMetadata':
				return _elm_lang$core$Maybe$Nothing;
			case 'BadImport':
				return _elm_lang$core$Maybe$Nothing;
			default:
				var _p10 = msg;
				if (_p10.ctor === 'Cancel') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					return _elm_lang$core$Maybe$Just(_p9._1);
				}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$isBlocking = function (state) {
	var _p11 = state;
	if (_p11.ctor === 'None') {
		return false;
	} else {
		return true;
	}
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Config = F5(
	function (a, b, c, d, e) {
		return {resume: a, open: b, importHistory: c, exportHistory: d, wrap: e};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$RiskyImport = F2(
	function (a, b) {
		return {ctor: 'RiskyImport', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$BadImport = function (a) {
	return {ctor: 'BadImport', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$corruptImport = _elm_lang$virtual_dom$VirtualDom_Overlay$BadImport(_elm_lang$virtual_dom$VirtualDom_Report$CorruptHistory);
var _elm_lang$virtual_dom$VirtualDom_Overlay$assessImport = F2(
	function (metadata, jsonString) {
		var _p12 = A2(_elm_lang$core$Json_Decode$decodeString, _elm_lang$virtual_dom$VirtualDom_Overlay$uploadDecoder, jsonString);
		if (_p12.ctor === 'Err') {
			return _elm_lang$core$Result$Err(_elm_lang$virtual_dom$VirtualDom_Overlay$corruptImport);
		} else {
			var _p14 = _p12._0._1;
			var report = A2(_elm_lang$virtual_dom$VirtualDom_Metadata$check, _p12._0._0, metadata);
			var _p13 = _elm_lang$virtual_dom$VirtualDom_Report$evaluate(report);
			switch (_p13.ctor) {
				case 'Impossible':
					return _elm_lang$core$Result$Err(
						_elm_lang$virtual_dom$VirtualDom_Overlay$BadImport(report));
				case 'Risky':
					return _elm_lang$core$Result$Err(
						A2(_elm_lang$virtual_dom$VirtualDom_Overlay$RiskyImport, report, _p14));
				default:
					return _elm_lang$core$Result$Ok(_p14);
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$BadMetadata = function (a) {
	return {ctor: 'BadMetadata', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$badMetadata = _elm_lang$virtual_dom$VirtualDom_Overlay$BadMetadata;
var _elm_lang$virtual_dom$VirtualDom_Overlay$None = {ctor: 'None'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$none = _elm_lang$virtual_dom$VirtualDom_Overlay$None;
var _elm_lang$virtual_dom$VirtualDom_Overlay$Proceed = {ctor: 'Proceed'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Cancel = {ctor: 'Cancel'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewButtons = function (buttons) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-buttons'),
			_1: {ctor: '[]'}
		},
		function () {
			var _p15 = buttons;
			if (_p15.ctor === 'Accept') {
				return {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'button',
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Overlay$Proceed),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p15._0),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			} else {
				return {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'button',
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Overlay$Cancel),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p15._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A3(
							_elm_lang$virtual_dom$VirtualDom_Helpers$node,
							'button',
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Overlay$Proceed),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p15._1),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				};
			}
		}());
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Message = {ctor: 'Message'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage = F4(
	function (config, title, details, buttons) {
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$virtual_dom$VirtualDom_Overlay$Message,
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$div,
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-title'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(title),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$div,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-details'),
									_1: {ctor: '[]'}
								},
								details),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									config.wrap,
									_elm_lang$virtual_dom$VirtualDom_Overlay$viewButtons(buttons)),
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {ctor: '[]'}
			}
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$Pause = {ctor: 'Pause'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Normal = {ctor: 'Normal'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Choose = F2(
	function (a, b) {
		return {ctor: 'Choose', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$Accept = function (a) {
	return {ctor: 'Accept', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewHelp = F5(
	function (config, isPaused, isOpen, numMsgs, state) {
		var _p16 = state;
		switch (_p16.ctor) {
			case 'None':
				var miniControls = isOpen ? {ctor: '[]'} : {
					ctor: '::',
					_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMiniControls, config, numMsgs),
					_1: {ctor: '[]'}
				};
				return {
					ctor: '_Tuple2',
					_0: isPaused ? _elm_lang$virtual_dom$VirtualDom_Overlay$Pause : _elm_lang$virtual_dom$VirtualDom_Overlay$Normal,
					_1: (isPaused && (!isOpen)) ? {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewResume(config),
						_1: miniControls
					} : miniControls
				};
			case 'BadMetadata':
				return A4(
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage,
					config,
					'Cannot use Import or Export',
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewBadMetadata(_p16._0),
					_elm_lang$virtual_dom$VirtualDom_Overlay$Accept('Ok'));
			case 'BadImport':
				return A4(
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage,
					config,
					'Cannot Import History',
					A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewReport, true, _p16._0),
					_elm_lang$virtual_dom$VirtualDom_Overlay$Accept('Ok'));
			default:
				return A4(
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage,
					config,
					'Warning',
					A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewReport, false, _p16._0),
					A2(_elm_lang$virtual_dom$VirtualDom_Overlay$Choose, 'Cancel', 'Import Anyway'));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$view = F5(
	function (config, isPaused, isOpen, numMsgs, state) {
		var _p17 = A5(_elm_lang$virtual_dom$VirtualDom_Overlay$viewHelp, config, isPaused, isOpen, numMsgs, state);
		var block = _p17._0;
		var nodes = _p17._1;
		return {
			ctor: '_Tuple2',
			_0: block,
			_1: A2(
				_elm_lang$virtual_dom$VirtualDom_Helpers$div,
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay'),
					_1: {ctor: '[]'}
				},
				{ctor: '::', _0: _elm_lang$virtual_dom$VirtualDom_Overlay$styles, _1: nodes})
		};
	});

var _elm_lang$virtual_dom$VirtualDom_Debug$styles = A3(
	_elm_lang$virtual_dom$VirtualDom_Helpers$node,
	'style',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('\n\nhtml {\n    overflow: hidden;\n    height: 100%;\n}\n\nbody {\n    height: 100%;\n    overflow: auto;\n}\n\n#debugger {\n  width: 100%\n  height: 100%;\n  font-family: monospace;\n}\n\n#values {\n  display: block;\n  float: left;\n  height: 100%;\n  width: calc(100% - 30ch);\n  margin: 0;\n  overflow: auto;\n  cursor: default;\n}\n\n.debugger-sidebar {\n  display: block;\n  float: left;\n  width: 30ch;\n  height: 100%;\n  color: white;\n  background-color: rgb(61, 61, 61);\n}\n\n.debugger-sidebar-controls {\n  width: 100%;\n  text-align: center;\n  background-color: rgb(50, 50, 50);\n}\n\n.debugger-sidebar-controls-import-export {\n  width: 100%;\n  height: 24px;\n  line-height: 24px;\n  font-size: 12px;\n}\n\n.debugger-sidebar-controls-resume {\n  width: 100%;\n  height: 30px;\n  line-height: 30px;\n  cursor: pointer;\n}\n\n.debugger-sidebar-controls-resume:hover {\n  background-color: rgb(41, 41, 41);\n}\n\n.debugger-sidebar-messages {\n  width: 100%;\n  overflow-y: auto;\n  height: calc(100% - 24px);\n}\n\n.debugger-sidebar-messages-paused {\n  width: 100%;\n  overflow-y: auto;\n  height: calc(100% - 54px);\n}\n\n.messages-entry {\n  cursor: pointer;\n  width: 100%;\n}\n\n.messages-entry:hover {\n  background-color: rgb(41, 41, 41);\n}\n\n.messages-entry-selected, .messages-entry-selected:hover {\n  background-color: rgb(10, 10, 10);\n}\n\n.messages-entry-content {\n  width: calc(100% - 7ch);\n  padding-top: 4px;\n  padding-bottom: 4px;\n  padding-left: 1ch;\n  text-overflow: ellipsis;\n  white-space: nowrap;\n  overflow: hidden;\n  display: inline-block;\n}\n\n.messages-entry-index {\n  color: #666;\n  width: 5ch;\n  padding-top: 4px;\n  padding-bottom: 4px;\n  padding-right: 1ch;\n  text-align: right;\n  display: block;\n  float: right;\n}\n\n'),
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$button = F2(
	function (msg, label) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$span,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(msg),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'cursor', _1: 'pointer'},
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(label),
				_1: {ctor: '[]'}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel = function (state) {
	var _p0 = state;
	if (_p0.ctor === 'Running') {
		return _p0._0;
	} else {
		return _p0._2;
	}
};
var _elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata = F2(
	function (model, func) {
		var _p1 = model.metadata;
		if (_p1.ctor === 'Ok') {
			return func(_p1._0);
		} else {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{
						overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$badMetadata(_p1._0)
					}),
				{ctor: '[]'});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$Model = F6(
	function (a, b, c, d, e, f) {
		return {history: a, state: b, expando: c, metadata: d, overlay: e, isDebuggerOpen: f};
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$Paused = F3(
	function (a, b, c) {
		return {ctor: 'Paused', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$Running = function (a) {
	return {ctor: 'Running', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$loadNewHistory = F3(
	function (rawHistory, userUpdate, model) {
		var pureUserUpdate = F2(
			function (msg, userModel) {
				return _elm_lang$core$Tuple$first(
					A2(userUpdate, msg, userModel));
			});
		var initialUserModel = _elm_lang$virtual_dom$VirtualDom_History$initialModel(model.history);
		var decoder = A2(_elm_lang$virtual_dom$VirtualDom_History$decoder, initialUserModel, pureUserUpdate);
		var _p2 = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, rawHistory);
		if (_p2.ctor === 'Err') {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$corruptImport}),
				{ctor: '[]'});
		} else {
			var _p3 = _p2._0._0;
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{
						history: _p2._0._1,
						state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(_p3),
						expando: _elm_lang$virtual_dom$VirtualDom_Expando$init(_p3),
						overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$none
					}),
				{ctor: '[]'});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$OverlayMsg = function (a) {
	return {ctor: 'OverlayMsg', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$Upload = function (a) {
	return {ctor: 'Upload', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$upload = A2(_elm_lang$core$Task$perform, _elm_lang$virtual_dom$VirtualDom_Debug$Upload, _elm_lang$virtual_dom$Native_Debug.upload);
var _elm_lang$virtual_dom$VirtualDom_Debug$Export = {ctor: 'Export'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Import = {ctor: 'Import'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Down = {ctor: 'Down'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Up = {ctor: 'Up'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Close = {ctor: 'Close'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Open = {ctor: 'Open'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Jump = function (a) {
	return {ctor: 'Jump', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$Resume = {ctor: 'Resume'};
var _elm_lang$virtual_dom$VirtualDom_Debug$overlayConfig = {resume: _elm_lang$virtual_dom$VirtualDom_Debug$Resume, open: _elm_lang$virtual_dom$VirtualDom_Debug$Open, importHistory: _elm_lang$virtual_dom$VirtualDom_Debug$Import, exportHistory: _elm_lang$virtual_dom$VirtualDom_Debug$Export, wrap: _elm_lang$virtual_dom$VirtualDom_Debug$OverlayMsg};
var _elm_lang$virtual_dom$VirtualDom_Debug$viewIn = function (_p4) {
	var _p5 = _p4;
	var isPaused = function () {
		var _p6 = _p5.state;
		if (_p6.ctor === 'Running') {
			return false;
		} else {
			return true;
		}
	}();
	return A5(
		_elm_lang$virtual_dom$VirtualDom_Overlay$view,
		_elm_lang$virtual_dom$VirtualDom_Debug$overlayConfig,
		isPaused,
		_p5.isDebuggerOpen,
		_elm_lang$virtual_dom$VirtualDom_History$size(_p5.history),
		_p5.overlay);
};
var _elm_lang$virtual_dom$VirtualDom_Debug$resumeButton = A2(
	_elm_lang$virtual_dom$VirtualDom_Helpers$div,
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Debug$Resume),
		_1: {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar-controls-resume'),
			_1: {ctor: '[]'}
		}
	},
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Resume'),
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$viewResumeButton = function (maybeIndex) {
	var _p7 = maybeIndex;
	if (_p7.ctor === 'Nothing') {
		return _elm_lang$virtual_dom$VirtualDom_Helpers$text('');
	} else {
		return _elm_lang$virtual_dom$VirtualDom_Debug$resumeButton;
	}
};
var _elm_lang$virtual_dom$VirtualDom_Debug$playButton = function (maybeIndex) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar-controls'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Debug$viewResumeButton(maybeIndex),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar-controls-import-export'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$button, _elm_lang$virtual_dom$VirtualDom_Debug$Import, 'Import'),
						_1: {
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' / '),
							_1: {
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$button, _elm_lang$virtual_dom$VirtualDom_Debug$Export, 'Export'),
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Debug$viewSidebar = F2(
	function (state, history) {
		var maybeIndex = function () {
			var _p8 = state;
			if (_p8.ctor === 'Running') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(_p8._0);
			}
		}();
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$map,
					_elm_lang$virtual_dom$VirtualDom_Debug$Jump,
					A2(_elm_lang$virtual_dom$VirtualDom_History$view, maybeIndex, history)),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Debug$playButton(maybeIndex),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$ExpandoMsg = function (a) {
	return {ctor: 'ExpandoMsg', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$viewOut = function (_p9) {
	var _p10 = _p9;
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$id('debugger'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Debug$styles,
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$viewSidebar, _p10.state, _p10.history),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$map,
						_elm_lang$virtual_dom$VirtualDom_Debug$ExpandoMsg,
						A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$div,
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$id('values'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Expando$view, _elm_lang$core$Maybe$Nothing, _p10.expando),
								_1: {ctor: '[]'}
							})),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Debug$UserMsg = function (a) {
	return {ctor: 'UserMsg', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapInit = F2(
	function (metadata, _p11) {
		var _p12 = _p11;
		var _p13 = _p12._0;
		return A2(
			_elm_lang$core$Platform_Cmd_ops['!'],
			{
				history: _elm_lang$virtual_dom$VirtualDom_History$empty(_p13),
				state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(_p13),
				expando: _elm_lang$virtual_dom$VirtualDom_Expando$init(_p13),
				metadata: _elm_lang$virtual_dom$VirtualDom_Metadata$decode(metadata),
				overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$none,
				isDebuggerOpen: false
			},
			{
				ctor: '::',
				_0: A2(_elm_lang$core$Platform_Cmd$map, _elm_lang$virtual_dom$VirtualDom_Debug$UserMsg, _p12._1),
				_1: {ctor: '[]'}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapSubs = F2(
	function (userSubscriptions, _p14) {
		var _p15 = _p14;
		return A2(
			_elm_lang$core$Platform_Sub$map,
			_elm_lang$virtual_dom$VirtualDom_Debug$UserMsg,
			userSubscriptions(
				_elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel(_p15.state)));
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapView = F2(
	function (userView, _p16) {
		var _p17 = _p16;
		var currentModel = function () {
			var _p18 = _p17.state;
			if (_p18.ctor === 'Running') {
				return _p18._0;
			} else {
				return _p18._1;
			}
		}();
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$map,
			_elm_lang$virtual_dom$VirtualDom_Debug$UserMsg,
			userView(currentModel));
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$NoOp = {ctor: 'NoOp'};
var _elm_lang$virtual_dom$VirtualDom_Debug$download = F2(
	function (metadata, history) {
		var json = _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'metadata',
					_1: _elm_lang$virtual_dom$VirtualDom_Metadata$encode(metadata)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'history',
						_1: _elm_lang$virtual_dom$VirtualDom_History$encode(history)
					},
					_1: {ctor: '[]'}
				}
			});
		var historyLength = _elm_lang$virtual_dom$VirtualDom_History$size(history);
		return A2(
			_elm_lang$core$Task$perform,
			function (_p19) {
				return _elm_lang$virtual_dom$VirtualDom_Debug$NoOp;
			},
			A2(_elm_lang$virtual_dom$Native_Debug.download, historyLength, json));
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$runIf = F2(
	function (bool, task) {
		return bool ? A2(
			_elm_lang$core$Task$perform,
			_elm_lang$core$Basics$always(_elm_lang$virtual_dom$VirtualDom_Debug$NoOp),
			task) : _elm_lang$core$Platform_Cmd$none;
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$updateUserMsg = F4(
	function (userUpdate, scrollTask, userMsg, _p20) {
		var _p21 = _p20;
		var _p25 = _p21.state;
		var _p24 = _p21;
		var userModel = _elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel(_p25);
		var newHistory = A3(_elm_lang$virtual_dom$VirtualDom_History$add, userMsg, userModel, _p21.history);
		var _p22 = A2(userUpdate, userMsg, userModel);
		var newUserModel = _p22._0;
		var userCmds = _p22._1;
		var commands = A2(_elm_lang$core$Platform_Cmd$map, _elm_lang$virtual_dom$VirtualDom_Debug$UserMsg, userCmds);
		var _p23 = _p25;
		if (_p23.ctor === 'Running') {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					_p24,
					{
						history: newHistory,
						state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(newUserModel),
						expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$merge, newUserModel, _p21.expando)
					}),
				{
					ctor: '::',
					_0: commands,
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$runIf, _p24.isDebuggerOpen, scrollTask),
						_1: {ctor: '[]'}
					}
				});
		} else {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					_p24,
					{
						history: newHistory,
						state: A3(_elm_lang$virtual_dom$VirtualDom_Debug$Paused, _p23._0, _p23._1, newUserModel)
					}),
				{
					ctor: '::',
					_0: commands,
					_1: {ctor: '[]'}
				});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapUpdate = F4(
	function (userUpdate, scrollTask, msg, model) {
		wrapUpdate:
		while (true) {
			var _p26 = msg;
			switch (_p26.ctor) {
				case 'NoOp':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						model,
						{ctor: '[]'});
				case 'UserMsg':
					return A4(_elm_lang$virtual_dom$VirtualDom_Debug$updateUserMsg, userUpdate, scrollTask, _p26._0, model);
				case 'ExpandoMsg':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{
								expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, _p26._0, model.expando)
							}),
						{ctor: '[]'});
				case 'Resume':
					var _p27 = model.state;
					if (_p27.ctor === 'Running') {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					} else {
						var _p28 = _p27._2;
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							_elm_lang$core$Native_Utils.update(
								model,
								{
									state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(_p28),
									expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$merge, _p28, model.expando)
								}),
							{
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$runIf, model.isDebuggerOpen, scrollTask),
								_1: {ctor: '[]'}
							});
					}
				case 'Jump':
					var _p30 = _p26._0;
					var _p29 = A3(_elm_lang$virtual_dom$VirtualDom_History$get, userUpdate, _p30, model.history);
					var indexModel = _p29._0;
					var indexMsg = _p29._1;
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{
								state: A3(
									_elm_lang$virtual_dom$VirtualDom_Debug$Paused,
									_p30,
									indexModel,
									_elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel(model.state)),
								expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$merge, indexModel, model.expando)
							}),
						{ctor: '[]'});
				case 'Open':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{isDebuggerOpen: true}),
						{ctor: '[]'});
				case 'Close':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{isDebuggerOpen: false}),
						{ctor: '[]'});
				case 'Up':
					var index = function () {
						var _p31 = model.state;
						if (_p31.ctor === 'Paused') {
							return _p31._0;
						} else {
							return _elm_lang$virtual_dom$VirtualDom_History$size(model.history);
						}
					}();
					if (_elm_lang$core$Native_Utils.cmp(index, 0) > 0) {
						var _v17 = userUpdate,
							_v18 = scrollTask,
							_v19 = _elm_lang$virtual_dom$VirtualDom_Debug$Jump(index - 1),
							_v20 = model;
						userUpdate = _v17;
						scrollTask = _v18;
						msg = _v19;
						model = _v20;
						continue wrapUpdate;
					} else {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					}
				case 'Down':
					var _p32 = model.state;
					if (_p32.ctor === 'Running') {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					} else {
						var _p33 = _p32._0;
						if (_elm_lang$core$Native_Utils.eq(
							_p33,
							_elm_lang$virtual_dom$VirtualDom_History$size(model.history) - 1)) {
							var _v22 = userUpdate,
								_v23 = scrollTask,
								_v24 = _elm_lang$virtual_dom$VirtualDom_Debug$Resume,
								_v25 = model;
							userUpdate = _v22;
							scrollTask = _v23;
							msg = _v24;
							model = _v25;
							continue wrapUpdate;
						} else {
							var _v26 = userUpdate,
								_v27 = scrollTask,
								_v28 = _elm_lang$virtual_dom$VirtualDom_Debug$Jump(_p33 + 1),
								_v29 = model;
							userUpdate = _v26;
							scrollTask = _v27;
							msg = _v28;
							model = _v29;
							continue wrapUpdate;
						}
					}
				case 'Import':
					return A2(
						_elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata,
						model,
						function (_p34) {
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Debug$upload,
									_1: {ctor: '[]'}
								});
						});
				case 'Export':
					return A2(
						_elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata,
						model,
						function (metadata) {
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								{
									ctor: '::',
									_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$download, metadata, model.history),
									_1: {ctor: '[]'}
								});
						});
				case 'Upload':
					return A2(
						_elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata,
						model,
						function (metadata) {
							var _p35 = A2(_elm_lang$virtual_dom$VirtualDom_Overlay$assessImport, metadata, _p26._0);
							if (_p35.ctor === 'Err') {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									_elm_lang$core$Native_Utils.update(
										model,
										{overlay: _p35._0}),
									{ctor: '[]'});
							} else {
								return A3(_elm_lang$virtual_dom$VirtualDom_Debug$loadNewHistory, _p35._0, userUpdate, model);
							}
						});
				default:
					var _p36 = A2(_elm_lang$virtual_dom$VirtualDom_Overlay$close, _p26._0, model.overlay);
					if (_p36.ctor === 'Nothing') {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							_elm_lang$core$Native_Utils.update(
								model,
								{overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$none}),
							{ctor: '[]'});
					} else {
						return A3(_elm_lang$virtual_dom$VirtualDom_Debug$loadNewHistory, _p36._0, userUpdate, model);
					}
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrap = F2(
	function (metadata, _p37) {
		var _p38 = _p37;
		return {
			init: A2(_elm_lang$virtual_dom$VirtualDom_Debug$wrapInit, metadata, _p38.init),
			view: _elm_lang$virtual_dom$VirtualDom_Debug$wrapView(_p38.view),
			update: _elm_lang$virtual_dom$VirtualDom_Debug$wrapUpdate(_p38.update),
			viewIn: _elm_lang$virtual_dom$VirtualDom_Debug$viewIn,
			viewOut: _elm_lang$virtual_dom$VirtualDom_Debug$viewOut,
			subscriptions: _elm_lang$virtual_dom$VirtualDom_Debug$wrapSubs(_p38.subscriptions)
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags = F2(
	function (metadata, _p39) {
		var _p40 = _p39;
		return {
			init: function (flags) {
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Debug$wrapInit,
					metadata,
					_p40.init(flags));
			},
			view: _elm_lang$virtual_dom$VirtualDom_Debug$wrapView(_p40.view),
			update: _elm_lang$virtual_dom$VirtualDom_Debug$wrapUpdate(_p40.update),
			viewIn: _elm_lang$virtual_dom$VirtualDom_Debug$viewIn,
			viewOut: _elm_lang$virtual_dom$VirtualDom_Debug$viewOut,
			subscriptions: _elm_lang$virtual_dom$VirtualDom_Debug$wrapSubs(_p40.subscriptions)
		};
	});

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _elm_lang$http$Native_Http = function() {


// ENCODING AND DECODING

function encodeUri(string)
{
	return encodeURIComponent(string);
}

function decodeUri(string)
{
	try
	{
		return _elm_lang$core$Maybe$Just(decodeURIComponent(string));
	}
	catch(e)
	{
		return _elm_lang$core$Maybe$Nothing;
	}
}


// SEND REQUEST

function toTask(request, maybeProgress)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var xhr = new XMLHttpRequest();

		configureProgress(xhr, maybeProgress);

		xhr.addEventListener('error', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NetworkError' }));
		});
		xhr.addEventListener('timeout', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'Timeout' }));
		});
		xhr.addEventListener('load', function() {
			callback(handleResponse(xhr, request.expect.responseToResult));
		});

		try
		{
			xhr.open(request.method, request.url, true);
		}
		catch (e)
		{
			return callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'BadUrl', _0: request.url }));
		}

		configureRequest(xhr, request);
		send(xhr, request.body);

		return function() { xhr.abort(); };
	});
}

function configureProgress(xhr, maybeProgress)
{
	if (maybeProgress.ctor === 'Nothing')
	{
		return;
	}

	xhr.addEventListener('progress', function(event) {
		if (!event.lengthComputable)
		{
			return;
		}
		_elm_lang$core$Native_Scheduler.rawSpawn(maybeProgress._0({
			bytes: event.loaded,
			bytesExpected: event.total
		}));
	});
}

function configureRequest(xhr, request)
{
	function setHeader(pair)
	{
		xhr.setRequestHeader(pair._0, pair._1);
	}

	A2(_elm_lang$core$List$map, setHeader, request.headers);
	xhr.responseType = request.expect.responseType;
	xhr.withCredentials = request.withCredentials;

	if (request.timeout.ctor === 'Just')
	{
		xhr.timeout = request.timeout._0;
	}
}

function send(xhr, body)
{
	switch (body.ctor)
	{
		case 'EmptyBody':
			xhr.send();
			return;

		case 'StringBody':
			xhr.setRequestHeader('Content-Type', body._0);
			xhr.send(body._1);
			return;

		case 'FormDataBody':
			xhr.send(body._0);
			return;
	}
}


// RESPONSES

function handleResponse(xhr, responseToResult)
{
	var response = toResponse(xhr);

	if (xhr.status < 200 || 300 <= xhr.status)
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadStatus',
			_0: response
		});
	}

	var result = responseToResult(response);

	if (result.ctor === 'Ok')
	{
		return _elm_lang$core$Native_Scheduler.succeed(result._0);
	}
	else
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadPayload',
			_0: result._0,
			_1: response
		});
	}
}

function toResponse(xhr)
{
	return {
		status: { code: xhr.status, message: xhr.statusText },
		headers: parseHeaders(xhr.getAllResponseHeaders()),
		url: xhr.responseURL,
		body: xhr.response
	};
}

function parseHeaders(rawHeaders)
{
	var headers = _elm_lang$core$Dict$empty;

	if (!rawHeaders)
	{
		return headers;
	}

	var headerPairs = rawHeaders.split('\u000d\u000a');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf('\u003a\u0020');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(_elm_lang$core$Dict$update, key, function(oldValue) {
				if (oldValue.ctor === 'Just')
				{
					return _elm_lang$core$Maybe$Just(value + ', ' + oldValue._0);
				}
				return _elm_lang$core$Maybe$Just(value);
			}, headers);
		}
	}

	return headers;
}


// EXPECTORS

function expectStringResponse(responseToResult)
{
	return {
		responseType: 'text',
		responseToResult: responseToResult
	};
}

function mapExpect(func, expect)
{
	return {
		responseType: expect.responseType,
		responseToResult: function(response) {
			var convertedResponse = expect.responseToResult(response);
			return A2(_elm_lang$core$Result$map, func, convertedResponse);
		}
	};
}


// BODY

function multipart(parts)
{
	var formData = new FormData();

	while (parts.ctor !== '[]')
	{
		var part = parts._0;
		formData.append(part._0, part._1);
		parts = parts._1;
	}

	return { ctor: 'FormDataBody', _0: formData };
}

return {
	toTask: F2(toTask),
	expectStringResponse: expectStringResponse,
	mapExpect: F2(mapExpect),
	multipart: multipart,
	encodeUri: encodeUri,
	decodeUri: decodeUri
};

}();

var _elm_lang$http$Http_Internal$map = F2(
	function (func, request) {
		return _elm_lang$core$Native_Utils.update(
			request,
			{
				expect: A2(_elm_lang$http$Native_Http.mapExpect, func, request.expect)
			});
	});
var _elm_lang$http$Http_Internal$RawRequest = F7(
	function (a, b, c, d, e, f, g) {
		return {method: a, headers: b, url: c, body: d, expect: e, timeout: f, withCredentials: g};
	});
var _elm_lang$http$Http_Internal$Request = function (a) {
	return {ctor: 'Request', _0: a};
};
var _elm_lang$http$Http_Internal$Expect = {ctor: 'Expect'};
var _elm_lang$http$Http_Internal$FormDataBody = {ctor: 'FormDataBody'};
var _elm_lang$http$Http_Internal$StringBody = F2(
	function (a, b) {
		return {ctor: 'StringBody', _0: a, _1: b};
	});
var _elm_lang$http$Http_Internal$EmptyBody = {ctor: 'EmptyBody'};
var _elm_lang$http$Http_Internal$Header = F2(
	function (a, b) {
		return {ctor: 'Header', _0: a, _1: b};
	});

var _elm_lang$http$Http$decodeUri = _elm_lang$http$Native_Http.decodeUri;
var _elm_lang$http$Http$encodeUri = _elm_lang$http$Native_Http.encodeUri;
var _elm_lang$http$Http$expectStringResponse = _elm_lang$http$Native_Http.expectStringResponse;
var _elm_lang$http$Http$expectJson = function (decoder) {
	return _elm_lang$http$Http$expectStringResponse(
		function (response) {
			return A2(_elm_lang$core$Json_Decode$decodeString, decoder, response.body);
		});
};
var _elm_lang$http$Http$expectString = _elm_lang$http$Http$expectStringResponse(
	function (response) {
		return _elm_lang$core$Result$Ok(response.body);
	});
var _elm_lang$http$Http$multipartBody = _elm_lang$http$Native_Http.multipart;
var _elm_lang$http$Http$stringBody = _elm_lang$http$Http_Internal$StringBody;
var _elm_lang$http$Http$jsonBody = function (value) {
	return A2(
		_elm_lang$http$Http_Internal$StringBody,
		'application/json',
		A2(_elm_lang$core$Json_Encode$encode, 0, value));
};
var _elm_lang$http$Http$emptyBody = _elm_lang$http$Http_Internal$EmptyBody;
var _elm_lang$http$Http$header = _elm_lang$http$Http_Internal$Header;
var _elm_lang$http$Http$request = _elm_lang$http$Http_Internal$Request;
var _elm_lang$http$Http$post = F3(
	function (url, body, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'POST',
				headers: {ctor: '[]'},
				url: url,
				body: body,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$get = F2(
	function (url, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'GET',
				headers: {ctor: '[]'},
				url: url,
				body: _elm_lang$http$Http$emptyBody,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$getString = function (url) {
	return _elm_lang$http$Http$request(
		{
			method: 'GET',
			headers: {ctor: '[]'},
			url: url,
			body: _elm_lang$http$Http$emptyBody,
			expect: _elm_lang$http$Http$expectString,
			timeout: _elm_lang$core$Maybe$Nothing,
			withCredentials: false
		});
};
var _elm_lang$http$Http$toTask = function (_p0) {
	var _p1 = _p0;
	return A2(_elm_lang$http$Native_Http.toTask, _p1._0, _elm_lang$core$Maybe$Nothing);
};
var _elm_lang$http$Http$send = F2(
	function (resultToMessage, request) {
		return A2(
			_elm_lang$core$Task$attempt,
			resultToMessage,
			_elm_lang$http$Http$toTask(request));
	});
var _elm_lang$http$Http$Response = F4(
	function (a, b, c, d) {
		return {url: a, status: b, headers: c, body: d};
	});
var _elm_lang$http$Http$BadPayload = F2(
	function (a, b) {
		return {ctor: 'BadPayload', _0: a, _1: b};
	});
var _elm_lang$http$Http$BadStatus = function (a) {
	return {ctor: 'BadStatus', _0: a};
};
var _elm_lang$http$Http$NetworkError = {ctor: 'NetworkError'};
var _elm_lang$http$Http$Timeout = {ctor: 'Timeout'};
var _elm_lang$http$Http$BadUrl = function (a) {
	return {ctor: 'BadUrl', _0: a};
};
var _elm_lang$http$Http$StringPart = F2(
	function (a, b) {
		return {ctor: 'StringPart', _0: a, _1: b};
	});
var _elm_lang$http$Http$stringPart = _elm_lang$http$Http$StringPart;

var _elm_lang$mouse$Mouse_ops = _elm_lang$mouse$Mouse_ops || {};
_elm_lang$mouse$Mouse_ops['&>'] = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return t2;
			},
			t1);
	});
var _elm_lang$mouse$Mouse$onSelfMsg = F3(
	function (router, _p1, state) {
		var _p2 = _p1;
		var _p3 = A2(_elm_lang$core$Dict$get, _p2.category, state);
		if (_p3.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p2.position));
			};
			return A2(
				_elm_lang$mouse$Mouse_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p3._0.taggers)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$mouse$Mouse$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$mouse$Mouse$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p4 = maybeValues;
		if (_p4.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p4._0});
		}
	});
var _elm_lang$mouse$Mouse$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p5 = subs;
			if (_p5.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p5._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p5._0._0,
					_elm_lang$mouse$Mouse$categorizeHelpHelp(_p5._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$mouse$Mouse$categorize = function (subs) {
	return A2(_elm_lang$mouse$Mouse$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$mouse$Mouse$subscription = _elm_lang$core$Native_Platform.leaf('Mouse');
var _elm_lang$mouse$Mouse$Position = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _elm_lang$mouse$Mouse$position = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$mouse$Mouse$Position,
	A2(_elm_lang$core$Json_Decode$field, 'pageX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'pageY', _elm_lang$core$Json_Decode$int));
var _elm_lang$mouse$Mouse$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$mouse$Mouse$Msg = F2(
	function (a, b) {
		return {category: a, position: b};
	});
var _elm_lang$mouse$Mouse$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				var tracker = A3(
					_elm_lang$dom$Dom_LowLevel$onDocument,
					category,
					_elm_lang$mouse$Mouse$position,
					function (_p6) {
						return A2(
							_elm_lang$core$Platform$sendToSelf,
							router,
							A2(_elm_lang$mouse$Mouse$Msg, category, _p6));
					});
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$mouse$Mouse$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(tracker));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p7, taggers, task) {
				var _p8 = _p7;
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$core$Dict$insert,
								category,
								A2(_elm_lang$mouse$Mouse$Watcher, taggers, _p8.pid),
								state));
					},
					task);
			});
		var leftStep = F3(
			function (category, _p9, task) {
				var _p10 = _p9;
				return A2(
					_elm_lang$mouse$Mouse_ops['&>'],
					_elm_lang$core$Process$kill(_p10.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$mouse$Mouse$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$mouse$Mouse$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$mouse$Mouse$clicks = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'click', tagger));
};
var _elm_lang$mouse$Mouse$moves = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousemove', tagger));
};
var _elm_lang$mouse$Mouse$downs = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousedown', tagger));
};
var _elm_lang$mouse$Mouse$ups = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mouseup', tagger));
};
var _elm_lang$mouse$Mouse$subMap = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return A2(
			_elm_lang$mouse$Mouse$MySub,
			_p12._0,
			function (_p13) {
				return func(
					_p12._1(_p13));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Mouse'] = {pkg: 'elm-lang/mouse', init: _elm_lang$mouse$Mouse$init, onEffects: _elm_lang$mouse$Mouse$onEffects, onSelfMsg: _elm_lang$mouse$Mouse$onSelfMsg, tag: 'sub', subMap: _elm_lang$mouse$Mouse$subMap};

var _elm_lang$navigation$Native_Navigation = function() {


// FAKE NAVIGATION

function go(n)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		if (n !== 0)
		{
			history.go(n);
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function pushState(url)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		history.pushState({}, '', url);
		callback(_elm_lang$core$Native_Scheduler.succeed(getLocation()));
	});
}

function replaceState(url)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		history.replaceState({}, '', url);
		callback(_elm_lang$core$Native_Scheduler.succeed(getLocation()));
	});
}


// REAL NAVIGATION

function reloadPage(skipCache)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		document.location.reload(skipCache);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function setLocation(url)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		try
		{
			window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			document.location.reload(false);
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


// GET LOCATION

function getLocation()
{
	var location = document.location;

	return {
		href: location.href,
		host: location.host,
		hostname: location.hostname,
		protocol: location.protocol,
		origin: location.origin,
		port_: location.port,
		pathname: location.pathname,
		search: location.search,
		hash: location.hash,
		username: location.username,
		password: location.password
	};
}


// DETECT IE11 PROBLEMS

function isInternetExplorer11()
{
	return window.navigator.userAgent.indexOf('Trident') !== -1;
}


return {
	go: go,
	setLocation: setLocation,
	reloadPage: reloadPage,
	pushState: pushState,
	replaceState: replaceState,
	getLocation: getLocation,
	isInternetExplorer11: isInternetExplorer11
};

}();

var _elm_lang$navigation$Navigation$replaceState = _elm_lang$navigation$Native_Navigation.replaceState;
var _elm_lang$navigation$Navigation$pushState = _elm_lang$navigation$Native_Navigation.pushState;
var _elm_lang$navigation$Navigation$go = _elm_lang$navigation$Native_Navigation.go;
var _elm_lang$navigation$Navigation$reloadPage = _elm_lang$navigation$Native_Navigation.reloadPage;
var _elm_lang$navigation$Navigation$setLocation = _elm_lang$navigation$Native_Navigation.setLocation;
var _elm_lang$navigation$Navigation_ops = _elm_lang$navigation$Navigation_ops || {};
_elm_lang$navigation$Navigation_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return task2;
			},
			task1);
	});
var _elm_lang$navigation$Navigation$notify = F3(
	function (router, subs, location) {
		var send = function (_p1) {
			var _p2 = _p1;
			return A2(
				_elm_lang$core$Platform$sendToApp,
				router,
				_p2._0(location));
		};
		return A2(
			_elm_lang$navigation$Navigation_ops['&>'],
			_elm_lang$core$Task$sequence(
				A2(_elm_lang$core$List$map, send, subs)),
			_elm_lang$core$Task$succeed(
				{ctor: '_Tuple0'}));
	});
var _elm_lang$navigation$Navigation$cmdHelp = F3(
	function (router, subs, cmd) {
		var _p3 = cmd;
		switch (_p3.ctor) {
			case 'Jump':
				return _elm_lang$navigation$Navigation$go(_p3._0);
			case 'New':
				return A2(
					_elm_lang$core$Task$andThen,
					A2(_elm_lang$navigation$Navigation$notify, router, subs),
					_elm_lang$navigation$Navigation$pushState(_p3._0));
			case 'Modify':
				return A2(
					_elm_lang$core$Task$andThen,
					A2(_elm_lang$navigation$Navigation$notify, router, subs),
					_elm_lang$navigation$Navigation$replaceState(_p3._0));
			case 'Visit':
				return _elm_lang$navigation$Navigation$setLocation(_p3._0);
			default:
				return _elm_lang$navigation$Navigation$reloadPage(_p3._0);
		}
	});
var _elm_lang$navigation$Navigation$killPopWatcher = function (popWatcher) {
	var _p4 = popWatcher;
	if (_p4.ctor === 'Normal') {
		return _elm_lang$core$Process$kill(_p4._0);
	} else {
		return A2(
			_elm_lang$navigation$Navigation_ops['&>'],
			_elm_lang$core$Process$kill(_p4._0),
			_elm_lang$core$Process$kill(_p4._1));
	}
};
var _elm_lang$navigation$Navigation$onSelfMsg = F3(
	function (router, location, state) {
		return A2(
			_elm_lang$navigation$Navigation_ops['&>'],
			A3(_elm_lang$navigation$Navigation$notify, router, state.subs, location),
			_elm_lang$core$Task$succeed(state));
	});
var _elm_lang$navigation$Navigation$subscription = _elm_lang$core$Native_Platform.leaf('Navigation');
var _elm_lang$navigation$Navigation$command = _elm_lang$core$Native_Platform.leaf('Navigation');
var _elm_lang$navigation$Navigation$Location = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return {href: a, host: b, hostname: c, protocol: d, origin: e, port_: f, pathname: g, search: h, hash: i, username: j, password: k};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$navigation$Navigation$State = F2(
	function (a, b) {
		return {subs: a, popWatcher: b};
	});
var _elm_lang$navigation$Navigation$init = _elm_lang$core$Task$succeed(
	A2(
		_elm_lang$navigation$Navigation$State,
		{ctor: '[]'},
		_elm_lang$core$Maybe$Nothing));
var _elm_lang$navigation$Navigation$Reload = function (a) {
	return {ctor: 'Reload', _0: a};
};
var _elm_lang$navigation$Navigation$reload = _elm_lang$navigation$Navigation$command(
	_elm_lang$navigation$Navigation$Reload(false));
var _elm_lang$navigation$Navigation$reloadAndSkipCache = _elm_lang$navigation$Navigation$command(
	_elm_lang$navigation$Navigation$Reload(true));
var _elm_lang$navigation$Navigation$Visit = function (a) {
	return {ctor: 'Visit', _0: a};
};
var _elm_lang$navigation$Navigation$load = function (url) {
	return _elm_lang$navigation$Navigation$command(
		_elm_lang$navigation$Navigation$Visit(url));
};
var _elm_lang$navigation$Navigation$Modify = function (a) {
	return {ctor: 'Modify', _0: a};
};
var _elm_lang$navigation$Navigation$modifyUrl = function (url) {
	return _elm_lang$navigation$Navigation$command(
		_elm_lang$navigation$Navigation$Modify(url));
};
var _elm_lang$navigation$Navigation$New = function (a) {
	return {ctor: 'New', _0: a};
};
var _elm_lang$navigation$Navigation$newUrl = function (url) {
	return _elm_lang$navigation$Navigation$command(
		_elm_lang$navigation$Navigation$New(url));
};
var _elm_lang$navigation$Navigation$Jump = function (a) {
	return {ctor: 'Jump', _0: a};
};
var _elm_lang$navigation$Navigation$back = function (n) {
	return _elm_lang$navigation$Navigation$command(
		_elm_lang$navigation$Navigation$Jump(0 - n));
};
var _elm_lang$navigation$Navigation$forward = function (n) {
	return _elm_lang$navigation$Navigation$command(
		_elm_lang$navigation$Navigation$Jump(n));
};
var _elm_lang$navigation$Navigation$cmdMap = F2(
	function (_p5, myCmd) {
		var _p6 = myCmd;
		switch (_p6.ctor) {
			case 'Jump':
				return _elm_lang$navigation$Navigation$Jump(_p6._0);
			case 'New':
				return _elm_lang$navigation$Navigation$New(_p6._0);
			case 'Modify':
				return _elm_lang$navigation$Navigation$Modify(_p6._0);
			case 'Visit':
				return _elm_lang$navigation$Navigation$Visit(_p6._0);
			default:
				return _elm_lang$navigation$Navigation$Reload(_p6._0);
		}
	});
var _elm_lang$navigation$Navigation$Monitor = function (a) {
	return {ctor: 'Monitor', _0: a};
};
var _elm_lang$navigation$Navigation$program = F2(
	function (locationToMessage, stuff) {
		var init = stuff.init(
			_elm_lang$navigation$Native_Navigation.getLocation(
				{ctor: '_Tuple0'}));
		var subs = function (model) {
			return _elm_lang$core$Platform_Sub$batch(
				{
					ctor: '::',
					_0: _elm_lang$navigation$Navigation$subscription(
						_elm_lang$navigation$Navigation$Monitor(locationToMessage)),
					_1: {
						ctor: '::',
						_0: stuff.subscriptions(model),
						_1: {ctor: '[]'}
					}
				});
		};
		return _elm_lang$html$Html$program(
			{init: init, view: stuff.view, update: stuff.update, subscriptions: subs});
	});
var _elm_lang$navigation$Navigation$programWithFlags = F2(
	function (locationToMessage, stuff) {
		var init = function (flags) {
			return A2(
				stuff.init,
				flags,
				_elm_lang$navigation$Native_Navigation.getLocation(
					{ctor: '_Tuple0'}));
		};
		var subs = function (model) {
			return _elm_lang$core$Platform_Sub$batch(
				{
					ctor: '::',
					_0: _elm_lang$navigation$Navigation$subscription(
						_elm_lang$navigation$Navigation$Monitor(locationToMessage)),
					_1: {
						ctor: '::',
						_0: stuff.subscriptions(model),
						_1: {ctor: '[]'}
					}
				});
		};
		return _elm_lang$html$Html$programWithFlags(
			{init: init, view: stuff.view, update: stuff.update, subscriptions: subs});
	});
var _elm_lang$navigation$Navigation$subMap = F2(
	function (func, _p7) {
		var _p8 = _p7;
		return _elm_lang$navigation$Navigation$Monitor(
			function (_p9) {
				return func(
					_p8._0(_p9));
			});
	});
var _elm_lang$navigation$Navigation$InternetExplorer = F2(
	function (a, b) {
		return {ctor: 'InternetExplorer', _0: a, _1: b};
	});
var _elm_lang$navigation$Navigation$Normal = function (a) {
	return {ctor: 'Normal', _0: a};
};
var _elm_lang$navigation$Navigation$spawnPopWatcher = function (router) {
	var reportLocation = function (_p10) {
		return A2(
			_elm_lang$core$Platform$sendToSelf,
			router,
			_elm_lang$navigation$Native_Navigation.getLocation(
				{ctor: '_Tuple0'}));
	};
	return _elm_lang$navigation$Native_Navigation.isInternetExplorer11(
		{ctor: '_Tuple0'}) ? A3(
		_elm_lang$core$Task$map2,
		_elm_lang$navigation$Navigation$InternetExplorer,
		_elm_lang$core$Process$spawn(
			A3(_elm_lang$dom$Dom_LowLevel$onWindow, 'popstate', _elm_lang$core$Json_Decode$value, reportLocation)),
		_elm_lang$core$Process$spawn(
			A3(_elm_lang$dom$Dom_LowLevel$onWindow, 'hashchange', _elm_lang$core$Json_Decode$value, reportLocation))) : A2(
		_elm_lang$core$Task$map,
		_elm_lang$navigation$Navigation$Normal,
		_elm_lang$core$Process$spawn(
			A3(_elm_lang$dom$Dom_LowLevel$onWindow, 'popstate', _elm_lang$core$Json_Decode$value, reportLocation)));
};
var _elm_lang$navigation$Navigation$onEffects = F4(
	function (router, cmds, subs, _p11) {
		var _p12 = _p11;
		var _p15 = _p12.popWatcher;
		var stepState = function () {
			var _p13 = {ctor: '_Tuple2', _0: subs, _1: _p15};
			_v6_2:
			do {
				if (_p13._0.ctor === '[]') {
					if (_p13._1.ctor === 'Just') {
						return A2(
							_elm_lang$navigation$Navigation_ops['&>'],
							_elm_lang$navigation$Navigation$killPopWatcher(_p13._1._0),
							_elm_lang$core$Task$succeed(
								A2(_elm_lang$navigation$Navigation$State, subs, _elm_lang$core$Maybe$Nothing)));
					} else {
						break _v6_2;
					}
				} else {
					if (_p13._1.ctor === 'Nothing') {
						return A2(
							_elm_lang$core$Task$map,
							function (_p14) {
								return A2(
									_elm_lang$navigation$Navigation$State,
									subs,
									_elm_lang$core$Maybe$Just(_p14));
							},
							_elm_lang$navigation$Navigation$spawnPopWatcher(router));
					} else {
						break _v6_2;
					}
				}
			} while(false);
			return _elm_lang$core$Task$succeed(
				A2(_elm_lang$navigation$Navigation$State, subs, _p15));
		}();
		return A2(
			_elm_lang$navigation$Navigation_ops['&>'],
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					A2(_elm_lang$navigation$Navigation$cmdHelp, router, subs),
					cmds)),
			stepState);
	});
_elm_lang$core$Native_Platform.effectManagers['Navigation'] = {pkg: 'elm-lang/navigation', init: _elm_lang$navigation$Navigation$init, onEffects: _elm_lang$navigation$Navigation$onEffects, onSelfMsg: _elm_lang$navigation$Navigation$onSelfMsg, tag: 'fx', cmdMap: _elm_lang$navigation$Navigation$cmdMap, subMap: _elm_lang$navigation$Navigation$subMap};

var _elm_lang$svg$Svg$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$svg$Svg$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$svg$Svg$svgNamespace = A2(
	_elm_lang$virtual_dom$VirtualDom$property,
	'namespace',
	_elm_lang$core$Json_Encode$string('http://www.w3.org/2000/svg'));
var _elm_lang$svg$Svg$node = F3(
	function (name, attributes, children) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$node,
			name,
			{ctor: '::', _0: _elm_lang$svg$Svg$svgNamespace, _1: attributes},
			children);
	});
var _elm_lang$svg$Svg$svg = _elm_lang$svg$Svg$node('svg');
var _elm_lang$svg$Svg$foreignObject = _elm_lang$svg$Svg$node('foreignObject');
var _elm_lang$svg$Svg$animate = _elm_lang$svg$Svg$node('animate');
var _elm_lang$svg$Svg$animateColor = _elm_lang$svg$Svg$node('animateColor');
var _elm_lang$svg$Svg$animateMotion = _elm_lang$svg$Svg$node('animateMotion');
var _elm_lang$svg$Svg$animateTransform = _elm_lang$svg$Svg$node('animateTransform');
var _elm_lang$svg$Svg$mpath = _elm_lang$svg$Svg$node('mpath');
var _elm_lang$svg$Svg$set = _elm_lang$svg$Svg$node('set');
var _elm_lang$svg$Svg$a = _elm_lang$svg$Svg$node('a');
var _elm_lang$svg$Svg$defs = _elm_lang$svg$Svg$node('defs');
var _elm_lang$svg$Svg$g = _elm_lang$svg$Svg$node('g');
var _elm_lang$svg$Svg$marker = _elm_lang$svg$Svg$node('marker');
var _elm_lang$svg$Svg$mask = _elm_lang$svg$Svg$node('mask');
var _elm_lang$svg$Svg$pattern = _elm_lang$svg$Svg$node('pattern');
var _elm_lang$svg$Svg$switch = _elm_lang$svg$Svg$node('switch');
var _elm_lang$svg$Svg$symbol = _elm_lang$svg$Svg$node('symbol');
var _elm_lang$svg$Svg$desc = _elm_lang$svg$Svg$node('desc');
var _elm_lang$svg$Svg$metadata = _elm_lang$svg$Svg$node('metadata');
var _elm_lang$svg$Svg$title = _elm_lang$svg$Svg$node('title');
var _elm_lang$svg$Svg$feBlend = _elm_lang$svg$Svg$node('feBlend');
var _elm_lang$svg$Svg$feColorMatrix = _elm_lang$svg$Svg$node('feColorMatrix');
var _elm_lang$svg$Svg$feComponentTransfer = _elm_lang$svg$Svg$node('feComponentTransfer');
var _elm_lang$svg$Svg$feComposite = _elm_lang$svg$Svg$node('feComposite');
var _elm_lang$svg$Svg$feConvolveMatrix = _elm_lang$svg$Svg$node('feConvolveMatrix');
var _elm_lang$svg$Svg$feDiffuseLighting = _elm_lang$svg$Svg$node('feDiffuseLighting');
var _elm_lang$svg$Svg$feDisplacementMap = _elm_lang$svg$Svg$node('feDisplacementMap');
var _elm_lang$svg$Svg$feFlood = _elm_lang$svg$Svg$node('feFlood');
var _elm_lang$svg$Svg$feFuncA = _elm_lang$svg$Svg$node('feFuncA');
var _elm_lang$svg$Svg$feFuncB = _elm_lang$svg$Svg$node('feFuncB');
var _elm_lang$svg$Svg$feFuncG = _elm_lang$svg$Svg$node('feFuncG');
var _elm_lang$svg$Svg$feFuncR = _elm_lang$svg$Svg$node('feFuncR');
var _elm_lang$svg$Svg$feGaussianBlur = _elm_lang$svg$Svg$node('feGaussianBlur');
var _elm_lang$svg$Svg$feImage = _elm_lang$svg$Svg$node('feImage');
var _elm_lang$svg$Svg$feMerge = _elm_lang$svg$Svg$node('feMerge');
var _elm_lang$svg$Svg$feMergeNode = _elm_lang$svg$Svg$node('feMergeNode');
var _elm_lang$svg$Svg$feMorphology = _elm_lang$svg$Svg$node('feMorphology');
var _elm_lang$svg$Svg$feOffset = _elm_lang$svg$Svg$node('feOffset');
var _elm_lang$svg$Svg$feSpecularLighting = _elm_lang$svg$Svg$node('feSpecularLighting');
var _elm_lang$svg$Svg$feTile = _elm_lang$svg$Svg$node('feTile');
var _elm_lang$svg$Svg$feTurbulence = _elm_lang$svg$Svg$node('feTurbulence');
var _elm_lang$svg$Svg$font = _elm_lang$svg$Svg$node('font');
var _elm_lang$svg$Svg$linearGradient = _elm_lang$svg$Svg$node('linearGradient');
var _elm_lang$svg$Svg$radialGradient = _elm_lang$svg$Svg$node('radialGradient');
var _elm_lang$svg$Svg$stop = _elm_lang$svg$Svg$node('stop');
var _elm_lang$svg$Svg$circle = _elm_lang$svg$Svg$node('circle');
var _elm_lang$svg$Svg$ellipse = _elm_lang$svg$Svg$node('ellipse');
var _elm_lang$svg$Svg$image = _elm_lang$svg$Svg$node('image');
var _elm_lang$svg$Svg$line = _elm_lang$svg$Svg$node('line');
var _elm_lang$svg$Svg$path = _elm_lang$svg$Svg$node('path');
var _elm_lang$svg$Svg$polygon = _elm_lang$svg$Svg$node('polygon');
var _elm_lang$svg$Svg$polyline = _elm_lang$svg$Svg$node('polyline');
var _elm_lang$svg$Svg$rect = _elm_lang$svg$Svg$node('rect');
var _elm_lang$svg$Svg$use = _elm_lang$svg$Svg$node('use');
var _elm_lang$svg$Svg$feDistantLight = _elm_lang$svg$Svg$node('feDistantLight');
var _elm_lang$svg$Svg$fePointLight = _elm_lang$svg$Svg$node('fePointLight');
var _elm_lang$svg$Svg$feSpotLight = _elm_lang$svg$Svg$node('feSpotLight');
var _elm_lang$svg$Svg$altGlyph = _elm_lang$svg$Svg$node('altGlyph');
var _elm_lang$svg$Svg$altGlyphDef = _elm_lang$svg$Svg$node('altGlyphDef');
var _elm_lang$svg$Svg$altGlyphItem = _elm_lang$svg$Svg$node('altGlyphItem');
var _elm_lang$svg$Svg$glyph = _elm_lang$svg$Svg$node('glyph');
var _elm_lang$svg$Svg$glyphRef = _elm_lang$svg$Svg$node('glyphRef');
var _elm_lang$svg$Svg$textPath = _elm_lang$svg$Svg$node('textPath');
var _elm_lang$svg$Svg$text_ = _elm_lang$svg$Svg$node('text');
var _elm_lang$svg$Svg$tref = _elm_lang$svg$Svg$node('tref');
var _elm_lang$svg$Svg$tspan = _elm_lang$svg$Svg$node('tspan');
var _elm_lang$svg$Svg$clipPath = _elm_lang$svg$Svg$node('clipPath');
var _elm_lang$svg$Svg$colorProfile = _elm_lang$svg$Svg$node('colorProfile');
var _elm_lang$svg$Svg$cursor = _elm_lang$svg$Svg$node('cursor');
var _elm_lang$svg$Svg$filter = _elm_lang$svg$Svg$node('filter');
var _elm_lang$svg$Svg$script = _elm_lang$svg$Svg$node('script');
var _elm_lang$svg$Svg$style = _elm_lang$svg$Svg$node('style');
var _elm_lang$svg$Svg$view = _elm_lang$svg$Svg$node('view');

var _elm_lang$svg$Svg_Attributes$writingMode = _elm_lang$virtual_dom$VirtualDom$attribute('writing-mode');
var _elm_lang$svg$Svg_Attributes$wordSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('word-spacing');
var _elm_lang$svg$Svg_Attributes$visibility = _elm_lang$virtual_dom$VirtualDom$attribute('visibility');
var _elm_lang$svg$Svg_Attributes$unicodeBidi = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-bidi');
var _elm_lang$svg$Svg_Attributes$textRendering = _elm_lang$virtual_dom$VirtualDom$attribute('text-rendering');
var _elm_lang$svg$Svg_Attributes$textDecoration = _elm_lang$virtual_dom$VirtualDom$attribute('text-decoration');
var _elm_lang$svg$Svg_Attributes$textAnchor = _elm_lang$virtual_dom$VirtualDom$attribute('text-anchor');
var _elm_lang$svg$Svg_Attributes$stroke = _elm_lang$virtual_dom$VirtualDom$attribute('stroke');
var _elm_lang$svg$Svg_Attributes$strokeWidth = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-width');
var _elm_lang$svg$Svg_Attributes$strokeOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-opacity');
var _elm_lang$svg$Svg_Attributes$strokeMiterlimit = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-miterlimit');
var _elm_lang$svg$Svg_Attributes$strokeLinejoin = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linejoin');
var _elm_lang$svg$Svg_Attributes$strokeLinecap = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linecap');
var _elm_lang$svg$Svg_Attributes$strokeDashoffset = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dashoffset');
var _elm_lang$svg$Svg_Attributes$strokeDasharray = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dasharray');
var _elm_lang$svg$Svg_Attributes$stopOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stop-opacity');
var _elm_lang$svg$Svg_Attributes$stopColor = _elm_lang$virtual_dom$VirtualDom$attribute('stop-color');
var _elm_lang$svg$Svg_Attributes$shapeRendering = _elm_lang$virtual_dom$VirtualDom$attribute('shape-rendering');
var _elm_lang$svg$Svg_Attributes$pointerEvents = _elm_lang$virtual_dom$VirtualDom$attribute('pointer-events');
var _elm_lang$svg$Svg_Attributes$overflow = _elm_lang$virtual_dom$VirtualDom$attribute('overflow');
var _elm_lang$svg$Svg_Attributes$opacity = _elm_lang$virtual_dom$VirtualDom$attribute('opacity');
var _elm_lang$svg$Svg_Attributes$mask = _elm_lang$virtual_dom$VirtualDom$attribute('mask');
var _elm_lang$svg$Svg_Attributes$markerStart = _elm_lang$virtual_dom$VirtualDom$attribute('marker-start');
var _elm_lang$svg$Svg_Attributes$markerMid = _elm_lang$virtual_dom$VirtualDom$attribute('marker-mid');
var _elm_lang$svg$Svg_Attributes$markerEnd = _elm_lang$virtual_dom$VirtualDom$attribute('marker-end');
var _elm_lang$svg$Svg_Attributes$lightingColor = _elm_lang$virtual_dom$VirtualDom$attribute('lighting-color');
var _elm_lang$svg$Svg_Attributes$letterSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('letter-spacing');
var _elm_lang$svg$Svg_Attributes$kerning = _elm_lang$virtual_dom$VirtualDom$attribute('kerning');
var _elm_lang$svg$Svg_Attributes$imageRendering = _elm_lang$virtual_dom$VirtualDom$attribute('image-rendering');
var _elm_lang$svg$Svg_Attributes$glyphOrientationVertical = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-vertical');
var _elm_lang$svg$Svg_Attributes$glyphOrientationHorizontal = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-horizontal');
var _elm_lang$svg$Svg_Attributes$fontWeight = _elm_lang$virtual_dom$VirtualDom$attribute('font-weight');
var _elm_lang$svg$Svg_Attributes$fontVariant = _elm_lang$virtual_dom$VirtualDom$attribute('font-variant');
var _elm_lang$svg$Svg_Attributes$fontStyle = _elm_lang$virtual_dom$VirtualDom$attribute('font-style');
var _elm_lang$svg$Svg_Attributes$fontStretch = _elm_lang$virtual_dom$VirtualDom$attribute('font-stretch');
var _elm_lang$svg$Svg_Attributes$fontSize = _elm_lang$virtual_dom$VirtualDom$attribute('font-size');
var _elm_lang$svg$Svg_Attributes$fontSizeAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('font-size-adjust');
var _elm_lang$svg$Svg_Attributes$fontFamily = _elm_lang$virtual_dom$VirtualDom$attribute('font-family');
var _elm_lang$svg$Svg_Attributes$floodOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('flood-opacity');
var _elm_lang$svg$Svg_Attributes$floodColor = _elm_lang$virtual_dom$VirtualDom$attribute('flood-color');
var _elm_lang$svg$Svg_Attributes$filter = _elm_lang$virtual_dom$VirtualDom$attribute('filter');
var _elm_lang$svg$Svg_Attributes$fill = _elm_lang$virtual_dom$VirtualDom$attribute('fill');
var _elm_lang$svg$Svg_Attributes$fillRule = _elm_lang$virtual_dom$VirtualDom$attribute('fill-rule');
var _elm_lang$svg$Svg_Attributes$fillOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('fill-opacity');
var _elm_lang$svg$Svg_Attributes$enableBackground = _elm_lang$virtual_dom$VirtualDom$attribute('enable-background');
var _elm_lang$svg$Svg_Attributes$dominantBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('dominant-baseline');
var _elm_lang$svg$Svg_Attributes$display = _elm_lang$virtual_dom$VirtualDom$attribute('display');
var _elm_lang$svg$Svg_Attributes$direction = _elm_lang$virtual_dom$VirtualDom$attribute('direction');
var _elm_lang$svg$Svg_Attributes$cursor = _elm_lang$virtual_dom$VirtualDom$attribute('cursor');
var _elm_lang$svg$Svg_Attributes$color = _elm_lang$virtual_dom$VirtualDom$attribute('color');
var _elm_lang$svg$Svg_Attributes$colorRendering = _elm_lang$virtual_dom$VirtualDom$attribute('color-rendering');
var _elm_lang$svg$Svg_Attributes$colorProfile = _elm_lang$virtual_dom$VirtualDom$attribute('color-profile');
var _elm_lang$svg$Svg_Attributes$colorInterpolation = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation');
var _elm_lang$svg$Svg_Attributes$colorInterpolationFilters = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation-filters');
var _elm_lang$svg$Svg_Attributes$clip = _elm_lang$virtual_dom$VirtualDom$attribute('clip');
var _elm_lang$svg$Svg_Attributes$clipRule = _elm_lang$virtual_dom$VirtualDom$attribute('clip-rule');
var _elm_lang$svg$Svg_Attributes$clipPath = _elm_lang$virtual_dom$VirtualDom$attribute('clip-path');
var _elm_lang$svg$Svg_Attributes$baselineShift = _elm_lang$virtual_dom$VirtualDom$attribute('baseline-shift');
var _elm_lang$svg$Svg_Attributes$alignmentBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('alignment-baseline');
var _elm_lang$svg$Svg_Attributes$zoomAndPan = _elm_lang$virtual_dom$VirtualDom$attribute('zoomAndPan');
var _elm_lang$svg$Svg_Attributes$z = _elm_lang$virtual_dom$VirtualDom$attribute('z');
var _elm_lang$svg$Svg_Attributes$yChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('yChannelSelector');
var _elm_lang$svg$Svg_Attributes$y2 = _elm_lang$virtual_dom$VirtualDom$attribute('y2');
var _elm_lang$svg$Svg_Attributes$y1 = _elm_lang$virtual_dom$VirtualDom$attribute('y1');
var _elm_lang$svg$Svg_Attributes$y = _elm_lang$virtual_dom$VirtualDom$attribute('y');
var _elm_lang$svg$Svg_Attributes$xmlSpace = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var _elm_lang$svg$Svg_Attributes$xmlLang = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:lang');
var _elm_lang$svg$Svg_Attributes$xmlBase = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:base');
var _elm_lang$svg$Svg_Attributes$xlinkType = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:type');
var _elm_lang$svg$Svg_Attributes$xlinkTitle = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:title');
var _elm_lang$svg$Svg_Attributes$xlinkShow = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:show');
var _elm_lang$svg$Svg_Attributes$xlinkRole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:role');
var _elm_lang$svg$Svg_Attributes$xlinkHref = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:href');
var _elm_lang$svg$Svg_Attributes$xlinkArcrole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:arcrole');
var _elm_lang$svg$Svg_Attributes$xlinkActuate = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:actuate');
var _elm_lang$svg$Svg_Attributes$xChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('xChannelSelector');
var _elm_lang$svg$Svg_Attributes$x2 = _elm_lang$virtual_dom$VirtualDom$attribute('x2');
var _elm_lang$svg$Svg_Attributes$x1 = _elm_lang$virtual_dom$VirtualDom$attribute('x1');
var _elm_lang$svg$Svg_Attributes$xHeight = _elm_lang$virtual_dom$VirtualDom$attribute('x-height');
var _elm_lang$svg$Svg_Attributes$x = _elm_lang$virtual_dom$VirtualDom$attribute('x');
var _elm_lang$svg$Svg_Attributes$widths = _elm_lang$virtual_dom$VirtualDom$attribute('widths');
var _elm_lang$svg$Svg_Attributes$width = _elm_lang$virtual_dom$VirtualDom$attribute('width');
var _elm_lang$svg$Svg_Attributes$viewTarget = _elm_lang$virtual_dom$VirtualDom$attribute('viewTarget');
var _elm_lang$svg$Svg_Attributes$viewBox = _elm_lang$virtual_dom$VirtualDom$attribute('viewBox');
var _elm_lang$svg$Svg_Attributes$vertOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-y');
var _elm_lang$svg$Svg_Attributes$vertOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-x');
var _elm_lang$svg$Svg_Attributes$vertAdvY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-adv-y');
var _elm_lang$svg$Svg_Attributes$version = _elm_lang$virtual_dom$VirtualDom$attribute('version');
var _elm_lang$svg$Svg_Attributes$values = _elm_lang$virtual_dom$VirtualDom$attribute('values');
var _elm_lang$svg$Svg_Attributes$vMathematical = _elm_lang$virtual_dom$VirtualDom$attribute('v-mathematical');
var _elm_lang$svg$Svg_Attributes$vIdeographic = _elm_lang$virtual_dom$VirtualDom$attribute('v-ideographic');
var _elm_lang$svg$Svg_Attributes$vHanging = _elm_lang$virtual_dom$VirtualDom$attribute('v-hanging');
var _elm_lang$svg$Svg_Attributes$vAlphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('v-alphabetic');
var _elm_lang$svg$Svg_Attributes$unitsPerEm = _elm_lang$virtual_dom$VirtualDom$attribute('units-per-em');
var _elm_lang$svg$Svg_Attributes$unicodeRange = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-range');
var _elm_lang$svg$Svg_Attributes$unicode = _elm_lang$virtual_dom$VirtualDom$attribute('unicode');
var _elm_lang$svg$Svg_Attributes$underlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('underline-thickness');
var _elm_lang$svg$Svg_Attributes$underlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('underline-position');
var _elm_lang$svg$Svg_Attributes$u2 = _elm_lang$virtual_dom$VirtualDom$attribute('u2');
var _elm_lang$svg$Svg_Attributes$u1 = _elm_lang$virtual_dom$VirtualDom$attribute('u1');
var _elm_lang$svg$Svg_Attributes$type_ = _elm_lang$virtual_dom$VirtualDom$attribute('type');
var _elm_lang$svg$Svg_Attributes$transform = _elm_lang$virtual_dom$VirtualDom$attribute('transform');
var _elm_lang$svg$Svg_Attributes$to = _elm_lang$virtual_dom$VirtualDom$attribute('to');
var _elm_lang$svg$Svg_Attributes$title = _elm_lang$virtual_dom$VirtualDom$attribute('title');
var _elm_lang$svg$Svg_Attributes$textLength = _elm_lang$virtual_dom$VirtualDom$attribute('textLength');
var _elm_lang$svg$Svg_Attributes$targetY = _elm_lang$virtual_dom$VirtualDom$attribute('targetY');
var _elm_lang$svg$Svg_Attributes$targetX = _elm_lang$virtual_dom$VirtualDom$attribute('targetX');
var _elm_lang$svg$Svg_Attributes$target = _elm_lang$virtual_dom$VirtualDom$attribute('target');
var _elm_lang$svg$Svg_Attributes$tableValues = _elm_lang$virtual_dom$VirtualDom$attribute('tableValues');
var _elm_lang$svg$Svg_Attributes$systemLanguage = _elm_lang$virtual_dom$VirtualDom$attribute('systemLanguage');
var _elm_lang$svg$Svg_Attributes$surfaceScale = _elm_lang$virtual_dom$VirtualDom$attribute('surfaceScale');
var _elm_lang$svg$Svg_Attributes$style = _elm_lang$virtual_dom$VirtualDom$attribute('style');
var _elm_lang$svg$Svg_Attributes$string = _elm_lang$virtual_dom$VirtualDom$attribute('string');
var _elm_lang$svg$Svg_Attributes$strikethroughThickness = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-thickness');
var _elm_lang$svg$Svg_Attributes$strikethroughPosition = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-position');
var _elm_lang$svg$Svg_Attributes$stitchTiles = _elm_lang$virtual_dom$VirtualDom$attribute('stitchTiles');
var _elm_lang$svg$Svg_Attributes$stemv = _elm_lang$virtual_dom$VirtualDom$attribute('stemv');
var _elm_lang$svg$Svg_Attributes$stemh = _elm_lang$virtual_dom$VirtualDom$attribute('stemh');
var _elm_lang$svg$Svg_Attributes$stdDeviation = _elm_lang$virtual_dom$VirtualDom$attribute('stdDeviation');
var _elm_lang$svg$Svg_Attributes$startOffset = _elm_lang$virtual_dom$VirtualDom$attribute('startOffset');
var _elm_lang$svg$Svg_Attributes$spreadMethod = _elm_lang$virtual_dom$VirtualDom$attribute('spreadMethod');
var _elm_lang$svg$Svg_Attributes$speed = _elm_lang$virtual_dom$VirtualDom$attribute('speed');
var _elm_lang$svg$Svg_Attributes$specularExponent = _elm_lang$virtual_dom$VirtualDom$attribute('specularExponent');
var _elm_lang$svg$Svg_Attributes$specularConstant = _elm_lang$virtual_dom$VirtualDom$attribute('specularConstant');
var _elm_lang$svg$Svg_Attributes$spacing = _elm_lang$virtual_dom$VirtualDom$attribute('spacing');
var _elm_lang$svg$Svg_Attributes$slope = _elm_lang$virtual_dom$VirtualDom$attribute('slope');
var _elm_lang$svg$Svg_Attributes$seed = _elm_lang$virtual_dom$VirtualDom$attribute('seed');
var _elm_lang$svg$Svg_Attributes$scale = _elm_lang$virtual_dom$VirtualDom$attribute('scale');
var _elm_lang$svg$Svg_Attributes$ry = _elm_lang$virtual_dom$VirtualDom$attribute('ry');
var _elm_lang$svg$Svg_Attributes$rx = _elm_lang$virtual_dom$VirtualDom$attribute('rx');
var _elm_lang$svg$Svg_Attributes$rotate = _elm_lang$virtual_dom$VirtualDom$attribute('rotate');
var _elm_lang$svg$Svg_Attributes$result = _elm_lang$virtual_dom$VirtualDom$attribute('result');
var _elm_lang$svg$Svg_Attributes$restart = _elm_lang$virtual_dom$VirtualDom$attribute('restart');
var _elm_lang$svg$Svg_Attributes$requiredFeatures = _elm_lang$virtual_dom$VirtualDom$attribute('requiredFeatures');
var _elm_lang$svg$Svg_Attributes$requiredExtensions = _elm_lang$virtual_dom$VirtualDom$attribute('requiredExtensions');
var _elm_lang$svg$Svg_Attributes$repeatDur = _elm_lang$virtual_dom$VirtualDom$attribute('repeatDur');
var _elm_lang$svg$Svg_Attributes$repeatCount = _elm_lang$virtual_dom$VirtualDom$attribute('repeatCount');
var _elm_lang$svg$Svg_Attributes$renderingIntent = _elm_lang$virtual_dom$VirtualDom$attribute('rendering-intent');
var _elm_lang$svg$Svg_Attributes$refY = _elm_lang$virtual_dom$VirtualDom$attribute('refY');
var _elm_lang$svg$Svg_Attributes$refX = _elm_lang$virtual_dom$VirtualDom$attribute('refX');
var _elm_lang$svg$Svg_Attributes$radius = _elm_lang$virtual_dom$VirtualDom$attribute('radius');
var _elm_lang$svg$Svg_Attributes$r = _elm_lang$virtual_dom$VirtualDom$attribute('r');
var _elm_lang$svg$Svg_Attributes$primitiveUnits = _elm_lang$virtual_dom$VirtualDom$attribute('primitiveUnits');
var _elm_lang$svg$Svg_Attributes$preserveAspectRatio = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAspectRatio');
var _elm_lang$svg$Svg_Attributes$preserveAlpha = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAlpha');
var _elm_lang$svg$Svg_Attributes$pointsAtZ = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtZ');
var _elm_lang$svg$Svg_Attributes$pointsAtY = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtY');
var _elm_lang$svg$Svg_Attributes$pointsAtX = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtX');
var _elm_lang$svg$Svg_Attributes$points = _elm_lang$virtual_dom$VirtualDom$attribute('points');
var _elm_lang$svg$Svg_Attributes$pointOrder = _elm_lang$virtual_dom$VirtualDom$attribute('point-order');
var _elm_lang$svg$Svg_Attributes$patternUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternUnits');
var _elm_lang$svg$Svg_Attributes$patternTransform = _elm_lang$virtual_dom$VirtualDom$attribute('patternTransform');
var _elm_lang$svg$Svg_Attributes$patternContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternContentUnits');
var _elm_lang$svg$Svg_Attributes$pathLength = _elm_lang$virtual_dom$VirtualDom$attribute('pathLength');
var _elm_lang$svg$Svg_Attributes$path = _elm_lang$virtual_dom$VirtualDom$attribute('path');
var _elm_lang$svg$Svg_Attributes$panose1 = _elm_lang$virtual_dom$VirtualDom$attribute('panose-1');
var _elm_lang$svg$Svg_Attributes$overlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('overline-thickness');
var _elm_lang$svg$Svg_Attributes$overlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('overline-position');
var _elm_lang$svg$Svg_Attributes$origin = _elm_lang$virtual_dom$VirtualDom$attribute('origin');
var _elm_lang$svg$Svg_Attributes$orientation = _elm_lang$virtual_dom$VirtualDom$attribute('orientation');
var _elm_lang$svg$Svg_Attributes$orient = _elm_lang$virtual_dom$VirtualDom$attribute('orient');
var _elm_lang$svg$Svg_Attributes$order = _elm_lang$virtual_dom$VirtualDom$attribute('order');
var _elm_lang$svg$Svg_Attributes$operator = _elm_lang$virtual_dom$VirtualDom$attribute('operator');
var _elm_lang$svg$Svg_Attributes$offset = _elm_lang$virtual_dom$VirtualDom$attribute('offset');
var _elm_lang$svg$Svg_Attributes$numOctaves = _elm_lang$virtual_dom$VirtualDom$attribute('numOctaves');
var _elm_lang$svg$Svg_Attributes$name = _elm_lang$virtual_dom$VirtualDom$attribute('name');
var _elm_lang$svg$Svg_Attributes$mode = _elm_lang$virtual_dom$VirtualDom$attribute('mode');
var _elm_lang$svg$Svg_Attributes$min = _elm_lang$virtual_dom$VirtualDom$attribute('min');
var _elm_lang$svg$Svg_Attributes$method = _elm_lang$virtual_dom$VirtualDom$attribute('method');
var _elm_lang$svg$Svg_Attributes$media = _elm_lang$virtual_dom$VirtualDom$attribute('media');
var _elm_lang$svg$Svg_Attributes$max = _elm_lang$virtual_dom$VirtualDom$attribute('max');
var _elm_lang$svg$Svg_Attributes$mathematical = _elm_lang$virtual_dom$VirtualDom$attribute('mathematical');
var _elm_lang$svg$Svg_Attributes$maskUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskUnits');
var _elm_lang$svg$Svg_Attributes$maskContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskContentUnits');
var _elm_lang$svg$Svg_Attributes$markerWidth = _elm_lang$virtual_dom$VirtualDom$attribute('markerWidth');
var _elm_lang$svg$Svg_Attributes$markerUnits = _elm_lang$virtual_dom$VirtualDom$attribute('markerUnits');
var _elm_lang$svg$Svg_Attributes$markerHeight = _elm_lang$virtual_dom$VirtualDom$attribute('markerHeight');
var _elm_lang$svg$Svg_Attributes$local = _elm_lang$virtual_dom$VirtualDom$attribute('local');
var _elm_lang$svg$Svg_Attributes$limitingConeAngle = _elm_lang$virtual_dom$VirtualDom$attribute('limitingConeAngle');
var _elm_lang$svg$Svg_Attributes$lengthAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('lengthAdjust');
var _elm_lang$svg$Svg_Attributes$lang = _elm_lang$virtual_dom$VirtualDom$attribute('lang');
var _elm_lang$svg$Svg_Attributes$keyTimes = _elm_lang$virtual_dom$VirtualDom$attribute('keyTimes');
var _elm_lang$svg$Svg_Attributes$keySplines = _elm_lang$virtual_dom$VirtualDom$attribute('keySplines');
var _elm_lang$svg$Svg_Attributes$keyPoints = _elm_lang$virtual_dom$VirtualDom$attribute('keyPoints');
var _elm_lang$svg$Svg_Attributes$kernelUnitLength = _elm_lang$virtual_dom$VirtualDom$attribute('kernelUnitLength');
var _elm_lang$svg$Svg_Attributes$kernelMatrix = _elm_lang$virtual_dom$VirtualDom$attribute('kernelMatrix');
var _elm_lang$svg$Svg_Attributes$k4 = _elm_lang$virtual_dom$VirtualDom$attribute('k4');
var _elm_lang$svg$Svg_Attributes$k3 = _elm_lang$virtual_dom$VirtualDom$attribute('k3');
var _elm_lang$svg$Svg_Attributes$k2 = _elm_lang$virtual_dom$VirtualDom$attribute('k2');
var _elm_lang$svg$Svg_Attributes$k1 = _elm_lang$virtual_dom$VirtualDom$attribute('k1');
var _elm_lang$svg$Svg_Attributes$k = _elm_lang$virtual_dom$VirtualDom$attribute('k');
var _elm_lang$svg$Svg_Attributes$intercept = _elm_lang$virtual_dom$VirtualDom$attribute('intercept');
var _elm_lang$svg$Svg_Attributes$in2 = _elm_lang$virtual_dom$VirtualDom$attribute('in2');
var _elm_lang$svg$Svg_Attributes$in_ = _elm_lang$virtual_dom$VirtualDom$attribute('in');
var _elm_lang$svg$Svg_Attributes$ideographic = _elm_lang$virtual_dom$VirtualDom$attribute('ideographic');
var _elm_lang$svg$Svg_Attributes$id = _elm_lang$virtual_dom$VirtualDom$attribute('id');
var _elm_lang$svg$Svg_Attributes$horizOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-y');
var _elm_lang$svg$Svg_Attributes$horizOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-x');
var _elm_lang$svg$Svg_Attributes$horizAdvX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-adv-x');
var _elm_lang$svg$Svg_Attributes$height = _elm_lang$virtual_dom$VirtualDom$attribute('height');
var _elm_lang$svg$Svg_Attributes$hanging = _elm_lang$virtual_dom$VirtualDom$attribute('hanging');
var _elm_lang$svg$Svg_Attributes$gradientUnits = _elm_lang$virtual_dom$VirtualDom$attribute('gradientUnits');
var _elm_lang$svg$Svg_Attributes$gradientTransform = _elm_lang$virtual_dom$VirtualDom$attribute('gradientTransform');
var _elm_lang$svg$Svg_Attributes$glyphRef = _elm_lang$virtual_dom$VirtualDom$attribute('glyphRef');
var _elm_lang$svg$Svg_Attributes$glyphName = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-name');
var _elm_lang$svg$Svg_Attributes$g2 = _elm_lang$virtual_dom$VirtualDom$attribute('g2');
var _elm_lang$svg$Svg_Attributes$g1 = _elm_lang$virtual_dom$VirtualDom$attribute('g1');
var _elm_lang$svg$Svg_Attributes$fy = _elm_lang$virtual_dom$VirtualDom$attribute('fy');
var _elm_lang$svg$Svg_Attributes$fx = _elm_lang$virtual_dom$VirtualDom$attribute('fx');
var _elm_lang$svg$Svg_Attributes$from = _elm_lang$virtual_dom$VirtualDom$attribute('from');
var _elm_lang$svg$Svg_Attributes$format = _elm_lang$virtual_dom$VirtualDom$attribute('format');
var _elm_lang$svg$Svg_Attributes$filterUnits = _elm_lang$virtual_dom$VirtualDom$attribute('filterUnits');
var _elm_lang$svg$Svg_Attributes$filterRes = _elm_lang$virtual_dom$VirtualDom$attribute('filterRes');
var _elm_lang$svg$Svg_Attributes$externalResourcesRequired = _elm_lang$virtual_dom$VirtualDom$attribute('externalResourcesRequired');
var _elm_lang$svg$Svg_Attributes$exponent = _elm_lang$virtual_dom$VirtualDom$attribute('exponent');
var _elm_lang$svg$Svg_Attributes$end = _elm_lang$virtual_dom$VirtualDom$attribute('end');
var _elm_lang$svg$Svg_Attributes$elevation = _elm_lang$virtual_dom$VirtualDom$attribute('elevation');
var _elm_lang$svg$Svg_Attributes$edgeMode = _elm_lang$virtual_dom$VirtualDom$attribute('edgeMode');
var _elm_lang$svg$Svg_Attributes$dy = _elm_lang$virtual_dom$VirtualDom$attribute('dy');
var _elm_lang$svg$Svg_Attributes$dx = _elm_lang$virtual_dom$VirtualDom$attribute('dx');
var _elm_lang$svg$Svg_Attributes$dur = _elm_lang$virtual_dom$VirtualDom$attribute('dur');
var _elm_lang$svg$Svg_Attributes$divisor = _elm_lang$virtual_dom$VirtualDom$attribute('divisor');
var _elm_lang$svg$Svg_Attributes$diffuseConstant = _elm_lang$virtual_dom$VirtualDom$attribute('diffuseConstant');
var _elm_lang$svg$Svg_Attributes$descent = _elm_lang$virtual_dom$VirtualDom$attribute('descent');
var _elm_lang$svg$Svg_Attributes$decelerate = _elm_lang$virtual_dom$VirtualDom$attribute('decelerate');
var _elm_lang$svg$Svg_Attributes$d = _elm_lang$virtual_dom$VirtualDom$attribute('d');
var _elm_lang$svg$Svg_Attributes$cy = _elm_lang$virtual_dom$VirtualDom$attribute('cy');
var _elm_lang$svg$Svg_Attributes$cx = _elm_lang$virtual_dom$VirtualDom$attribute('cx');
var _elm_lang$svg$Svg_Attributes$contentStyleType = _elm_lang$virtual_dom$VirtualDom$attribute('contentStyleType');
var _elm_lang$svg$Svg_Attributes$contentScriptType = _elm_lang$virtual_dom$VirtualDom$attribute('contentScriptType');
var _elm_lang$svg$Svg_Attributes$clipPathUnits = _elm_lang$virtual_dom$VirtualDom$attribute('clipPathUnits');
var _elm_lang$svg$Svg_Attributes$class = _elm_lang$virtual_dom$VirtualDom$attribute('class');
var _elm_lang$svg$Svg_Attributes$capHeight = _elm_lang$virtual_dom$VirtualDom$attribute('cap-height');
var _elm_lang$svg$Svg_Attributes$calcMode = _elm_lang$virtual_dom$VirtualDom$attribute('calcMode');
var _elm_lang$svg$Svg_Attributes$by = _elm_lang$virtual_dom$VirtualDom$attribute('by');
var _elm_lang$svg$Svg_Attributes$bias = _elm_lang$virtual_dom$VirtualDom$attribute('bias');
var _elm_lang$svg$Svg_Attributes$begin = _elm_lang$virtual_dom$VirtualDom$attribute('begin');
var _elm_lang$svg$Svg_Attributes$bbox = _elm_lang$virtual_dom$VirtualDom$attribute('bbox');
var _elm_lang$svg$Svg_Attributes$baseProfile = _elm_lang$virtual_dom$VirtualDom$attribute('baseProfile');
var _elm_lang$svg$Svg_Attributes$baseFrequency = _elm_lang$virtual_dom$VirtualDom$attribute('baseFrequency');
var _elm_lang$svg$Svg_Attributes$azimuth = _elm_lang$virtual_dom$VirtualDom$attribute('azimuth');
var _elm_lang$svg$Svg_Attributes$autoReverse = _elm_lang$virtual_dom$VirtualDom$attribute('autoReverse');
var _elm_lang$svg$Svg_Attributes$attributeType = _elm_lang$virtual_dom$VirtualDom$attribute('attributeType');
var _elm_lang$svg$Svg_Attributes$attributeName = _elm_lang$virtual_dom$VirtualDom$attribute('attributeName');
var _elm_lang$svg$Svg_Attributes$ascent = _elm_lang$virtual_dom$VirtualDom$attribute('ascent');
var _elm_lang$svg$Svg_Attributes$arabicForm = _elm_lang$virtual_dom$VirtualDom$attribute('arabic-form');
var _elm_lang$svg$Svg_Attributes$amplitude = _elm_lang$virtual_dom$VirtualDom$attribute('amplitude');
var _elm_lang$svg$Svg_Attributes$allowReorder = _elm_lang$virtual_dom$VirtualDom$attribute('allowReorder');
var _elm_lang$svg$Svg_Attributes$alphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('alphabetic');
var _elm_lang$svg$Svg_Attributes$additive = _elm_lang$virtual_dom$VirtualDom$attribute('additive');
var _elm_lang$svg$Svg_Attributes$accumulate = _elm_lang$virtual_dom$VirtualDom$attribute('accumulate');
var _elm_lang$svg$Svg_Attributes$accelerate = _elm_lang$virtual_dom$VirtualDom$attribute('accelerate');
var _elm_lang$svg$Svg_Attributes$accentHeight = _elm_lang$virtual_dom$VirtualDom$attribute('accent-height');

var _user$project$Native_Window = {
  size: function(a) {
    return {
      width: window.innerWidth,
      height: window.innerHeight
    }
  }
};

var _user$project$Native_Timestamp = { timestamp: function(a) { return Date.now() }}

var _user$project$Native_Random = { random: function(a) { return Math.round(1024*1024*1024 * Math.random()) }}

var _user$project$Native_Size = {
  /* Measure the size of the html string. In order to get the size, we
   * have to actually render it, which we do offscreen. */
  size: function(htmlStr) {
    var div = document.createElement('div');
    div.innerHTML = htmlStr;
    div.style.position = 'absolute';
    div.style.top = '-1000px';
    div.style.left = '-1000px';
    document.body.appendChild(div);
    var bb = div.getBoundingClientRect();
    document.body.removeChild(div);
    return bb;
  }
};

var _user$project$Util$containsOrdered = F2(
	function (needle, haystack) {
		containsOrdered:
		while (true) {
			var _p0 = {ctor: '_Tuple2', _0: needle, _1: haystack};
			if (_p0._0.ctor === '[]') {
				return true;
			} else {
				if (_p0._1.ctor === '[]') {
					return false;
				} else {
					var _p3 = _p0._0._1;
					var _p2 = _p0._0._0;
					var _p1 = _p0._1._1;
					if (_elm_lang$core$Native_Utils.eq(_p2, _p0._1._0)) {
						var _v1 = _p3,
							_v2 = _p1;
						needle = _v1;
						haystack = _v2;
						continue containsOrdered;
					} else {
						var _v3 = {ctor: '::', _0: _p2, _1: _p3},
							_v4 = _p1;
						needle = _v3;
						haystack = _v4;
						continue containsOrdered;
					}
				}
			}
		}
	});
var _user$project$Util$combineResult = function (results) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (r, rs) {
				var _p4 = {ctor: '_Tuple2', _0: r, _1: rs};
				if (_p4._0.ctor === 'Ok') {
					if (_p4._1.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							{ctor: '::', _0: _p4._0._0, _1: _p4._1._0});
					} else {
						return _elm_lang$core$Result$Err(_p4._1._0);
					}
				} else {
					if (_p4._1.ctor === 'Ok') {
						return _elm_lang$core$Result$Err(
							{
								ctor: '::',
								_0: _p4._0._0,
								_1: {ctor: '[]'}
							});
					} else {
						return _elm_lang$core$Result$Err(
							{ctor: '::', _0: _p4._0._0, _1: _p4._1._0});
					}
				}
			}),
		_elm_lang$core$Result$Ok(
			{ctor: '[]'}),
		results);
};
var _user$project$Util$logF = F3(
	function (msg, fn, obj) {
		var _p5 = A2(
			_elm_lang$core$Debug$log,
			msg,
			fn(obj));
		return obj;
	});
var _user$project$Util$letter2int = function (s) {
	return A2(
		F2(
			function (x, y) {
				return x * y;
			}),
		-1,
		A2(
			F2(
				function (x, y) {
					return x - y;
				}),
			_elm_lang$core$Char$toCode(
				_elm_lang$core$Native_Utils.chr('a')),
			_elm_lang$core$Char$toCode(
				_elm_lang$core$Tuple$first(
					A2(
						_elm_lang$core$Maybe$withDefault,
						{
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.chr('!'),
							_1: ''
						},
						_elm_lang$core$String$uncons(s))))));
};
var _user$project$Util$int2letter = function (i) {
	return _elm_lang$core$String$fromChar(
		_elm_lang$core$Char$fromCode(
			A2(
				F2(
					function (x, y) {
						return x + y;
					}),
				i,
				_elm_lang$core$Char$toCode(
					_elm_lang$core$Native_Utils.chr('a')))));
};
var _user$project$Util$resultIsOk = function (r) {
	var _p6 = r;
	if (_p6.ctor === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var _user$project$Util$zip = F2(
	function (a, b) {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			a,
			b);
	});
var _user$project$Util$uniqueCombinations = function (xs) {
	return A2(
		_elm_lang$core$List$filter,
		function (_p7) {
			var _p8 = _p7;
			return !_elm_lang$core$Native_Utils.eq(_p8._0, _p8._1);
		},
		A2(
			_elm_lang$core$List$concatMap,
			function (_p9) {
				var _p10 = _p9;
				return A2(
					_elm_lang$core$List$map,
					function (y) {
						return {ctor: '_Tuple2', _0: _p10._0, _1: y};
					},
					_p10._1);
			},
			A2(
				_user$project$Util$zip,
				xs,
				_elm_community$list_extra$List_Extra$tails(xs))));
};
var _user$project$Util$findIndex = F2(
	function (fn, l) {
		return A2(
			_elm_community$list_extra$List_Extra$find,
			function (_p11) {
				var _p12 = _p11;
				return fn(_p12._1);
			},
			A2(
				_elm_lang$core$List$indexedMap,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					}),
				l));
	});
var _user$project$Util$replace = F3(
	function (re, repl, str) {
		return A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex(re),
			function (_p13) {
				return repl;
			},
			str);
	});
var _user$project$Util$rematch = F2(
	function (re, s) {
		return A2(
			_elm_lang$core$Regex$contains,
			_elm_lang$core$Regex$regex(re),
			s);
	});
var _user$project$Util$toIntWithDefault = F2(
	function (d, s) {
		return A2(
			_elm_lang$core$Result$withDefault,
			d,
			_elm_lang$core$String$toInt(s));
	});
var _user$project$Util$deMaybeM = F2(
	function (msg, x) {
		var _p14 = x;
		if (_p14.ctor === 'Just') {
			return _p14._0;
		} else {
			return _elm_lang$core$Native_Utils.crashCase(
				'Util',
				{
					start: {line: 32, column: 3},
					end: {line: 34, column: 31}
				},
				_p14)(msg);
		}
	});
var _user$project$Util$deMaybe = _user$project$Util$deMaybeM('deMaybe - nothing there');
var _user$project$Util$hdExn = function (l) {
	return _user$project$Util$deMaybe(
		_elm_lang$core$List$head(l));
};
var _user$project$Util$htmlSize = function (str) {
	var size = _user$project$Native_Size.size(str);
	return {ctor: '_Tuple2', _0: size.width, _1: size.height};
};
var _user$project$Util$random = function (a) {
	return _user$project$Native_Random.random(a);
};
var _user$project$Util$windowSize = function (a) {
	var size = _user$project$Native_Window.size(a);
	return {ctor: '_Tuple2', _0: size.width, _1: size.height - 45};
};
var _user$project$Util$timestamp = function (a) {
	return _user$project$Native_Timestamp.timestamp(a);
};

var _user$project$Types$holeOrID = function (a) {
	var _p0 = a;
	if (_p0.ctor === 'Empty') {
		return _p0._0;
	} else {
		return _p0._0;
	}
};
var _user$project$Types$unwrapState = function (s) {
	var _p1 = s;
	if (_p1.ctor === 'Dragging') {
		return _p1._3;
	} else {
		return s;
	}
};
var _user$project$Types$deID = function (_p2) {
	var _p3 = _p2;
	return _p3._0;
};
var _user$project$Types$Exception = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {$short: a, $long: b, tipe: c, actual: d, actualType: e, result: f, resultType: g, expected: h, info: i, workarounds: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _user$project$Types$Pos = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _user$project$Types$VPos = F2(
	function (a, b) {
		return {vx: a, vy: b};
	});
var _user$project$Types$MouseEvent = F2(
	function (a, b) {
		return {pos: a, button: b};
	});
var _user$project$Types$LiveValue = F4(
	function (a, b, c, d) {
		return {value: a, tipe: b, json: c, exc: d};
	});
var _user$project$Types$Autocomplete = F9(
	function (a, b, c, d, e, f, g, h, i) {
		return {functions: a, varnames: b, completions: c, index: d, value: e, open: f, showFunctions: g, liveValue: h, tipe: i};
	});
var _user$project$Types$HandlerSpec = F3(
	function (a, b, c) {
		return {module_: a, name: b, modifier: c};
	});
var _user$project$Types$Handler = F2(
	function (a, b) {
		return {ast: a, spec: b};
	});
var _user$project$Types$DB = F2(
	function (a, b) {
		return {name: a, cols: b};
	});
var _user$project$Types$Toplevel = F3(
	function (a, b, c) {
		return {id: a, pos: b, data: c};
	});
var _user$project$Types$TLAResult = F4(
	function (a, b, c, d) {
		return {id: a, astValue: b, liveValues: c, availableVarnames: d};
	});
var _user$project$Types$Model = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {center: a, error: b, lastMsg: c, lastMod: d, tests: e, complete: f, state: g, toplevels: h, analysis: i, integrationTestState: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _user$project$Types$Parameter = F5(
	function (a, b, c, d, e) {
		return {name: a, tipe: b, block_args: c, optional: d, description: e};
	});
var _user$project$Types$Function = F4(
	function (a, b, c, d) {
		return {name: a, parameters: b, description: c, returnTipe: d};
	});
var _user$project$Types$FlagParameter = F5(
	function (a, b, c, d, e) {
		return {name: a, tipe: b, block_args: c, optional: d, description: e};
	});
var _user$project$Types$FlagFunction = F4(
	function (a, b, c, d) {
		return {name: a, parameters: b, description: c, return_type: d};
	});
var _user$project$Types$Flags = F2(
	function (a, b) {
		return {editorState: a, complete: b};
	});
var _user$project$Types$Editor = {};
var _user$project$Types$TDB = {ctor: 'TDB'};
var _user$project$Types$TResp = {ctor: 'TResp'};
var _user$project$Types$TIncomplete = {ctor: 'TIncomplete'};
var _user$project$Types$TNull = {ctor: 'TNull'};
var _user$project$Types$TBlock = {ctor: 'TBlock'};
var _user$project$Types$TAny = {ctor: 'TAny'};
var _user$project$Types$TList = {ctor: 'TList'};
var _user$project$Types$TObj = {ctor: 'TObj'};
var _user$project$Types$TFloat = {ctor: 'TFloat'};
var _user$project$Types$TBool = {ctor: 'TBool'};
var _user$project$Types$TChar = {ctor: 'TChar'};
var _user$project$Types$TStr = {ctor: 'TStr'};
var _user$project$Types$TInt = {ctor: 'TInt'};
var _user$project$Types$TLID = function (a) {
	return {ctor: 'TLID', _0: a};
};
var _user$project$Types$ID = function (a) {
	return {ctor: 'ID', _0: a};
};
var _user$project$Types$gid = function (unit) {
	return _user$project$Types$ID(
		_user$project$Util$random(unit));
};
var _user$project$Types$Filling = F2(
	function (a, b) {
		return {ctor: 'Filling', _0: a, _1: b};
	});
var _user$project$Types$Creating = function (a) {
	return {ctor: 'Creating', _0: a};
};
var _user$project$Types$Deselected = {ctor: 'Deselected'};
var _user$project$Types$Dragging = F4(
	function (a, b, c, d) {
		return {ctor: 'Dragging', _0: a, _1: b, _2: c, _3: d};
	});
var _user$project$Types$Entering = function (a) {
	return {ctor: 'Entering', _0: a};
};
var _user$project$Types$Selecting = F2(
	function (a, b) {
		return {ctor: 'Selecting', _0: a, _1: b};
	});
var _user$project$Types$Initialization = {ctor: 'Initialization'};
var _user$project$Types$SaveTestButton = {ctor: 'SaveTestButton'};
var _user$project$Types$ClearGraph = {ctor: 'ClearGraph'};
var _user$project$Types$FinishIntegrationTest = {ctor: 'FinishIntegrationTest'};
var _user$project$Types$AddRandom = {ctor: 'AddRandom'};
var _user$project$Types$LocationChange = function (a) {
	return {ctor: 'LocationChange', _0: a};
};
var _user$project$Types$SaveTestCallBack = function (a) {
	return {ctor: 'SaveTestCallBack', _0: a};
};
var _user$project$Types$RPCCallBack = F4(
	function (a, b, c, d) {
		return {ctor: 'RPCCallBack', _0: a, _1: b, _2: c, _3: d};
	});
var _user$project$Types$FocusAutocompleteItem = function (a) {
	return {ctor: 'FocusAutocompleteItem', _0: a};
};
var _user$project$Types$FocusEntry = function (a) {
	return {ctor: 'FocusEntry', _0: a};
};
var _user$project$Types$GlobalKeyPress = function (a) {
	return {ctor: 'GlobalKeyPress', _0: a};
};
var _user$project$Types$EntrySubmitMsg = {ctor: 'EntrySubmitMsg'};
var _user$project$Types$EntryInputMsg = function (a) {
	return {ctor: 'EntryInputMsg', _0: a};
};
var _user$project$Types$DragToplevel = F2(
	function (a, b) {
		return {ctor: 'DragToplevel', _0: a, _1: b};
	});
var _user$project$Types$ToplevelClickUp = F2(
	function (a, b) {
		return {ctor: 'ToplevelClickUp', _0: a, _1: b};
	});
var _user$project$Types$ToplevelClickDown = F2(
	function (a, b) {
		return {ctor: 'ToplevelClickDown', _0: a, _1: b};
	});
var _user$project$Types$GlobalClick = function (a) {
	return {ctor: 'GlobalClick', _0: a};
};
var _user$project$Types$FocusSame = {ctor: 'FocusSame'};
var _user$project$Types$FocusNext = F2(
	function (a, b) {
		return {ctor: 'FocusNext', _0: a, _1: b};
	});
var _user$project$Types$FocusExact = F2(
	function (a, b) {
		return {ctor: 'FocusExact', _0: a, _1: b};
	});
var _user$project$Types$Refocus = function (a) {
	return {ctor: 'Refocus', _0: a};
};
var _user$project$Types$FocusNothing = {ctor: 'FocusNothing'};
var _user$project$Types$Redo = {ctor: 'Redo'};
var _user$project$Types$Undo = {ctor: 'Undo'};
var _user$project$Types$Savepoint = {ctor: 'Savepoint'};
var _user$project$Types$DeleteAll = {ctor: 'DeleteAll'};
var _user$project$Types$MoveTL = F2(
	function (a, b) {
		return {ctor: 'MoveTL', _0: a, _1: b};
	});
var _user$project$Types$DeleteTL = function (a) {
	return {ctor: 'DeleteTL', _0: a};
};
var _user$project$Types$SetDBColType = F3(
	function (a, b, c) {
		return {ctor: 'SetDBColType', _0: a, _1: b, _2: c};
	});
var _user$project$Types$SetDBColName = F3(
	function (a, b, c) {
		return {ctor: 'SetDBColName', _0: a, _1: b, _2: c};
	});
var _user$project$Types$AddDBCol = F3(
	function (a, b, c) {
		return {ctor: 'AddDBCol', _0: a, _1: b, _2: c};
	});
var _user$project$Types$CreateDB = F3(
	function (a, b, c) {
		return {ctor: 'CreateDB', _0: a, _1: b, _2: c};
	});
var _user$project$Types$SetHandler = F3(
	function (a, b, c) {
		return {ctor: 'SetHandler', _0: a, _1: b, _2: c};
	});
var _user$project$Types$NoOp = {ctor: 'NoOp'};
var _user$project$Types$ACVariable = function (a) {
	return {ctor: 'ACVariable', _0: a};
};
var _user$project$Types$ACField = function (a) {
	return {ctor: 'ACField', _0: a};
};
var _user$project$Types$ACFunction = function (a) {
	return {ctor: 'ACFunction', _0: a};
};
var _user$project$Types$StubVariant = {ctor: 'StubVariant'};
var _user$project$Types$Nested = F2(
	function (a, b) {
		return {ctor: 'Nested', _0: a, _1: b};
	});
var _user$project$Types$Leaf = function (a) {
	return {ctor: 'Leaf', _0: a};
};
var _user$project$Types$FieldAccess = F3(
	function (a, b, c) {
		return {ctor: 'FieldAccess', _0: a, _1: b, _2: c};
	});
var _user$project$Types$Thread = F2(
	function (a, b) {
		return {ctor: 'Thread', _0: a, _1: b};
	});
var _user$project$Types$Hole = function (a) {
	return {ctor: 'Hole', _0: a};
};
var _user$project$Types$Value = F2(
	function (a, b) {
		return {ctor: 'Value', _0: a, _1: b};
	});
var _user$project$Types$Lambda = F3(
	function (a, b, c) {
		return {ctor: 'Lambda', _0: a, _1: b, _2: c};
	});
var _user$project$Types$Let = F4(
	function (a, b, c, d) {
		return {ctor: 'Let', _0: a, _1: b, _2: c, _3: d};
	});
var _user$project$Types$Variable = F2(
	function (a, b) {
		return {ctor: 'Variable', _0: a, _1: b};
	});
var _user$project$Types$FnCall = F3(
	function (a, b, c) {
		return {ctor: 'FnCall', _0: a, _1: b, _2: c};
	});
var _user$project$Types$If = F4(
	function (a, b, c, d) {
		return {ctor: 'If', _0: a, _1: b, _2: c, _3: d};
	});
var _user$project$Types$NotAHole = {ctor: 'NotAHole'};
var _user$project$Types$DBColTypeHole = function (a) {
	return {ctor: 'DBColTypeHole', _0: a};
};
var _user$project$Types$DBColNameHole = function (a) {
	return {ctor: 'DBColNameHole', _0: a};
};
var _user$project$Types$FieldHole = function (a) {
	return {ctor: 'FieldHole', _0: a};
};
var _user$project$Types$ExprHole = function (a) {
	return {ctor: 'ExprHole', _0: a};
};
var _user$project$Types$SpecHole = function (a) {
	return {ctor: 'SpecHole', _0: a};
};
var _user$project$Types$BindHole = function (a) {
	return {ctor: 'BindHole', _0: a};
};
var _user$project$Types$Full = F2(
	function (a, b) {
		return {ctor: 'Full', _0: a, _1: b};
	});
var _user$project$Types$Empty = function (a) {
	return {ctor: 'Empty', _0: a};
};
var _user$project$Types$TLDB = function (a) {
	return {ctor: 'TLDB', _0: a};
};
var _user$project$Types$TLHandler = function (a) {
	return {ctor: 'TLHandler', _0: a};
};
var _user$project$Types$NoIntegrationTest = {ctor: 'NoIntegrationTest'};
var _user$project$Types$IntegrationTestFinished = function (a) {
	return {ctor: 'IntegrationTestFinished', _0: a};
};
var _user$project$Types$IntegrationTestExpectation = function (a) {
	return {ctor: 'IntegrationTestExpectation', _0: a};
};
var _user$project$Types$ACShowFunctions = function (a) {
	return {ctor: 'ACShowFunctions', _0: a};
};
var _user$project$Types$ACSetAvailableVarnames = function (a) {
	return {ctor: 'ACSetAvailableVarnames', _0: a};
};
var _user$project$Types$ACFilterByLiveValue = function (a) {
	return {ctor: 'ACFilterByLiveValue', _0: a};
};
var _user$project$Types$ACSelectUp = {ctor: 'ACSelectUp'};
var _user$project$Types$ACSelectDown = {ctor: 'ACSelectDown'};
var _user$project$Types$ACClear = {ctor: 'ACClear'};
var _user$project$Types$ACReset = {ctor: 'ACReset'};
var _user$project$Types$ACOpen = function (a) {
	return {ctor: 'ACOpen', _0: a};
};
var _user$project$Types$ACAppendQuery = function (a) {
	return {ctor: 'ACAppendQuery', _0: a};
};
var _user$project$Types$ACSetQuery = function (a) {
	return {ctor: 'ACSetQuery', _0: a};
};
var _user$project$Types$SetState = function (a) {
	return {ctor: 'SetState', _0: a};
};
var _user$project$Types$EndIntegrationTest = {ctor: 'EndIntegrationTest'};
var _user$project$Types$TriggerIntegrationTest = function (a) {
	return {ctor: 'TriggerIntegrationTest', _0: a};
};
var _user$project$Types$Drag = F4(
	function (a, b, c, d) {
		return {ctor: 'Drag', _0: a, _1: b, _2: c, _3: d};
	});
var _user$project$Types$Many = function (a) {
	return {ctor: 'Many', _0: a};
};
var _user$project$Types$AutocompleteMod = function (a) {
	return {ctor: 'AutocompleteMod', _0: a};
};
var _user$project$Types$MakeCmd = function (a) {
	return {ctor: 'MakeCmd', _0: a};
};
var _user$project$Types$NoChange = {ctor: 'NoChange'};
var _user$project$Types$SetCenter = function (a) {
	return {ctor: 'SetCenter', _0: a};
};
var _user$project$Types$RPC = function (a) {
	return {ctor: 'RPC', _0: a};
};
var _user$project$Types$Enter = function (a) {
	return {ctor: 'Enter', _0: a};
};
var _user$project$Types$SetToplevels = F2(
	function (a, b) {
		return {ctor: 'SetToplevels', _0: a, _1: b};
	});
var _user$project$Types$Deselect = {ctor: 'Deselect'};
var _user$project$Types$Select = F2(
	function (a, b) {
		return {ctor: 'Select', _0: a, _1: b};
	});
var _user$project$Types$ClearError = {ctor: 'ClearError'};
var _user$project$Types$Error = function (a) {
	return {ctor: 'Error', _0: a};
};

var _user$project$AST$toContent = function (a) {
	var _p0 = a;
	switch (_p0.ctor) {
		case 'Value':
			return _p0._1;
		case 'Variable':
			return _p0._1;
		default:
			return '';
	}
};
var _user$project$AST$listHoles = function (expr) {
	listHoles:
	while (true) {
		var lhList = function (exprs) {
			return _elm_lang$core$List$concat(
				A2(_elm_lang$core$List$map, _user$project$AST$listHoles, exprs));
		};
		var _p1 = expr;
		switch (_p1.ctor) {
			case 'Value':
				return {ctor: '[]'};
			case 'Let':
				var bindHoles = function () {
					var _p2 = _p1._1;
					if (_p2.ctor === 'Full') {
						return {ctor: '[]'};
					} else {
						return {
							ctor: '::',
							_0: _p2._0,
							_1: {ctor: '[]'}
						};
					}
				}();
				var exprHoles = lhList(
					{
						ctor: '::',
						_0: _p1._2,
						_1: {
							ctor: '::',
							_0: _p1._3,
							_1: {ctor: '[]'}
						}
					});
				return A2(_elm_lang$core$Basics_ops['++'], bindHoles, exprHoles);
			case 'If':
				return lhList(
					{
						ctor: '::',
						_0: _p1._1,
						_1: {
							ctor: '::',
							_0: _p1._2,
							_1: {
								ctor: '::',
								_0: _p1._3,
								_1: {ctor: '[]'}
							}
						}
					});
			case 'Variable':
				return {ctor: '[]'};
			case 'FnCall':
				return lhList(_p1._2);
			case 'Lambda':
				var _v3 = _p1._2;
				expr = _v3;
				continue listHoles;
			case 'Hole':
				return {
					ctor: '::',
					_0: _p1._0,
					_1: {ctor: '[]'}
				};
			case 'Thread':
				return lhList(_p1._1);
			default:
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$AST$listHoles(_p1._1),
					{
						ctor: '::',
						_0: _user$project$Types$holeOrID(_p1._2),
						_1: {ctor: '[]'}
					});
		}
	}
};
var _user$project$AST$listFieldHoles = function (expr) {
	listFieldHoles:
	while (true) {
		var lfhList = function (exprs) {
			return _elm_lang$core$List$concat(
				A2(_elm_lang$core$List$map, _user$project$AST$listFieldHoles, exprs));
		};
		var _p3 = expr;
		switch (_p3.ctor) {
			case 'Value':
				return {ctor: '[]'};
			case 'Let':
				return lfhList(
					{
						ctor: '::',
						_0: _p3._2,
						_1: {
							ctor: '::',
							_0: _p3._3,
							_1: {ctor: '[]'}
						}
					});
			case 'If':
				return lfhList(
					{
						ctor: '::',
						_0: _p3._1,
						_1: {
							ctor: '::',
							_0: _p3._2,
							_1: {
								ctor: '::',
								_0: _p3._3,
								_1: {ctor: '[]'}
							}
						}
					});
			case 'Variable':
				return {ctor: '[]'};
			case 'FnCall':
				return lfhList(_p3._2);
			case 'Lambda':
				var _v5 = _p3._2;
				expr = _v5;
				continue listFieldHoles;
			case 'Hole':
				return {ctor: '[]'};
			case 'Thread':
				return lfhList(_p3._1);
			default:
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$AST$listFieldHoles(_p3._1),
					{
						ctor: '::',
						_0: _user$project$Types$holeOrID(_p3._2),
						_1: {ctor: '[]'}
					});
		}
	}
};
var _user$project$AST$listBindHoles = function (expr) {
	listBindHoles:
	while (true) {
		var lbhList = function (exprs) {
			return _elm_lang$core$List$concat(
				A2(_elm_lang$core$List$map, _user$project$AST$listBindHoles, exprs));
		};
		var _p4 = expr;
		switch (_p4.ctor) {
			case 'Value':
				return {ctor: '[]'};
			case 'Let':
				var bindHoles = function () {
					var _p5 = _p4._1;
					if (_p5.ctor === 'Full') {
						return {ctor: '[]'};
					} else {
						return {
							ctor: '::',
							_0: _p5._0,
							_1: {ctor: '[]'}
						};
					}
				}();
				var exprBindHoles = lbhList(
					{
						ctor: '::',
						_0: _p4._2,
						_1: {
							ctor: '::',
							_0: _p4._3,
							_1: {ctor: '[]'}
						}
					});
				return A2(_elm_lang$core$Basics_ops['++'], bindHoles, exprBindHoles);
			case 'If':
				return lbhList(
					{
						ctor: '::',
						_0: _p4._1,
						_1: {
							ctor: '::',
							_0: _p4._2,
							_1: {
								ctor: '::',
								_0: _p4._3,
								_1: {ctor: '[]'}
							}
						}
					});
			case 'Variable':
				return {ctor: '[]'};
			case 'FnCall':
				return lbhList(_p4._2);
			case 'Lambda':
				var _v8 = _p4._2;
				expr = _v8;
				continue listBindHoles;
			case 'Hole':
				return {ctor: '[]'};
			case 'Thread':
				return lbhList(_p4._1);
			default:
				var _v9 = _p4._1;
				expr = _v9;
				continue listBindHoles;
		}
	}
};
var _user$project$AST$isHole = function (e) {
	var _p6 = e;
	if (_p6.ctor === 'Hole') {
		return true;
	} else {
		return false;
	}
};
var _user$project$AST$replaceFieldHole = F3(
	function (hid, replacement, expr) {
		var rfh = A2(_user$project$AST$replaceFieldHole, hid, replacement);
		var rfhList = function (exprs) {
			return A2(_elm_lang$core$List$map, rfh, exprs);
		};
		var _p7 = expr;
		switch (_p7.ctor) {
			case 'Value':
				return A2(_user$project$Types$Value, _p7._0, _p7._1);
			case 'Let':
				return A4(
					_user$project$Types$Let,
					_p7._0,
					_p7._1,
					rfh(_p7._2),
					rfh(_p7._3));
			case 'If':
				return A4(
					_user$project$Types$If,
					_p7._0,
					rfh(_p7._1),
					rfh(_p7._2),
					rfh(_p7._3));
			case 'Variable':
				return A2(_user$project$Types$Variable, _p7._0, _p7._1);
			case 'FnCall':
				return A3(
					_user$project$Types$FnCall,
					_p7._0,
					_p7._1,
					rfhList(_p7._2));
			case 'Lambda':
				return A3(
					_user$project$Types$Lambda,
					_p7._0,
					_p7._1,
					rfh(_p7._2));
			case 'Hole':
				return _user$project$Types$Hole(_p7._0);
			case 'Thread':
				return A2(
					_user$project$Types$Thread,
					_p7._0,
					rfhList(_p7._1));
			default:
				var _p11 = _p7._2;
				var newName = function () {
					var _p8 = _p11;
					if (_p8.ctor === 'Full') {
						var _p9 = _p8._0;
						return _elm_lang$core$Native_Utils.eq(_p9, hid) ? A2(_user$project$Types$Full, _p9, replacement) : _p11;
					} else {
						var _p10 = _p8._0;
						return _elm_lang$core$Native_Utils.eq(_p10, hid) ? A2(_user$project$Types$Full, _p10, replacement) : _p11;
					}
				}();
				return A3(
					_user$project$Types$FieldAccess,
					_p7._0,
					rfh(_p7._1),
					newName);
		}
	});
var _user$project$AST$replaceBindHole_ = F3(
	function (hid, replacement, expr) {
		var rbh = A2(_user$project$AST$replaceBindHole_, hid, replacement);
		var rbhList = function (exprs) {
			return A2(_elm_lang$core$List$map, rbh, exprs);
		};
		var _p12 = expr;
		switch (_p12.ctor) {
			case 'Value':
				return A2(_user$project$Types$Value, _p12._0, _p12._1);
			case 'Let':
				var _p16 = _p12._1;
				var newLhs = function () {
					var _p13 = _p16;
					if (_p13.ctor === 'Full') {
						var _p14 = _p13._0;
						return _elm_lang$core$Native_Utils.eq(_p14, hid) ? A2(_user$project$Types$Full, _p14, replacement) : _p16;
					} else {
						var _p15 = _p13._0;
						return _elm_lang$core$Native_Utils.eq(_p15, hid) ? A2(_user$project$Types$Full, _p15, replacement) : _p16;
					}
				}();
				return A4(
					_user$project$Types$Let,
					_p12._0,
					newLhs,
					rbh(_p12._2),
					rbh(_p12._3));
			case 'If':
				return A4(
					_user$project$Types$If,
					_p12._0,
					rbh(_p12._1),
					rbh(_p12._2),
					rbh(_p12._3));
			case 'Variable':
				return A2(_user$project$Types$Variable, _p12._0, _p12._1);
			case 'FnCall':
				return A3(
					_user$project$Types$FnCall,
					_p12._0,
					_p12._1,
					rbhList(_p12._2));
			case 'Lambda':
				return A3(
					_user$project$Types$Lambda,
					_p12._0,
					_p12._1,
					rbh(_p12._2));
			case 'Hole':
				return _user$project$Types$Hole(_p12._0);
			case 'Thread':
				return A2(
					_user$project$Types$Thread,
					_p12._0,
					rbhList(_p12._1));
			default:
				return A3(
					_user$project$Types$FieldAccess,
					_p12._0,
					rbh(_p12._1),
					_p12._2);
		}
	});
var _user$project$AST$replaceBindHole = F3(
	function (hid, replacement, ast) {
		return A3(_user$project$AST$replaceBindHole_, hid, replacement, ast);
	});
var _user$project$AST$closeThread = function (expr) {
	closeThread:
	while (true) {
		var ct = _user$project$AST$closeThread;
		var ctList = _elm_lang$core$List$map(ct);
		var _p17 = expr;
		switch (_p17.ctor) {
			case 'Value':
				return expr;
			case 'Hole':
				return expr;
			case 'Variable':
				return expr;
			case 'Let':
				return A4(
					_user$project$Types$Let,
					_p17._0,
					_p17._1,
					ct(_p17._2),
					ct(_p17._3));
			case 'If':
				return A4(
					_user$project$Types$If,
					_p17._0,
					ct(_p17._1),
					ct(_p17._2),
					ct(_p17._3));
			case 'FnCall':
				return A3(
					_user$project$Types$FnCall,
					_p17._0,
					_p17._1,
					ctList(_p17._2));
			case 'Lambda':
				return A3(
					_user$project$Types$Lambda,
					_p17._0,
					_p17._1,
					ct(_p17._2));
			case 'FieldAccess':
				return A3(
					_user$project$Types$FieldAccess,
					_p17._0,
					ct(_p17._1),
					_p17._2);
			default:
				var _p19 = _p17._0;
				var newExprs = ctList(_p17._1);
				var init = A2(
					_elm_lang$core$Maybe$withDefault,
					{ctor: '[]'},
					_elm_community$list_extra$List_Extra$init(newExprs));
				var last = _user$project$Util$deMaybe(
					_elm_community$list_extra$List_Extra$last(newExprs));
				var _p18 = {ctor: '_Tuple2', _0: init, _1: last};
				if (_p18._0.ctor === '[]') {
					return last;
				} else {
					if (_p18._1.ctor === 'Hole') {
						if (_p18._0._1.ctor === '[]') {
							return _p18._0._0;
						} else {
							var _v17 = A2(_user$project$Types$Thread, _p19, _p18._0);
							expr = _v17;
							continue closeThread;
						}
					} else {
						return A2(_user$project$Types$Thread, _p19, newExprs);
					}
				}
		}
	}
};
var _user$project$AST$toID = function (expr) {
	var _p20 = expr;
	switch (_p20.ctor) {
		case 'Value':
			return _p20._0;
		case 'Let':
			return _p20._0;
		case 'If':
			return _p20._0;
		case 'Variable':
			return _p20._0;
		case 'FnCall':
			return _p20._0;
		case 'Lambda':
			return _p20._0;
		case 'Hole':
			return _p20._0;
		case 'Thread':
			return _p20._0;
		default:
			return _p20._0;
	}
};
var _user$project$AST$listThreadHoles = function (expr) {
	listThreadHoles:
	while (true) {
		var lthList = function (exprs) {
			return _elm_lang$core$List$concat(
				A2(_elm_lang$core$List$map, _user$project$AST$listThreadHoles, exprs));
		};
		var _p21 = expr;
		switch (_p21.ctor) {
			case 'Value':
				return {ctor: '[]'};
			case 'Let':
				return lthList(
					{
						ctor: '::',
						_0: _p21._2,
						_1: {
							ctor: '::',
							_0: _p21._3,
							_1: {ctor: '[]'}
						}
					});
			case 'If':
				return lthList(
					{
						ctor: '::',
						_0: _p21._1,
						_1: {
							ctor: '::',
							_0: _p21._2,
							_1: {
								ctor: '::',
								_0: _p21._3,
								_1: {ctor: '[]'}
							}
						}
					});
			case 'Variable':
				return {ctor: '[]'};
			case 'FnCall':
				return lthList(_p21._2);
			case 'Lambda':
				var _v20 = _p21._2;
				expr = _v20;
				continue listThreadHoles;
			case 'Hole':
				return {ctor: '[]'};
			case 'Thread':
				var _p22 = A2(_elm_lang$core$List$partition, _user$project$AST$isHole, _p21._1);
				var holes = _p22._0;
				var notHoles = _p22._1;
				var holeids = A2(_elm_lang$core$List$map, _user$project$AST$toID, holes);
				var subExprsHoleids = lthList(notHoles);
				return A2(_elm_lang$core$Basics_ops['++'], holeids, subExprsHoleids);
			default:
				var _v21 = _p21._1;
				expr = _v21;
				continue listThreadHoles;
		}
	}
};
var _user$project$AST$wrapInThread = F2(
	function (id, expr) {
		var wrap = function (e) {
			var _p23 = e;
			if (_p23.ctor === 'Thread') {
				return e;
			} else {
				return A2(
					_user$project$Types$Thread,
					_user$project$Types$gid(
						{ctor: '_Tuple0'}),
					{
						ctor: '::',
						_0: e,
						_1: {
							ctor: '::',
							_0: _user$project$Types$Hole(
								_user$project$Types$gid(
									{ctor: '_Tuple0'})),
							_1: {ctor: '[]'}
						}
					});
			}
		};
		var wt = function (e) {
			return A2(_user$project$AST$wrapInThread, id, e);
		};
		var nested = function () {
			var _p24 = expr;
			switch (_p24.ctor) {
				case 'Value':
					return expr;
				case 'Hole':
					return expr;
				case 'Variable':
					return expr;
				case 'Let':
					return A4(
						_user$project$Types$Let,
						_p24._0,
						_p24._1,
						wt(_p24._2),
						wt(_p24._3));
				case 'If':
					return A4(
						_user$project$Types$If,
						_p24._0,
						wt(_p24._1),
						wt(_p24._2),
						wt(_p24._3));
				case 'FnCall':
					return A3(
						_user$project$Types$FnCall,
						_p24._0,
						_p24._1,
						A2(_elm_lang$core$List$map, wt, _p24._2));
				case 'Lambda':
					return A3(
						_user$project$Types$Lambda,
						_p24._0,
						_p24._1,
						wt(_p24._2));
				case 'Thread':
					return A2(
						_user$project$Types$Thread,
						_p24._0,
						A2(_elm_lang$core$List$map, wt, _p24._1));
				default:
					return A3(
						_user$project$Types$FieldAccess,
						_p24._0,
						wt(_p24._1),
						_p24._2);
			}
		}();
		return _elm_lang$core$Native_Utils.eq(
			_user$project$AST$toID(expr),
			id) ? wrap(expr) : nested;
	});
var _user$project$AST$maybeExtendThreadAt = F2(
	function (id, expr) {
		var et = function (e) {
			return A2(_user$project$AST$maybeExtendThreadAt, id, e);
		};
		var _p25 = expr;
		switch (_p25.ctor) {
			case 'Value':
				return expr;
			case 'Hole':
				return expr;
			case 'Variable':
				return expr;
			case 'Let':
				return A4(
					_user$project$Types$Let,
					_p25._0,
					_p25._1,
					et(_p25._2),
					et(_p25._3));
			case 'If':
				return A4(
					_user$project$Types$If,
					_p25._0,
					et(_p25._1),
					et(_p25._2),
					et(_p25._3));
			case 'FnCall':
				return A3(
					_user$project$Types$FnCall,
					_p25._0,
					_p25._1,
					A2(_elm_lang$core$List$map, et, _p25._2));
			case 'Lambda':
				return A3(
					_user$project$Types$Lambda,
					_p25._0,
					_p25._1,
					et(_p25._2));
			case 'Thread':
				var newExprs = A3(
					_elm_lang$core$List$foldr,
					F2(
						function (e, list) {
							return _elm_lang$core$Native_Utils.eq(
								_user$project$AST$toID(e),
								id) ? {
								ctor: '::',
								_0: e,
								_1: {
									ctor: '::',
									_0: _user$project$Types$Hole(
										_user$project$Types$gid(
											{ctor: '_Tuple0'})),
									_1: list
								}
							} : {ctor: '::', _0: e, _1: list};
						}),
					{ctor: '[]'},
					_p25._1);
				return A2(
					_user$project$Types$Thread,
					_p25._0,
					A2(_elm_lang$core$List$map, et, newExprs));
			default:
				return A3(
					_user$project$Types$FieldAccess,
					_p25._0,
					et(_p25._1),
					_p25._2);
		}
	});
var _user$project$AST$children = function (e) {
	var _p26 = e;
	switch (_p26.ctor) {
		case 'Value':
			return {ctor: '[]'};
		case 'Hole':
			return {ctor: '[]'};
		case 'Variable':
			return {ctor: '[]'};
		case 'If':
			return {
				ctor: '::',
				_0: _user$project$AST$toID(_p26._1),
				_1: {
					ctor: '::',
					_0: _user$project$AST$toID(_p26._2),
					_1: {
						ctor: '::',
						_0: _user$project$AST$toID(_p26._3),
						_1: {ctor: '[]'}
					}
				}
			};
		case 'FnCall':
			return A2(_elm_lang$core$List$map, _user$project$AST$toID, _p26._2);
		case 'Lambda':
			return {
				ctor: '::',
				_0: _user$project$AST$toID(_p26._2),
				_1: {ctor: '[]'}
			};
		case 'Thread':
			return A2(_elm_lang$core$List$map, _user$project$AST$toID, _p26._1);
		case 'FieldAccess':
			return {
				ctor: '::',
				_0: _user$project$AST$toID(_p26._1),
				_1: {
					ctor: '::',
					_0: _user$project$Types$holeOrID(_p26._2),
					_1: {ctor: '[]'}
				}
			};
		default:
			return {
				ctor: '::',
				_0: _user$project$Types$holeOrID(_p26._1),
				_1: {
					ctor: '::',
					_0: _user$project$AST$toID(_p26._2),
					_1: {
						ctor: '::',
						_0: _user$project$AST$toID(_p26._3),
						_1: {ctor: '[]'}
					}
				}
			};
	}
};
var _user$project$AST$parentOf_ = F2(
	function (eid, expr) {
		var filterMaybe = function (xs) {
			return _elm_lang$core$List$head(
				A2(_elm_lang$core$List$filterMap, _elm_lang$core$Basics$identity, xs));
		};
		var returnOr = F2(
			function (fn, e) {
				return A2(
					_elm_lang$core$List$member,
					eid,
					_user$project$AST$children(e)) ? _elm_lang$core$Maybe$Just(e) : fn(e);
			});
		var po = _user$project$AST$parentOf_(eid);
		var _p27 = expr;
		switch (_p27.ctor) {
			case 'Value':
				return _elm_lang$core$Maybe$Nothing;
			case 'Hole':
				return _elm_lang$core$Maybe$Nothing;
			case 'Variable':
				return _elm_lang$core$Maybe$Nothing;
			case 'Let':
				return A2(
					returnOr,
					function (_p28) {
						return filterMaybe(
							{
								ctor: '::',
								_0: po(_p27._3),
								_1: {
									ctor: '::',
									_0: po(_p27._2),
									_1: {ctor: '[]'}
								}
							});
					},
					expr);
			case 'If':
				return A2(
					returnOr,
					function (_p29) {
						var eb = po(_p27._3);
						var ib = po(_p27._2);
						var c = po(_p27._1);
						return filterMaybe(
							{
								ctor: '::',
								_0: c,
								_1: {
									ctor: '::',
									_0: ib,
									_1: {
										ctor: '::',
										_0: eb,
										_1: {ctor: '[]'}
									}
								}
							});
					},
					expr);
			case 'FnCall':
				return A2(
					returnOr,
					function (_p30) {
						return filterMaybe(
							A2(_elm_lang$core$List$map, po, _p27._2));
					},
					expr);
			case 'Lambda':
				return A2(
					returnOr,
					function (_p31) {
						return po(_p27._2);
					},
					expr);
			case 'Thread':
				return A2(
					returnOr,
					function (_p32) {
						return filterMaybe(
							A2(_elm_lang$core$List$map, po, _p27._1));
					},
					expr);
			default:
				return _elm_lang$core$Native_Utils.eq(
					_user$project$Types$holeOrID(_p27._2),
					eid) ? _elm_lang$core$Maybe$Just(expr) : A2(
					returnOr,
					function (_p33) {
						return po(_p27._1);
					},
					expr);
		}
	});
var _user$project$AST$parentOf = F2(
	function (id, ast) {
		return _user$project$Util$deMaybe(
			A2(_user$project$AST$parentOf_, id, ast));
	});
var _user$project$AST$childrenOf = F2(
	function (pid, expr) {
		var returnOr = F2(
			function (fn, e) {
				return _elm_lang$core$Native_Utils.eq(
					pid,
					_user$project$AST$toID(e)) ? _user$project$AST$children(e) : fn(e);
			});
		var co = _user$project$AST$childrenOf(pid);
		var _p34 = expr;
		switch (_p34.ctor) {
			case 'Value':
				return {ctor: '[]'};
			case 'Hole':
				return {ctor: '[]'};
			case 'Variable':
				return {ctor: '[]'};
			case 'Let':
				return A2(
					returnOr,
					function (_p35) {
						return _elm_lang$core$List$concat(
							{
								ctor: '::',
								_0: co(_p34._3),
								_1: {
									ctor: '::',
									_0: co(_p34._2),
									_1: {ctor: '[]'}
								}
							});
					},
					expr);
			case 'If':
				return A2(
					returnOr,
					function (_p36) {
						var eb = co(_p34._3);
						var ib = co(_p34._2);
						var c = co(_p34._1);
						return _elm_lang$core$List$concat(
							{
								ctor: '::',
								_0: c,
								_1: {
									ctor: '::',
									_0: ib,
									_1: {
										ctor: '::',
										_0: eb,
										_1: {ctor: '[]'}
									}
								}
							});
					},
					expr);
			case 'FnCall':
				return A2(
					returnOr,
					function (_p37) {
						return _elm_lang$core$List$concat(
							A2(_elm_lang$core$List$map, co, _p34._2));
					},
					expr);
			case 'Lambda':
				return A2(
					returnOr,
					function (_p38) {
						return co(_p34._2);
					},
					expr);
			case 'Thread':
				return A2(
					returnOr,
					function (_p39) {
						return _elm_lang$core$List$concat(
							A2(_elm_lang$core$List$map, co, _p34._1));
					},
					expr);
			default:
				return A2(
					returnOr,
					function (_p40) {
						return co(_p34._1);
					},
					expr);
		}
	});
var _user$project$AST$ancestorsWhere = F3(
	function (id, expr, fn) {
		if (_elm_lang$core$Native_Utils.eq(
			_user$project$AST$toID(expr),
			id)) {
			return {ctor: '[]'};
		} else {
			var r = function (e) {
				return A3(_user$project$AST$ancestorsWhere, id, e, fn);
			};
			var rlist = function (es) {
				return _elm_lang$core$List$concat(
					A2(_elm_lang$core$List$map, r, es));
			};
			var nested = function () {
				var _p41 = expr;
				switch (_p41.ctor) {
					case 'Value':
						return {ctor: '[]'};
					case 'Hole':
						return {ctor: '[]'};
					case 'Variable':
						return {ctor: '[]'};
					case 'Let':
						return rlist(
							{
								ctor: '::',
								_0: _p41._2,
								_1: {
									ctor: '::',
									_0: _p41._3,
									_1: {ctor: '[]'}
								}
							});
					case 'If':
						return rlist(
							{
								ctor: '::',
								_0: _p41._1,
								_1: {
									ctor: '::',
									_0: _p41._2,
									_1: {
										ctor: '::',
										_0: _p41._3,
										_1: {ctor: '[]'}
									}
								}
							});
					case 'FnCall':
						return rlist(_p41._2);
					case 'Lambda':
						return r(_p41._2);
					case 'Thread':
						return rlist(_p41._1);
					default:
						return r(_p41._1);
				}
			}();
			var $this = fn(expr) ? {
				ctor: '::',
				_0: expr,
				_1: {ctor: '[]'}
			} : {ctor: '[]'};
			return A2(_elm_lang$core$Basics_ops['++'], $this, nested);
		}
	});
var _user$project$AST$threadAncestors = F2(
	function (id, expr) {
		return A3(
			_user$project$AST$ancestorsWhere,
			id,
			expr,
			function (e) {
				var _p42 = e;
				if (_p42.ctor === 'Thread') {
					return true;
				} else {
					return false;
				}
			});
	});
var _user$project$AST$siblings = F2(
	function (id, ast) {
		var parent = A2(_user$project$AST$parentOf_, id, ast);
		var _p43 = parent;
		if (_p43.ctor === 'Nothing') {
			return {
				ctor: '::',
				_0: id,
				_1: {ctor: '[]'}
			};
		} else {
			var _p44 = _p43._0;
			switch (_p44.ctor) {
				case 'If':
					return A2(
						_elm_lang$core$List$map,
						_user$project$AST$toID,
						{
							ctor: '::',
							_0: _p44._1,
							_1: {
								ctor: '::',
								_0: _p44._2,
								_1: {
									ctor: '::',
									_0: _p44._3,
									_1: {ctor: '[]'}
								}
							}
						});
				case 'Let':
					return {
						ctor: '::',
						_0: _user$project$Types$holeOrID(_p44._1),
						_1: {
							ctor: '::',
							_0: _user$project$AST$toID(_p44._2),
							_1: {
								ctor: '::',
								_0: _user$project$AST$toID(_p44._3),
								_1: {ctor: '[]'}
							}
						}
					};
				case 'FnCall':
					return A2(_elm_lang$core$List$map, _user$project$AST$toID, _p44._2);
				case 'Lambda':
					return {
						ctor: '::',
						_0: _user$project$AST$toID(_p44._2),
						_1: {ctor: '[]'}
					};
				case 'Thread':
					return A2(_elm_lang$core$List$map, _user$project$AST$toID, _p44._1);
				case 'FieldAccess':
					return {
						ctor: '::',
						_0: _user$project$AST$toID(_p44._1),
						_1: {
							ctor: '::',
							_0: _user$project$Types$holeOrID(_p44._2),
							_1: {ctor: '[]'}
						}
					};
				default:
					return {
						ctor: '::',
						_0: id,
						_1: {ctor: '[]'}
					};
			}
		}
	});
var _user$project$AST$subExpr = F2(
	function (id, expr) {
		var filterMaybe = function (xs) {
			return _elm_lang$core$List$head(
				A2(_elm_lang$core$List$filterMap, _elm_lang$core$Basics$identity, xs));
		};
		var nothing = function (_p45) {
			return _elm_lang$core$Maybe$Nothing;
		};
		var returnOr = F2(
			function (fn, e) {
				return _elm_lang$core$Native_Utils.eq(
					_user$project$AST$toID(e),
					id) ? _elm_lang$core$Maybe$Just(e) : fn(e);
			});
		var returnOrNothing = returnOr(
			function (_p46) {
				return _elm_lang$core$Maybe$Nothing;
			});
		var se = _user$project$AST$subExpr(id);
		var _p47 = expr;
		switch (_p47.ctor) {
			case 'Value':
				return returnOrNothing(expr);
			case 'Hole':
				return returnOrNothing(expr);
			case 'Variable':
				return returnOrNothing(expr);
			case 'Let':
				return A2(
					returnOr,
					function (_p48) {
						return filterMaybe(
							{
								ctor: '::',
								_0: se(_p47._3),
								_1: {
									ctor: '::',
									_0: se(_p47._2),
									_1: {ctor: '[]'}
								}
							});
					},
					expr);
			case 'If':
				return A2(
					returnOr,
					function (_p49) {
						var eb = se(_p47._3);
						var ib = se(_p47._2);
						var c = se(_p47._1);
						return filterMaybe(
							{
								ctor: '::',
								_0: c,
								_1: {
									ctor: '::',
									_0: ib,
									_1: {
										ctor: '::',
										_0: eb,
										_1: {ctor: '[]'}
									}
								}
							});
					},
					expr);
			case 'FnCall':
				return A2(
					returnOr,
					function (_p50) {
						return filterMaybe(
							A2(_elm_lang$core$List$map, se, _p47._2));
					},
					expr);
			case 'Lambda':
				return A2(
					returnOr,
					function (_p51) {
						return se(_p47._2);
					},
					expr);
			case 'Thread':
				return A2(
					returnOr,
					function (_p52) {
						return filterMaybe(
							A2(_elm_lang$core$List$map, se, _p47._1));
					},
					expr);
			default:
				return A2(
					returnOr,
					function (_p53) {
						return se(_p47._1);
					},
					expr);
		}
	});
var _user$project$AST$inAST = F2(
	function (id, ast) {
		var expr = _elm_community$maybe_extra$Maybe_Extra$toList(
			A2(
				_elm_lang$core$Maybe$map,
				_user$project$AST$toID,
				A2(_user$project$AST$subExpr, id, ast)));
		var holes = _user$project$AST$listHoles(ast);
		return A2(
			_elm_lang$core$List$member,
			id,
			A2(_elm_lang$core$Basics_ops['++'], holes, expr));
	});
var _user$project$AST$subtree = F2(
	function (id, ast) {
		return _user$project$Util$deMaybe(
			A2(_user$project$AST$subExpr, id, ast));
	});
var _user$project$AST$replaceExpr = F3(
	function (id, replacement, expr) {
		var re = A2(_user$project$AST$replaceExpr, id, replacement);
		var reList = function (exprs) {
			return A2(_elm_lang$core$List$map, re, exprs);
		};
		if (_elm_lang$core$Native_Utils.eq(
			_user$project$AST$toID(expr),
			id)) {
			return replacement;
		} else {
			var _p54 = expr;
			switch (_p54.ctor) {
				case 'Let':
					return A4(
						_user$project$Types$Let,
						_p54._0,
						_p54._1,
						re(_p54._2),
						re(_p54._3));
				case 'If':
					return A4(
						_user$project$Types$If,
						_p54._0,
						re(_p54._1),
						re(_p54._2),
						re(_p54._3));
				case 'FnCall':
					return A3(
						_user$project$Types$FnCall,
						_p54._0,
						_p54._1,
						reList(_p54._2));
				case 'Lambda':
					return A3(
						_user$project$Types$Lambda,
						_p54._0,
						_p54._1,
						re(_p54._2));
				case 'Thread':
					return A2(
						_user$project$Types$Thread,
						_p54._0,
						reList(_p54._1));
				case 'FieldAccess':
					return A3(
						_user$project$Types$FieldAccess,
						_p54._0,
						re(_p54._1),
						_p54._2);
				case 'Hole':
					return expr;
				case 'Value':
					return expr;
				default:
					return expr;
			}
		}
	});
var _user$project$AST$deleteExpr = F2(
	function (id, ast) {
		var replacement = _user$project$Types$Hole(
			_user$project$Types$ID(
				_user$project$Util$random(
					{ctor: '_Tuple0'})));
		return {
			ctor: '_Tuple2',
			_0: _user$project$AST$toID(replacement),
			_1: A3(_user$project$AST$replaceExpr, id, replacement, ast)
		};
	});
var _user$project$AST$isLeaf = F2(
	function (id, ast) {
		var _p55 = A2(_user$project$AST$subExpr, id, ast);
		if (_p55.ctor === 'Nothing') {
			return false;
		} else {
			var _p56 = _p55._0;
			switch (_p56.ctor) {
				case 'Value':
					return true;
				case 'Hole':
					return true;
				case 'Variable':
					return true;
				case 'FnCall':
					return _elm_lang$core$Native_Utils.eq(
						_elm_lang$core$List$length(_p56._2),
						0);
				default:
					return false;
			}
		}
	});
var _user$project$AST$isInfix = function (name) {
	return A2(
		_elm_lang$core$List$member,
		name,
		{
			ctor: '::',
			_0: '<',
			_1: {
				ctor: '::',
				_0: '==',
				_1: {
					ctor: '::',
					_0: '%',
					_1: {
						ctor: '::',
						_0: '+',
						_1: {
							ctor: '::',
							_0: '-',
							_1: {
								ctor: '::',
								_0: '^',
								_1: {
									ctor: '::',
									_0: '!=',
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			}
		});
};

var _user$project$Analysis$getAnalysisResults = F2(
	function (m, id) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			{
				id: id,
				astValue: {value: 'null', tipe: _user$project$Types$TNull, json: 'null', exc: _elm_lang$core$Maybe$Nothing},
				liveValues: _elm_lang$core$Dict$empty,
				availableVarnames: _elm_lang$core$Dict$empty
			},
			_elm_lang$core$List$head(
				A2(
					_elm_lang$core$List$filter,
					function (tlar) {
						return _elm_lang$core$Native_Utils.eq(tlar.id, id);
					},
					m.analysis)));
	});
var _user$project$Analysis$getLiveValuesDict = F2(
	function (m, id) {
		return function (_) {
			return _.liveValues;
		}(
			A2(_user$project$Analysis$getAnalysisResults, m, id));
	});
var _user$project$Analysis$getLiveValue = F3(
	function (m, tlid, _p0) {
		var _p1 = _p0;
		return A2(
			_elm_lang$core$Dict$get,
			_p1._0,
			A2(_user$project$Analysis$getLiveValuesDict, m, tlid));
	});
var _user$project$Analysis$getAvailableVarnamesDict = F2(
	function (m, id) {
		return function (_) {
			return _.availableVarnames;
		}(
			A2(_user$project$Analysis$getAnalysisResults, m, id));
	});
var _user$project$Analysis$getAvailableVarnames = F3(
	function (m, tlid, _p2) {
		var _p3 = _p2;
		return A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '[]'},
			A2(
				_elm_lang$core$Dict$get,
				_p3._0,
				A2(_user$project$Analysis$getAvailableVarnamesDict, m, tlid)));
	});

var _user$project$Runtime$str2tipe = function (t) {
	var _p0 = _elm_lang$core$String$toLower(t);
	switch (_p0) {
		case 'any':
			return _user$project$Types$TAny;
		case 'int':
			return _user$project$Types$TInt;
		case 'float':
			return _user$project$Types$TFloat;
		case 'bool':
			return _user$project$Types$TBool;
		case 'nothing':
			return _user$project$Types$TNull;
		case 'char':
			return _user$project$Types$TChar;
		case 'str':
			return _user$project$Types$TStr;
		case 'list':
			return _user$project$Types$TList;
		case 'obj':
			return _user$project$Types$TObj;
		case 'block':
			return _user$project$Types$TBlock;
		case 'incomplete':
			return _user$project$Types$TIncomplete;
		case 'response':
			return _user$project$Types$TResp;
		case 'datastore':
			return _user$project$Types$TDB;
		case 'error':
			return _user$project$Types$TIncomplete;
		default:
			return _elm_lang$core$Native_Utils.crashCase(
				'Runtime',
				{
					start: {line: 51, column: 3},
					end: {line: 66, column: 48}
				},
				_p0)(
				A2(_elm_lang$core$Basics_ops['++'], 'invalid typename: ', t));
	}
};
var _user$project$Runtime$tipe2str = function (t) {
	var _p2 = t;
	switch (_p2.ctor) {
		case 'TAny':
			return 'Any';
		case 'TInt':
			return 'Int';
		case 'TFloat':
			return 'Float';
		case 'TBool':
			return 'Bool';
		case 'TNull':
			return 'Nothing';
		case 'TChar':
			return 'Char';
		case 'TStr':
			return 'Str';
		case 'TList':
			return 'List';
		case 'TObj':
			return 'Obj';
		case 'TBlock':
			return 'Block';
		case 'TIncomplete':
			return 'Incomplete';
		case 'TResp':
			return 'Response';
		default:
			return 'Datastore';
	}
};
var _user$project$Runtime$isCompatible = F2(
	function (t1, t2) {
		return _elm_lang$core$Native_Utils.eq(t1, _user$project$Types$TAny) || (_elm_lang$core$Native_Utils.eq(t2, _user$project$Types$TAny) || _elm_lang$core$Native_Utils.eq(t1, t2));
	});
var _user$project$Runtime$isChar = function (s) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$String$length(s),
		3) && (A2(_elm_lang$core$String$startsWith, s, '\'') && A2(_elm_lang$core$String$endsWith, s, '\''));
};
var _user$project$Runtime$isBool = function (s) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$String$toLower(s),
		'true') || _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$String$toLower(s),
		'false');
};
var _user$project$Runtime$isString = function (s) {
	return A2(_elm_lang$core$String$startsWith, '\"', s) && A2(_elm_lang$core$String$endsWith, '\"', s);
};
var _user$project$Runtime$isFloat = function (s) {
	return _user$project$Util$resultIsOk(
		_elm_lang$core$String$toFloat(s));
};
var _user$project$Runtime$isInt = function (s) {
	return _user$project$Util$resultIsOk(
		_elm_lang$core$String$toInt(s));
};
var _user$project$Runtime$tipeOf = function (s) {
	return _user$project$Runtime$isInt(s) ? _user$project$Types$TInt : (_user$project$Runtime$isFloat(s) ? _user$project$Types$TFloat : (_user$project$Runtime$isString(s) ? _user$project$Types$TStr : (_user$project$Runtime$isChar(s) ? _user$project$Types$TChar : (_user$project$Runtime$isBool(s) ? _user$project$Types$TBool : _user$project$Types$TIncomplete))));
};

var _user$project$Autocomplete$isStringEntry = function (a) {
	return A2(_elm_lang$core$String$startsWith, '\"', a.value);
};
var _user$project$Autocomplete$isLargeStringEntry = function (a) {
	return _user$project$Autocomplete$isStringEntry(a) && A2(_elm_lang$core$String$contains, '\n', a.value);
};
var _user$project$Autocomplete$isSmallStringEntry = function (a) {
	return _user$project$Autocomplete$isStringEntry(a) && (!_user$project$Autocomplete$isLargeStringEntry(a));
};
var _user$project$Autocomplete$findFunction = F2(
	function (a, name) {
		return A2(
			_elm_community$list_extra$List_Extra$find,
			function (f) {
				return _elm_lang$core$Native_Utils.eq(f.name, name);
			},
			a.functions);
	});
var _user$project$Autocomplete$findFirstParam = F2(
	function (_p0, except) {
		var _p1 = _p0;
		return A2(
			_elm_community$list_extra$List_Extra$find,
			function (p) {
				return !_elm_lang$core$Native_Utils.eq(
					_elm_lang$core$Maybe$Just(p),
					except);
			},
			_p1.parameters);
	});
var _user$project$Autocomplete$findParamByType = F2(
	function (_p2, tipe) {
		var _p3 = _p2;
		return A2(
			_elm_community$list_extra$List_Extra$find,
			function (p) {
				return A2(_user$project$Runtime$isCompatible, p.tipe, tipe);
			},
			_p3.parameters);
	});
var _user$project$Autocomplete$showFunctions = F2(
	function (b, a) {
		return _elm_lang$core$Native_Utils.update(
			a,
			{showFunctions: b});
	});
var _user$project$Autocomplete$setVarnames = F2(
	function (vs, a) {
		return _elm_lang$core$Native_Utils.update(
			a,
			{varnames: vs});
	});
var _user$project$Autocomplete$jsonFields = function (json) {
	return A2(
		_elm_lang$core$List$map,
		_user$project$Types$ACField,
		_elm_lang$core$Dict$keys(
			_user$project$Util$deMaybe(
				_elm_lang$core$Result$toMaybe(
					A2(
						_elm_lang$core$Json_Decode$decodeString,
						_elm_lang$core$Json_Decode$dict(_elm_lang$core$Json_Decode$value),
						json)))));
};
var _user$project$Autocomplete$containsOrdered = F2(
	function (needle, haystack) {
		var _p4 = _elm_lang$core$String$uncons(needle);
		if (_p4.ctor === 'Just') {
			var $char = _elm_lang$core$String$fromChar(_p4._0._0);
			return A2(_elm_lang$core$String$contains, $char, haystack) && A2(
				_user$project$Autocomplete$containsOrdered,
				_p4._0._1,
				A2(
					_elm_lang$core$String$join,
					$char,
					A2(
						_elm_lang$core$List$drop,
						1,
						A2(_elm_lang$core$String$split, $char, haystack))));
		} else {
			return true;
		}
	});
var _user$project$Autocomplete$asTypeString = function (item) {
	var _p5 = item;
	switch (_p5.ctor) {
		case 'ACFunction':
			var _p6 = _p5._0;
			return function (s) {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						s,
						A2(
							_elm_lang$core$Basics_ops['++'],
							') ->  ',
							_user$project$Runtime$tipe2str(_p6.returnTipe))));
			}(
				A2(
					_elm_lang$core$String$join,
					', ',
					A2(
						_elm_lang$core$List$map,
						_user$project$Runtime$tipe2str,
						A2(
							_elm_lang$core$List$map,
							function (_) {
								return _.tipe;
							},
							_p6.parameters))));
		case 'ACField':
			return '';
		default:
			return '';
	}
};
var _user$project$Autocomplete$asName = function (aci) {
	var _p7 = aci;
	switch (_p7.ctor) {
		case 'ACFunction':
			return _p7._0.name;
		case 'ACField':
			return _p7._0;
		default:
			return _p7._0;
	}
};
var _user$project$Autocomplete$asString = function (aci) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_user$project$Autocomplete$asName(aci),
		_user$project$Autocomplete$asTypeString(aci));
};
var _user$project$Autocomplete$regenerate = function (a) {
	var functions = a.showFunctions ? a.functions : {ctor: '[]'};
	var fields = function () {
		var _p8 = a.liveValue;
		if (_p8.ctor === 'Just') {
			var _p9 = _p8._0;
			return _elm_lang$core$Native_Utils.eq(_p9.tipe, _user$project$Types$TObj) ? _user$project$Autocomplete$jsonFields(_p9.json) : {ctor: '[]'};
		} else {
			return {ctor: '[]'};
		}
	}();
	var lcq = _elm_lang$core$String$toLower(a.value);
	var options = A2(
		_elm_lang$core$List$filter,
		function (i) {
			return A2(
				_elm_lang$core$String$contains,
				lcq,
				A3(
					_user$project$Util$replace,
					'⟶',
					'->',
					_elm_lang$core$String$toLower(
						function (i) {
							return (_elm_lang$core$Native_Utils.cmp(
								1,
								_elm_lang$core$String$length(lcq)) > -1) ? _user$project$Autocomplete$asName(i) : _user$project$Autocomplete$asString(i);
						}(i))));
		},
		A2(
			_elm_lang$core$List$append,
			A2(_elm_lang$core$List$map, _user$project$Types$ACVariable, a.varnames),
			A2(
				_elm_lang$core$List$append,
				fields,
				A2(
					_elm_lang$core$List$map,
					_user$project$Types$ACFunction,
					A2(
						_elm_lang$core$List$filter,
						function (fn) {
							var _p10 = a.liveValue;
							if (_p10.ctor === 'Just') {
								return !_elm_lang$core$Native_Utils.eq(
									_elm_lang$core$Maybe$Nothing,
									A2(_user$project$Autocomplete$findParamByType, fn, _p10._0.tipe));
							} else {
								return true;
							}
						},
						A2(
							_elm_lang$core$List$filter,
							function (_p11) {
								var _p12 = _p11;
								var _p13 = a.tipe;
								if (_p13.ctor === 'Just') {
									return A2(_user$project$Runtime$isCompatible, _p12.returnTipe, _p13._0);
								} else {
									return true;
								}
							},
							functions))))));
	var completions = (!a.open) ? {ctor: '[]'} : options;
	return _elm_lang$core$Native_Utils.update(
		a,
		{
			completions: completions,
			index: _elm_lang$core$Native_Utils.eq(
				_elm_lang$core$List$length(completions),
				0) ? -1 : ((_elm_lang$core$Native_Utils.cmp(
				_elm_lang$core$List$length(completions),
				_elm_lang$core$List$length(a.completions)) < 0) ? 0 : a.index)
		});
};
var _user$project$Autocomplete$setQuery = F2(
	function (q, a) {
		return _user$project$Autocomplete$regenerate(
			_elm_lang$core$Native_Utils.update(
				a,
				{value: q}));
	});
var _user$project$Autocomplete$appendQuery = F2(
	function (str, a) {
		var q = _user$project$Autocomplete$isStringEntry(a) ? A2(
			_elm_lang$core$Basics_ops['++'],
			A2(_elm_lang$core$String$dropRight, 1, a.value),
			A2(_elm_lang$core$Basics_ops['++'], str, '\"')) : A2(_elm_lang$core$Basics_ops['++'], a.value, str);
		return A2(_user$project$Autocomplete$setQuery, q, a);
	});
var _user$project$Autocomplete$sharedPrefix2 = F2(
	function (l, r) {
		var _p14 = {
			ctor: '_Tuple2',
			_0: _elm_lang$core$String$uncons(l),
			_1: _elm_lang$core$String$uncons(r)
		};
		if (((((_p14.ctor === '_Tuple2') && (_p14._0.ctor === 'Just')) && (_p14._0._0.ctor === '_Tuple2')) && (_p14._1.ctor === 'Just')) && (_p14._1._0.ctor === '_Tuple2')) {
			var _p15 = _p14._0._0._0;
			return _elm_lang$core$Native_Utils.eq(_p15, _p14._1._0._0) ? A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$String$fromChar(_p15),
				A2(_user$project$Autocomplete$sharedPrefix2, _p14._0._0._1, _p14._1._0._1)) : '';
		} else {
			return '';
		}
	});
var _user$project$Autocomplete$sharedPrefixList = function (strs) {
	var _p16 = _elm_lang$core$List$head(strs);
	if (_p16.ctor === 'Nothing') {
		return '';
	} else {
		return A3(_elm_lang$core$List$foldl, _user$project$Autocomplete$sharedPrefix2, _p16._0, strs);
	}
};
var _user$project$Autocomplete$sharedPrefix = function (a) {
	return _user$project$Autocomplete$sharedPrefixList(
		A2(_elm_lang$core$List$map, _user$project$Autocomplete$asName, a.completions));
};
var _user$project$Autocomplete$compareSuggestionWithActual = F2(
	function (a, actual) {
		var suggestion = _user$project$Autocomplete$sharedPrefix(a);
		var _p17 = A2(
			_elm_lang$core$String$indexes,
			_elm_lang$core$String$toLower(actual),
			_elm_lang$core$String$toLower(suggestion));
		if (_p17.ctor === '[]') {
			return {ctor: '_Tuple3', _0: '', _1: suggestion, _2: actual};
		} else {
			var _p18 = _p17._0;
			var suffix = A3(
				_elm_lang$core$String$slice,
				_p18 + _elm_lang$core$String$length(actual),
				_elm_lang$core$String$length(suggestion),
				suggestion);
			var prefix = A3(_elm_lang$core$String$slice, 0, _p18, suggestion);
			return {
				ctor: '_Tuple3',
				_0: prefix,
				_1: A2(
					_elm_lang$core$Basics_ops['++'],
					prefix,
					A2(_elm_lang$core$Basics_ops['++'], actual, suffix)),
				_2: actual
			};
		}
	});
var _user$project$Autocomplete$highlighted = function (a) {
	return A2(_elm_community$list_extra$List_Extra$getAt, a.index, a.completions);
};
var _user$project$Autocomplete$getValue = function (a) {
	var _p19 = _user$project$Autocomplete$highlighted(a);
	if (_p19.ctor === 'Just') {
		return _user$project$Autocomplete$asName(_p19._0);
	} else {
		return a.value;
	}
};
var _user$project$Autocomplete$selectUp = function (a) {
	var max = _elm_lang$core$List$length(a.completions) - 1;
	return _elm_lang$core$Native_Utils.update(
		a,
		{
			index: (_elm_lang$core$Native_Utils.cmp(a.index, 0) < 1) ? max : (a.index - 1)
		});
};
var _user$project$Autocomplete$selectDown = function (a) {
	var max_ = _elm_lang$core$List$length(a.completions);
	var max = A2(_elm_lang$core$Basics$max, max_, 1);
	var $new = A2(_elm_lang$core$Basics_ops['%'], a.index + 1, max);
	return _elm_lang$core$Native_Utils.update(
		a,
		{index: $new});
};
var _user$project$Autocomplete$open = F2(
	function (o, a) {
		return _elm_lang$core$Native_Utils.update(
			a,
			{open: o});
	});
var _user$project$Autocomplete$clear = function (a) {
	var cleared = A2(_user$project$Autocomplete$setQuery, '', a);
	return _elm_lang$core$Native_Utils.update(
		cleared,
		{index: -1});
};
var _user$project$Autocomplete$forLiveValue = F2(
	function (lv, a) {
		return _elm_lang$core$Native_Utils.update(
			a,
			{liveValue: lv});
	});
var _user$project$Autocomplete$init = function (functions) {
	return {
		functions: functions,
		varnames: {ctor: '[]'},
		completions: A2(_elm_lang$core$List$map, _user$project$Types$ACFunction, functions),
		index: -1,
		open: true,
		showFunctions: true,
		value: '',
		liveValue: _elm_lang$core$Maybe$Nothing,
		tipe: _elm_lang$core$Maybe$Nothing
	};
};
var _user$project$Autocomplete$reset = function (a) {
	return _user$project$Autocomplete$init(a.functions);
};
var _user$project$Autocomplete$update = F2(
	function (mod, a) {
		return _user$project$Autocomplete$regenerate(
			function () {
				var _p20 = mod;
				switch (_p20.ctor) {
					case 'ACSetQuery':
						return A2(_user$project$Autocomplete$setQuery, _p20._0, a);
					case 'ACAppendQuery':
						return A2(_user$project$Autocomplete$appendQuery, _p20._0, a);
					case 'ACReset':
						return _user$project$Autocomplete$reset(a);
					case 'ACOpen':
						return A2(_user$project$Autocomplete$open, _p20._0, a);
					case 'ACClear':
						return _user$project$Autocomplete$clear(a);
					case 'ACSelectDown':
						return _user$project$Autocomplete$selectDown(a);
					case 'ACSelectUp':
						return _user$project$Autocomplete$selectUp(a);
					case 'ACFilterByLiveValue':
						return A2(_user$project$Autocomplete$forLiveValue, _p20._0, a);
					case 'ACSetAvailableVarnames':
						return A2(_user$project$Autocomplete$setVarnames, _p20._0, a);
					default:
						return A2(_user$project$Autocomplete$showFunctions, _p20._0, a);
				}
			}());
	});
var _user$project$Autocomplete$empty = _user$project$Autocomplete$init(
	{ctor: '[]'});
var _user$project$Autocomplete$height = function (i) {
	return (_elm_lang$core$Native_Utils.cmp(i, 5) < 0) ? 0 : (16 * (i - 5));
};
var _user$project$Autocomplete$focusItem = function (i) {
	return A2(
		_elm_lang$core$Task$attempt,
		_user$project$Types$FocusAutocompleteItem,
		A2(
			_elm_lang$dom$Dom_Scroll$toY,
			'autocomplete-holder',
			_elm_lang$core$Basics$toFloat(
				_user$project$Autocomplete$height(i))));
};

var _user$project$DB$listColTypeHoles = function (db) {
	var r2h = function (col) {
		var _p0 = col;
		if ((_p0.ctor === '_Tuple2') && (_p0._1.ctor === 'Empty')) {
			return {
				ctor: '::',
				_0: _p0._1._0,
				_1: {ctor: '[]'}
			};
		} else {
			return {ctor: '[]'};
		}
	};
	return _elm_lang$core$List$concat(
		A2(_elm_lang$core$List$map, r2h, db.cols));
};
var _user$project$DB$listColNameHoles = function (db) {
	var r2h = function (col) {
		var _p1 = col;
		if ((_p1.ctor === '_Tuple2') && (_p1._0.ctor === 'Empty')) {
			return {
				ctor: '::',
				_0: _p1._0._0,
				_1: {ctor: '[]'}
			};
		} else {
			return {ctor: '[]'};
		}
	};
	return _elm_lang$core$List$concat(
		A2(_elm_lang$core$List$map, r2h, db.cols));
};
var _user$project$DB$listHoles = function (db) {
	return A2(
		_elm_community$list_extra$List_Extra$interweave,
		_user$project$DB$listColNameHoles(db),
		_user$project$DB$listColTypeHoles(db));
};

var _user$project$Defaults$model2editor = function (m) {
	return {};
};
var _user$project$Defaults$defaultEditor = {};
var _user$project$Defaults$moveSize = 50;
var _user$project$Defaults$initialPos = {vx: 475, vy: 325};
var _user$project$Defaults$defaultModel = function (e) {
	return {
		error: _elm_lang$core$Maybe$Nothing,
		lastMsg: _user$project$Types$Initialization,
		lastMod: _user$project$Types$NoChange,
		center: {x: _user$project$Defaults$initialPos.vx, y: _user$project$Defaults$initialPos.vy},
		complete: _user$project$Autocomplete$empty,
		state: _user$project$Types$Deselected,
		tests: {ctor: '[]'},
		toplevels: {ctor: '[]'},
		analysis: {ctor: '[]'},
		integrationTestState: _user$project$Types$NoIntegrationTest
	};
};
var _user$project$Defaults$leftButton = 0;
var _user$project$Defaults$entryID = 'entryBox';

var _user$project$Viewport$moveRight = function (c) {
	return {x: c.x + _user$project$Defaults$moveSize, y: c.y};
};
var _user$project$Viewport$moveLeft = function (c) {
	return {x: c.x - _user$project$Defaults$moveSize, y: c.y};
};
var _user$project$Viewport$moveDown = function (c) {
	return {x: c.x, y: c.y + _user$project$Defaults$moveSize};
};
var _user$project$Viewport$moveUp = function (c) {
	return {x: c.x, y: c.y - _user$project$Defaults$moveSize};
};
var _user$project$Viewport$toAbsolute = F2(
	function (m, pos) {
		var d = function (_) {
			return _.center;
		}(
			_user$project$Defaults$defaultModel(_user$project$Defaults$defaultEditor));
		return {x: (pos.vx + m.center.x) - d.x, y: (pos.vy + m.center.y) - d.y};
	});
var _user$project$Viewport$toViewport = F2(
	function (m, pos) {
		var d = function (_) {
			return _.center;
		}(
			_user$project$Defaults$defaultModel(_user$project$Defaults$defaultEditor));
		return {vx: (d.x + pos.x) - m.center.x, vy: (d.y + pos.y) - m.center.y};
	});

var _user$project$Toplevel$rootOf = function (tl) {
	var _p0 = tl.data;
	if (_p0.ctor === 'TLHandler') {
		return _elm_lang$core$Maybe$Just(
			_user$project$AST$toID(_p0._0.ast));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _user$project$Toplevel$getChildrenOf = F2(
	function (tl, id) {
		var _p1 = tl.data;
		if (_p1.ctor === 'TLHandler') {
			return A2(_user$project$AST$childrenOf, id, _p1._0.ast);
		} else {
			return {ctor: '[]'};
		}
	});
var _user$project$Toplevel$firstChild = F2(
	function (tl, id) {
		return _elm_lang$core$List$head(
			A2(_user$project$Toplevel$getChildrenOf, tl, id));
	});
var _user$project$Toplevel$getParentOf = F2(
	function (tl, id) {
		var _p2 = tl.data;
		if (_p2.ctor === 'TLHandler') {
			return A2(
				_elm_lang$core$Maybe$map,
				_user$project$AST$toID,
				A2(_user$project$AST$parentOf_, id, _p2._0.ast));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _user$project$Toplevel$asDB = function (tl) {
	var _p3 = tl.data;
	if (_p3.ctor === 'TLDB') {
		return _elm_lang$core$Maybe$Just(_p3._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _user$project$Toplevel$dbs = function (tls) {
	return A2(_elm_lang$core$List$filterMap, _user$project$Toplevel$asDB, tls);
};
var _user$project$Toplevel$asHandler = function (tl) {
	var _p4 = tl.data;
	if (_p4.ctor === 'TLHandler') {
		return _elm_lang$core$Maybe$Just(_p4._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _user$project$Toplevel$handlers = function (tls) {
	return A2(_elm_lang$core$List$filterMap, _user$project$Toplevel$asHandler, tls);
};
var _user$project$Toplevel$moveTL = F3(
	function (xOffset, yOffset, tl) {
		var newPos = {x: tl.pos.x + xOffset, y: tl.pos.y + yOffset};
		return _elm_lang$core$Native_Utils.update(
			tl,
			{pos: newPos});
	});
var _user$project$Toplevel$update = F3(
	function (m, tlid, f) {
		var mapped = A2(
			_elm_lang$core$List$map,
			function (t) {
				return (!_elm_lang$core$Native_Utils.eq(t.id, tlid)) ? t : f(t);
			},
			m.toplevels);
		return _elm_lang$core$Native_Utils.update(
			m,
			{toplevels: mapped});
	});
var _user$project$Toplevel$move = F4(
	function (tlid, xOffset, yOffset, m) {
		return A3(
			_user$project$Toplevel$update,
			m,
			tlid,
			A2(_user$project$Toplevel$moveTL, xOffset, yOffset));
	});
var _user$project$Toplevel$siblings = F2(
	function (tl, id) {
		var _p5 = tl.data;
		if (_p5.ctor === 'TLHandler') {
			var _p7 = _p5._0;
			var _p6 = A2(_user$project$Toplevel$getParentOf, tl, id);
			if (_p6.ctor === 'Just') {
				return A2(_user$project$AST$siblings, id, _p7.ast);
			} else {
				var specs = {
					ctor: '::',
					_0: _p7.spec.module_,
					_1: {
						ctor: '::',
						_0: _p7.spec.name,
						_1: {
							ctor: '::',
							_0: _p7.spec.modifier,
							_1: {ctor: '[]'}
						}
					}
				};
				return A2(
					_elm_lang$core$Basics_ops['++'],
					{
						ctor: '::',
						_0: _user$project$AST$toID(_p7.ast),
						_1: {ctor: '[]'}
					},
					A2(_elm_lang$core$List$map, _user$project$Types$holeOrID, specs));
			}
		} else {
			return {ctor: '[]'};
		}
	});
var _user$project$Toplevel$getNextSibling = F2(
	function (tl, id) {
		var _p8 = tl.data;
		if (_p8.ctor === 'TLHandler') {
			var sibs = A2(_user$project$Toplevel$siblings, tl, id);
			return A2(
				_elm_lang$core$Maybe$withDefault,
				id,
				A2(
					_elm_lang$core$Maybe$andThen,
					function (i) {
						return A2(_elm_community$list_extra$List_Extra$getAt, i, sibs);
					},
					A2(
						_elm_lang$core$Maybe$map,
						function (i) {
							return A2(
								_elm_lang$core$Basics_ops['%'],
								i,
								_elm_lang$core$List$length(sibs));
						},
						A2(
							_elm_lang$core$Maybe$map,
							F2(
								function (x, y) {
									return x + y;
								})(1),
							A2(_elm_community$list_extra$List_Extra$elemIndex, id, sibs)))));
		} else {
			return id;
		}
	});
var _user$project$Toplevel$getPrevSibling = F2(
	function (tl, id) {
		var _p9 = tl.data;
		if (_p9.ctor === 'TLHandler') {
			var sibs = A2(_user$project$Toplevel$siblings, tl, id);
			var last = _user$project$Util$deMaybe(
				_elm_community$list_extra$List_Extra$last(sibs));
			return A2(
				_elm_lang$core$Maybe$withDefault,
				last,
				A2(
					_elm_lang$core$Maybe$andThen,
					function (i) {
						return A2(_elm_community$list_extra$List_Extra$getAt, i, sibs);
					},
					A2(
						_elm_lang$core$Maybe$map,
						function (i) {
							return i - 1;
						},
						A2(_elm_community$list_extra$List_Extra$elemIndex, id, sibs))));
		} else {
			return id;
		}
	});
var _user$project$Toplevel$replaceSpecHole = F3(
	function (id, value, hs) {
		var rh = function (a) {
			var _p10 = a;
			if (_p10.ctor === 'Empty') {
				var _p11 = _p10._0;
				return _elm_lang$core$Native_Utils.eq(id, _p11) ? A2(_user$project$Types$Full, _p11, value) : a;
			} else {
				var _p12 = _p10._0;
				return _elm_lang$core$Native_Utils.eq(id, _p12) ? A2(_user$project$Types$Full, _p12, value) : a;
			}
		};
		return {
			name: rh(hs.name),
			module_: rh(hs.module_),
			modifier: rh(hs.modifier)
		};
	});
var _user$project$Toplevel$filledSpecs = function (h) {
	var f2l = function (a) {
		var _p13 = a;
		if (_p13.ctor === 'Full') {
			return {
				ctor: '::',
				_0: _p13._0,
				_1: {ctor: '[]'}
			};
		} else {
			return {ctor: '[]'};
		}
	};
	return A2(
		_elm_lang$core$Basics_ops['++'],
		f2l(h.spec.module_),
		A2(
			_elm_lang$core$Basics_ops['++'],
			f2l(h.spec.name),
			f2l(h.spec.modifier)));
};
var _user$project$Toplevel$isFilledSpec = F2(
	function (h, id) {
		return A2(
			_elm_lang$core$List$member,
			id,
			_user$project$Toplevel$filledSpecs(h));
	});
var _user$project$Toplevel$getSpec = F2(
	function (h, id) {
		return _elm_lang$core$List$head(
			A2(
				_elm_lang$core$List$filter,
				function (spec) {
					var _p14 = spec;
					if (_p14.ctor === 'Full') {
						return _elm_lang$core$Native_Utils.eq(_p14._0, id);
					} else {
						return _elm_lang$core$Native_Utils.eq(_p14._0, id);
					}
				},
				{
					ctor: '::',
					_0: h.spec.module_,
					_1: {
						ctor: '::',
						_0: h.spec.name,
						_1: {
							ctor: '::',
							_0: h.spec.modifier,
							_1: {ctor: '[]'}
						}
					}
				}));
	});
var _user$project$Toplevel$specHoles = function (h) {
	var e2l = function (a) {
		var _p15 = a;
		if (_p15.ctor === 'Empty') {
			return {
				ctor: '::',
				_0: _p15._0,
				_1: {ctor: '[]'}
			};
		} else {
			return {ctor: '[]'};
		}
	};
	return A2(
		_elm_lang$core$Basics_ops['++'],
		e2l(h.spec.module_),
		A2(
			_elm_lang$core$Basics_ops['++'],
			e2l(h.spec.name),
			e2l(h.spec.modifier)));
};
var _user$project$Toplevel$allHoles = function (tl) {
	var _p16 = tl.data;
	if (_p16.ctor === 'TLHandler') {
		var _p17 = _p16._0;
		return A2(
			_elm_lang$core$Basics_ops['++'],
			_user$project$AST$listHoles(_p17.ast),
			_user$project$Toplevel$specHoles(_p17));
	} else {
		return _user$project$DB$listHoles(_p16._0);
	}
};
var _user$project$Toplevel$getPrevHole = F2(
	function (tl, next) {
		var holes = _user$project$Toplevel$allHoles(tl);
		return A2(
			_elm_lang$core$Maybe$andThen,
			function (i) {
				return A2(_elm_community$list_extra$List_Extra$getAt, i, holes);
			},
			A2(
				_elm_lang$core$Maybe$map,
				function (i) {
					return i - 1;
				},
				A2(_elm_community$list_extra$List_Extra$elemIndex, next, holes)));
	});
var _user$project$Toplevel$firstHole = function (tl) {
	return _elm_lang$core$List$head(
		_user$project$Toplevel$allHoles(tl));
};
var _user$project$Toplevel$getNextHole = F2(
	function (tl, pred) {
		var _p18 = pred;
		if (_p18.ctor === 'Just') {
			var holes = _user$project$Toplevel$allHoles(tl);
			return A2(
				_elm_lang$core$Maybe$andThen,
				function (i) {
					return A2(_elm_community$list_extra$List_Extra$getAt, i, holes);
				},
				A2(
					_elm_lang$core$Maybe$map,
					F2(
						function (x, y) {
							return x + y;
						})(1),
					A2(_elm_community$list_extra$List_Extra$elemIndex, _p18._0, holes)));
		} else {
			return _user$project$Toplevel$firstHole(tl);
		}
	});
var _user$project$Toplevel$isExpression = F2(
	function (h, id) {
		var _p19 = A2(_user$project$AST$subExpr, id, h.ast);
		if (_p19.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _user$project$Toplevel$isDBColTypeHole = F2(
	function (db, id) {
		return A2(
			_elm_lang$core$List$member,
			id,
			_user$project$DB$listColTypeHoles(db));
	});
var _user$project$Toplevel$isDBColNameHole = F2(
	function (db, id) {
		return A2(
			_elm_lang$core$List$member,
			id,
			_user$project$DB$listColNameHoles(db));
	});
var _user$project$Toplevel$isFieldHole = F2(
	function (h, id) {
		return A2(
			_elm_lang$core$List$member,
			id,
			_user$project$AST$listFieldHoles(h.ast));
	});
var _user$project$Toplevel$isExprHole = F2(
	function (h, id) {
		var bhs = _user$project$AST$listBindHoles(h.ast);
		return A2(
			_elm_lang$core$List$member,
			id,
			A2(
				_elm_lang$core$List$filter,
				function (hl) {
					return !A2(_elm_lang$core$List$member, hl, bhs);
				},
				_user$project$AST$listHoles(h.ast)));
	});
var _user$project$Toplevel$isSpecHole = F2(
	function (h, id) {
		return A2(
			_elm_lang$core$List$member,
			id,
			_user$project$Toplevel$specHoles(h));
	});
var _user$project$Toplevel$isSpec = F2(
	function (h, id) {
		return A2(_user$project$Toplevel$isSpecHole, h, id) || A2(_user$project$Toplevel$isFilledSpec, h, id);
	});
var _user$project$Toplevel$isBindHole = F2(
	function (h, id) {
		return A2(
			_elm_lang$core$List$member,
			id,
			_user$project$AST$listBindHoles(h.ast));
	});
var _user$project$Toplevel$holeType = F2(
	function (tl, id) {
		var _p20 = tl.data;
		if (_p20.ctor === 'TLHandler') {
			var _p21 = _p20._0;
			return A2(_user$project$Toplevel$isBindHole, _p21, id) ? _user$project$Types$BindHole(_p21) : (A2(_user$project$Toplevel$isSpecHole, _p21, id) ? _user$project$Types$SpecHole(_p21) : (A2(_user$project$Toplevel$isFieldHole, _p21, id) ? _user$project$Types$FieldHole(_p21) : (A2(_user$project$Toplevel$isExprHole, _p21, id) ? _user$project$Types$ExprHole(_p21) : _user$project$Types$NotAHole)));
		} else {
			var _p22 = _p20._0;
			return A2(_user$project$Toplevel$isDBColNameHole, _p22, id) ? _user$project$Types$DBColNameHole(_p22) : (A2(_user$project$Toplevel$isDBColTypeHole, _p22, id) ? _user$project$Types$DBColTypeHole(_p22) : _user$project$Types$NotAHole);
		}
	});
var _user$project$Toplevel$isHole = F2(
	function (tl, id) {
		var _p23 = A2(_user$project$Toplevel$holeType, tl, id);
		if (_p23.ctor === 'NotAHole') {
			return false;
		} else {
			return true;
		}
	});
var _user$project$Toplevel$isThreadHole = F2(
	function (h, id) {
		return A2(
			_elm_lang$core$List$member,
			id,
			_user$project$AST$listThreadHoles(h.ast));
	});
var _user$project$Toplevel$replace = F2(
	function (m, tl) {
		return A3(
			_user$project$Toplevel$update,
			m,
			tl.id,
			function (_p24) {
				return tl;
			});
	});
var _user$project$Toplevel$getTL = F2(
	function (m, id) {
		return _user$project$Util$deMaybe(
			A2(
				_elm_community$list_extra$List_Extra$find,
				function (tl) {
					return _elm_lang$core$Native_Utils.eq(tl.id, id);
				},
				m.toplevels));
	});

var _user$project$Entry$createFunction = F3(
	function (m, name, hasImplicitParam) {
		var fn = _elm_lang$core$List$head(
			A2(
				_elm_lang$core$List$filter,
				function (fn) {
					return _elm_lang$core$Native_Utils.eq(fn.name, name);
				},
				m.complete.functions));
		var holes = function (count) {
			return A2(
				_elm_lang$core$List$map,
				function (_p0) {
					return _user$project$Types$Hole(
						_user$project$Types$gid(
							{ctor: '_Tuple0'}));
				},
				A2(_elm_lang$core$List$range, 1, count));
		};
		var holeModifier = hasImplicitParam ? -1 : 0;
		var _p1 = fn;
		if (_p1.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				A3(
					_user$project$Types$FnCall,
					_user$project$Types$gid(
						{ctor: '_Tuple0'}),
					name,
					holes(
						_elm_lang$core$List$length(_p1._0.parameters) + holeModifier)));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _user$project$Entry$emptyHS = function (_p2) {
	return {
		name: _user$project$Types$Empty(
			_user$project$Types$gid(
				{ctor: '_Tuple0'})),
		module_: _user$project$Types$Empty(
			_user$project$Types$gid(
				{ctor: '_Tuple0'})),
		modifier: _user$project$Types$Empty(
			_user$project$Types$gid(
				{ctor: '_Tuple0'}))
	};
};
var _user$project$Entry$tlid = function (unit) {
	return _user$project$Types$TLID(
		_user$project$Util$random(unit));
};
var _user$project$Entry$focusEntry = A2(
	_elm_lang$core$Task$attempt,
	_user$project$Types$FocusEntry,
	_elm_lang$dom$Dom$focus(_user$project$Defaults$entryID));
var _user$project$Entry$createFindSpace = function (m) {
	return _user$project$Types$Enter(
		_user$project$Types$Creating(
			A2(_user$project$Viewport$toAbsolute, m, _user$project$Defaults$initialPos)));
};
var _user$project$Entry$ContinueThread = {ctor: 'ContinueThread'};
var _user$project$Entry$submit = F4(
	function (m, cursor, action, value) {
		var parseAst = F2(
			function (str, hasImplicit) {
				var firstWord = A2(_elm_lang$core$String$split, ' ', str);
				var hid3 = _user$project$Types$gid(
					{ctor: '_Tuple0'});
				var hid2 = _user$project$Types$gid(
					{ctor: '_Tuple0'});
				var hid1 = _user$project$Types$gid(
					{ctor: '_Tuple0'});
				var eid = _user$project$Types$gid(
					{ctor: '_Tuple0'});
				var _p3 = firstWord;
				_v1_4:
				do {
					if ((_p3.ctor === '::') && (_p3._1.ctor === '[]')) {
						switch (_p3._0) {
							case 'if':
								return _elm_lang$core$Maybe$Just(
									A4(
										_user$project$Types$If,
										eid,
										_user$project$Types$Hole(hid1),
										_user$project$Types$Hole(hid2),
										_user$project$Types$Hole(hid3)));
							case 'let':
								return _elm_lang$core$Maybe$Just(
									A4(
										_user$project$Types$Let,
										eid,
										_user$project$Types$Empty(hid1),
										_user$project$Types$Hole(hid2),
										_user$project$Types$Hole(hid3)));
							case 'lambda':
								return _elm_lang$core$Maybe$Just(
									A3(
										_user$project$Types$Lambda,
										eid,
										{
											ctor: '::',
											_0: 'var',
											_1: {ctor: '[]'}
										},
										_user$project$Types$Hole(hid1)));
							case '':
								return _elm_lang$core$Maybe$Just(
									_user$project$Types$Hole(eid));
							default:
								break _v1_4;
						}
					} else {
						break _v1_4;
					}
				} while(false);
				return (_elm_lang$core$Native_Utils.eq(
					_user$project$Runtime$tipeOf(str),
					_user$project$Types$TIncomplete) || _user$project$AST$isInfix(str)) ? A3(_user$project$Entry$createFunction, m, value, hasImplicit) : _elm_lang$core$Maybe$Just(
					A2(_user$project$Types$Value, eid, str));
			});
		var _p4 = cursor;
		if (_p4.ctor === 'Creating') {
			var _p7 = _p4._0;
			var threadIt = function (expr) {
				var _p5 = action;
				if (_p5.ctor === 'ContinueThread') {
					return expr;
				} else {
					var hid = _user$project$Types$gid(
						{ctor: '_Tuple0'});
					return A2(
						_user$project$Types$Thread,
						_user$project$Types$gid(
							{ctor: '_Tuple0'}),
						{
							ctor: '::',
							_0: expr,
							_1: {
								ctor: '::',
								_0: _user$project$Types$Hole(hid),
								_1: {ctor: '[]'}
							}
						});
				}
			};
			var id = _user$project$Entry$tlid(
				{ctor: '_Tuple0'});
			var wrap = function (op) {
				return _user$project$Types$RPC(
					{
						ctor: '_Tuple2',
						_0: {
							ctor: '::',
							_0: op,
							_1: {ctor: '[]'}
						},
						_1: A2(_user$project$Types$FocusNext, id, _elm_lang$core$Maybe$Nothing)
					});
			};
			if (A2(_elm_lang$core$String$startsWith, 'DB', value)) {
				var dbName = _elm_lang$core$String$trim(
					A2(_elm_lang$core$String$dropLeft, 2, value));
				return wrap(
					A3(_user$project$Types$CreateDB, id, _p7, dbName));
			} else {
				if (A2(_elm_lang$core$String$startsWith, '.', value)) {
					var access = A3(
						_user$project$Types$FieldAccess,
						_user$project$Types$gid(
							{ctor: '_Tuple0'}),
						A2(
							_user$project$Types$Variable,
							_user$project$Types$gid(
								{ctor: '_Tuple0'}),
							A2(_elm_lang$core$String$dropLeft, 1, value)),
						_user$project$Types$Empty(
							_user$project$Types$gid(
								{ctor: '_Tuple0'})));
					var ast = threadIt(access);
					var handler = {
						ast: ast,
						spec: _user$project$Entry$emptyHS(
							{ctor: '_Tuple0'})
					};
					return wrap(
						A3(_user$project$Types$SetHandler, id, _p7, handler));
				} else {
					var _p6 = A2(
						parseAst,
						value,
						_elm_lang$core$Native_Utils.eq(action, _user$project$Entry$ContinueThread));
					if (_p6.ctor === 'Nothing') {
						return _user$project$Types$NoChange;
					} else {
						var ast = threadIt(_p6._0);
						var handler = {
							ast: ast,
							spec: _user$project$Entry$emptyHS(
								{ctor: '_Tuple0'})
						};
						return _user$project$Types$RPC(
							{
								ctor: '_Tuple2',
								_0: {
									ctor: '::',
									_0: A3(_user$project$Types$SetHandler, id, _p7, handler),
									_1: {ctor: '[]'}
								},
								_1: A2(_user$project$Types$FocusNext, id, _elm_lang$core$Maybe$Nothing)
							});
					}
				}
			}
		} else {
			var _p19 = _p4._0;
			var _p18 = _p4._1;
			var tl = A2(_user$project$Toplevel$getTL, m, _p19);
			var predecessor = A2(_user$project$Toplevel$getPrevHole, tl, _p18);
			var wrap = function (op) {
				return _user$project$Types$RPC(
					{
						ctor: '_Tuple2',
						_0: {
							ctor: '::',
							_0: op,
							_1: {ctor: '[]'}
						},
						_1: A2(_user$project$Types$FocusNext, _p19, predecessor)
					});
			};
			var replaceExpr = F5(
				function (m, h, tlid, id, value) {
					var ast1 = function () {
						var _p8 = action;
						if (_p8.ctor === 'ContinueThread') {
							return h.ast;
						} else {
							return A2(_user$project$AST$wrapInThread, id, h.ast);
						}
					}();
					var ast2 = A2(_user$project$AST$maybeExtendThreadAt, id, ast1);
					var oldExpr = A2(_user$project$AST$subtree, id, h.ast);
					var newExpr = function () {
						if (A2(_elm_lang$core$String$startsWith, '= ', value)) {
							var _p9 = _elm_lang$core$List$reverse(
								A2(_user$project$AST$threadAncestors, id, h.ast));
							if ((_p9.ctor === '::') && (_p9._0.ctor === 'Thread')) {
								var bindName = _elm_lang$core$String$trim(
									A2(_elm_lang$core$String$dropLeft, 2, value));
								return A4(
									_user$project$Types$Let,
									_user$project$Types$gid(
										{ctor: '_Tuple0'}),
									A2(
										_user$project$Types$Full,
										_user$project$Types$gid(
											{ctor: '_Tuple0'}),
										bindName),
									_user$project$AST$closeThread(_p9._0),
									_user$project$Types$Hole(
										_user$project$Types$gid(
											{ctor: '_Tuple0'})));
							} else {
								return oldExpr;
							}
						} else {
							if (A2(_elm_lang$core$String$startsWith, '.', value)) {
								return A3(
									_user$project$Types$FieldAccess,
									_user$project$Types$gid(
										{ctor: '_Tuple0'}),
									A2(
										_user$project$Types$Variable,
										_user$project$Types$gid(
											{ctor: '_Tuple0'}),
										A2(_elm_lang$core$String$dropLeft, 1, value)),
									_user$project$Types$Empty(
										_user$project$Types$gid(
											{ctor: '_Tuple0'})));
							} else {
								var availableVars = A3(_user$project$Analysis$getAvailableVarnames, m, tlid, id);
								var holeReplacement = A2(_elm_lang$core$List$member, value, availableVars) ? _elm_lang$core$Maybe$Just(
									A2(
										_user$project$Types$Variable,
										_user$project$Types$gid(
											{ctor: '_Tuple0'}),
										value)) : A2(
									parseAst,
									value,
									_elm_lang$core$Native_Utils.eq(action, _user$project$Entry$ContinueThread) && A2(_user$project$Toplevel$isThreadHole, h, id));
								var _p10 = holeReplacement;
								if (_p10.ctor === 'Nothing') {
									return oldExpr;
								} else {
									return _p10._0;
								}
							}
						}
					}();
					var replacement = A3(_user$project$AST$replaceExpr, id, newExpr, ast2);
					return _elm_lang$core$Native_Utils.eq(oldExpr, newExpr) ? _user$project$Types$NoChange : wrap(
						A3(
							_user$project$Types$SetHandler,
							tlid,
							tl.pos,
							_elm_lang$core$Native_Utils.update(
								h,
								{ast: replacement})));
				});
			if (_elm_lang$core$Native_Utils.cmp(
				_elm_lang$core$String$length(value),
				1) < 0) {
				return _user$project$Types$NoChange;
			} else {
				var _p11 = A2(_user$project$Toplevel$holeType, tl, _p18);
				switch (_p11.ctor) {
					case 'DBColTypeHole':
						return wrap(
							A3(_user$project$Types$SetDBColType, _p19, _p18, value));
					case 'DBColNameHole':
						return wrap(
							A3(_user$project$Types$SetDBColName, _p19, _p18, value));
					case 'BindHole':
						var _p12 = _p11._0;
						var replacement = A3(_user$project$AST$replaceBindHole, _p18, value, _p12.ast);
						return wrap(
							A3(
								_user$project$Types$SetHandler,
								_p19,
								tl.pos,
								_elm_lang$core$Native_Utils.update(
									_p12,
									{ast: replacement})));
					case 'SpecHole':
						var _p13 = _p11._0;
						var replacement = A3(_user$project$Toplevel$replaceSpecHole, _p18, value, _p13.spec);
						return wrap(
							A3(
								_user$project$Types$SetHandler,
								_p19,
								tl.pos,
								_elm_lang$core$Native_Utils.update(
									_p13,
									{spec: replacement})));
					case 'FieldHole':
						var _p15 = _p11._0;
						var replacement = A3(_user$project$AST$replaceFieldHole, _p18, value, _p15.ast);
						var withNewParent = function () {
							var _p14 = action;
							if (_p14.ctor === 'ContinueThread') {
								return replacement;
							} else {
								var parent = A2(_user$project$AST$parentOf, _p18, replacement);
								return A2(
									_user$project$AST$wrapInThread,
									_user$project$AST$toID(parent),
									replacement);
							}
						}();
						return wrap(
							A3(
								_user$project$Types$SetHandler,
								_p19,
								tl.pos,
								_elm_lang$core$Native_Utils.update(
									_p15,
									{ast: withNewParent})));
					case 'ExprHole':
						return A5(replaceExpr, m, _p11._0, _p19, _p18, value);
					default:
						var _p16 = tl.data;
						if (_p16.ctor === 'TLHandler') {
							var _p17 = _p16._0;
							if (A2(_user$project$Toplevel$isExpression, _p17, _p18)) {
								return A5(replaceExpr, m, _p17, _p19, _p18, value);
							} else {
								var replacement = A3(_user$project$Toplevel$replaceSpecHole, _p18, value, _p17.spec);
								return wrap(
									A3(
										_user$project$Types$SetHandler,
										_p19,
										tl.pos,
										_elm_lang$core$Native_Utils.update(
											_p17,
											{spec: replacement})));
							}
						} else {
							return _user$project$Types$NoChange;
						}
				}
			}
		}
	});
var _user$project$Entry$StartThread = {ctor: 'StartThread'};

var _user$project$IntegrationTest$enterChangesState = function (_p0) {
	var _p1 = _p0;
	return _user$project$Types$IntegrationTestExpectation(
		function (m) {
			var _p2 = m.state;
			if ((_p2.ctor === 'Entering') && (_p2._0.ctor === 'Creating')) {
				return true;
			} else {
				return false;
			}
		});
};
var _user$project$IntegrationTest$trigger = function (name) {
	var _p3 = name;
	if (_p3 === 'test_empty_integration_test') {
		return _user$project$IntegrationTest$enterChangesState(
			{ctor: '_Tuple0'});
	} else {
		return _elm_lang$core$Native_Utils.crashCase(
			'IntegrationTest',
			{
				start: {line: 7, column: 3},
				end: {line: 9, column: 65}
			},
			_p3)(
			A2(_elm_lang$core$Basics_ops['++'], 'I have no idea what this test is: ', _p3));
	}
};

var _user$project$JSON$decodePair = F2(
	function (d1, d2) {
		return A3(
			_elm_lang$core$Json_Decode$map2,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			A2(_elm_lang$core$Json_Decode$index, 0, d1),
			A2(_elm_lang$core$Json_Decode$index, 1, d2));
	});
var _user$project$JSON$decodeTLID = A2(_elm_lang$core$Json_Decode$map, _user$project$Types$TLID, _elm_lang$core$Json_Decode$int);
var _user$project$JSON$decodeID = A2(_elm_lang$core$Json_Decode$map, _user$project$Types$ID, _elm_lang$core$Json_Decode$int);
var _user$project$JSON$encodeTLID = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$core$Json_Encode$int(_p1._0);
};
var _user$project$JSON$encodeID = function (_p2) {
	var _p3 = _p2;
	return _elm_lang$core$Json_Encode$int(_p3._0);
};
var _user$project$JSON$decodeVariants = function (decoders) {
	var names = A2(_elm_lang$core$List$map, _elm_lang$core$Tuple$first, decoders);
	var nameStr = A2(_elm_lang$core$String$join, ', ', names);
	var map = _elm_lang$core$Dict$fromList(decoders);
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		function (str) {
			var _p4 = A2(_elm_lang$core$Dict$get, str, map);
			if (_p4.ctor === 'Just') {
				return _p4._0;
			} else {
				return _elm_lang$core$Json_Decode$fail(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'Got ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							str,
							A2(_elm_lang$core$Basics_ops['++'], ', expected one of ', nameStr))));
			}
		},
		A2(_elm_lang$core$Json_Decode$index, 0, _elm_lang$core$Json_Decode$string));
};
var _user$project$JSON$decodeVariant1 = F2(
	function ($const, d1) {
		return A2(
			_elm_lang$core$Json_Decode$map,
			$const,
			A2(_elm_lang$core$Json_Decode$index, 1, d1));
	});
var _user$project$JSON$decodeVariant2 = F3(
	function ($const, d1, d2) {
		return A3(
			_elm_lang$core$Json_Decode$map2,
			$const,
			A2(_elm_lang$core$Json_Decode$index, 1, d1),
			A2(_elm_lang$core$Json_Decode$index, 2, d2));
	});
var _user$project$JSON$decodeHoleOr = function (d) {
	return _user$project$JSON$decodeVariants(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'Full',
				_1: A3(_user$project$JSON$decodeVariant2, _user$project$Types$Full, _user$project$JSON$decodeID, d)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'Empty',
					_1: A2(_user$project$JSON$decodeVariant1, _user$project$Types$Empty, _user$project$JSON$decodeID)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$JSON$decodeVariant3 = F4(
	function ($const, d1, d2, d3) {
		return A4(
			_elm_lang$core$Json_Decode$map3,
			$const,
			A2(_elm_lang$core$Json_Decode$index, 1, d1),
			A2(_elm_lang$core$Json_Decode$index, 2, d2),
			A2(_elm_lang$core$Json_Decode$index, 3, d3));
	});
var _user$project$JSON$decodeVariant4 = F5(
	function ($const, d1, d2, d3, d4) {
		return A5(
			_elm_lang$core$Json_Decode$map4,
			$const,
			A2(_elm_lang$core$Json_Decode$index, 1, d1),
			A2(_elm_lang$core$Json_Decode$index, 2, d2),
			A2(_elm_lang$core$Json_Decode$index, 3, d3),
			A2(_elm_lang$core$Json_Decode$index, 4, d4));
	});
var _user$project$JSON$decodeVariant5 = F6(
	function ($const, d1, d2, d3, d4, d5) {
		return A6(
			_elm_lang$core$Json_Decode$map5,
			$const,
			A2(_elm_lang$core$Json_Decode$index, 1, d1),
			A2(_elm_lang$core$Json_Decode$index, 2, d2),
			A2(_elm_lang$core$Json_Decode$index, 3, d3),
			A2(_elm_lang$core$Json_Decode$index, 4, d4),
			A2(_elm_lang$core$Json_Decode$index, 5, d5));
	});
var _user$project$JSON$encodeVariant = F2(
	function (name, vals) {
		return _elm_lang$core$Json_Encode$list(
			{
				ctor: '::',
				_0: _elm_lang$core$Json_Encode$string(name),
				_1: vals
			});
	});
var _user$project$JSON$encodeHoleOr = F2(
	function (v, encoder) {
		var _p5 = v;
		if (_p5.ctor === 'Full') {
			return A2(
				_user$project$JSON$encodeVariant,
				'Full',
				{
					ctor: '::',
					_0: _elm_lang$core$Json_Encode$int(_p5._0._0),
					_1: {
						ctor: '::',
						_0: encoder(_p5._1),
						_1: {ctor: '[]'}
					}
				});
		} else {
			return A2(
				_user$project$JSON$encodeVariant,
				'Empty',
				{
					ctor: '::',
					_0: _elm_lang$core$Json_Encode$int(_p5._0._0),
					_1: {ctor: '[]'}
				});
		}
	});

var _user$project$RPC$decodeTipe = A2(
	_elm_lang$core$Json_Decode$map,
	_elm_lang$core$String$dropLeft(1),
	A2(_elm_lang$core$Json_Decode$index, 0, _elm_lang$core$Json_Decode$string));
var _user$project$RPC$decodeDB = function () {
	var toDB = F2(
		function (name, cols) {
			return {name: name, cols: cols};
		});
	return A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'cols',
		_elm_lang$core$Json_Decode$list(
			A2(
				_user$project$JSON$decodePair,
				_user$project$JSON$decodeHoleOr(_elm_lang$core$Json_Decode$string),
				_user$project$JSON$decodeHoleOr(_user$project$RPC$decodeTipe))),
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'display_name',
			_elm_lang$core$Json_Decode$string,
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(toDB)));
}();
var _user$project$RPC$decodeHandlerSpec = function () {
	var toHS = F3(
		function (module_, name, modifier) {
			return {name: name, module_: module_, modifier: modifier};
		});
	return A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'modifier',
		_user$project$JSON$decodeHoleOr(_elm_lang$core$Json_Decode$string),
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'name',
			_user$project$JSON$decodeHoleOr(_elm_lang$core$Json_Decode$string),
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'module',
				_user$project$JSON$decodeHoleOr(_elm_lang$core$Json_Decode$string),
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(toHS))));
}();
var _user$project$RPC$decodeLiveValue = function () {
	var toExc = function ($short) {
		return function ($long) {
			return function (tipe) {
				return function (actual) {
					return function (actualType) {
						return function (result) {
							return function (resultType) {
								return function (expected) {
									return function (info) {
										return function (workarounds) {
											return _elm_lang$core$Maybe$Just(
												{$short: $short, $long: $long, tipe: tipe, actual: actual, actualType: actualType, result: result, resultType: resultType, expected: expected, info: info, workarounds: workarounds});
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
	var toLiveValue = F4(
		function (value, tipe, json, exc) {
			return {
				value: value,
				tipe: _user$project$Runtime$str2tipe(tipe),
				json: json,
				exc: exc
			};
		});
	return A4(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional,
		'exc',
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'workarounds',
			_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string),
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'info',
				_elm_lang$core$Json_Decode$dict(_elm_lang$core$Json_Decode$string),
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
					'expected',
					_elm_lang$core$Json_Decode$string,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'result_tipe',
						_elm_lang$core$Json_Decode$string,
						A3(
							_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
							'result',
							_elm_lang$core$Json_Decode$string,
							A3(
								_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
								'actual_tipe',
								_elm_lang$core$Json_Decode$string,
								A3(
									_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
									'actual',
									_elm_lang$core$Json_Decode$string,
									A3(
										_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
										'tipe',
										_elm_lang$core$Json_Decode$string,
										A3(
											_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
											'long',
											_elm_lang$core$Json_Decode$string,
											A3(
												_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
												'short',
												_elm_lang$core$Json_Decode$string,
												_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(toExc))))))))))),
		_elm_lang$core$Maybe$Nothing,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'json',
			_elm_lang$core$Json_Decode$string,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'type',
				_elm_lang$core$Json_Decode$string,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
					'value',
					_elm_lang$core$Json_Decode$string,
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(toLiveValue)))));
}();
var _user$project$RPC$decodeTLAResult = function () {
	var toTLAResult = F4(
		function (tlid, astValue, liveValues, availableVarnames) {
			return {
				id: _user$project$Types$TLID(tlid),
				astValue: astValue,
				liveValues: A2(
					_elm_community$dict_extra$Dict_Extra$mapKeys,
					_user$project$Util$toIntWithDefault(0),
					liveValues),
				availableVarnames: A2(
					_elm_community$dict_extra$Dict_Extra$mapKeys,
					_user$project$Util$toIntWithDefault(0),
					availableVarnames)
			};
		});
	return A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'available_varnames',
		_elm_lang$core$Json_Decode$dict(
			_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string)),
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'live_values',
			_elm_lang$core$Json_Decode$dict(_user$project$RPC$decodeLiveValue),
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'ast_value',
				_user$project$RPC$decodeLiveValue,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
					'id',
					_elm_lang$core$Json_Decode$int,
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(toTLAResult)))));
}();
var _user$project$RPC$decodeExpr = function () {
	var dv1 = _user$project$JSON$decodeVariant1;
	var dv2 = _user$project$JSON$decodeVariant2;
	var dv3 = _user$project$JSON$decodeVariant3;
	var dv4 = _user$project$JSON$decodeVariant4;
	var did = _user$project$JSON$decodeID;
	var de = _elm_lang$core$Json_Decode$lazy(
		function (_p0) {
			return _user$project$RPC$decodeExpr;
		});
	return _user$project$JSON$decodeVariants(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'Let',
				_1: A5(
					dv4,
					_user$project$Types$Let,
					did,
					_user$project$JSON$decodeHoleOr(_elm_lang$core$Json_Decode$string),
					de,
					de)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'Hole',
					_1: A2(dv1, _user$project$Types$Hole, did)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'Value',
						_1: A3(dv2, _user$project$Types$Value, did, _elm_lang$core$Json_Decode$string)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'If',
							_1: A5(dv4, _user$project$Types$If, did, de, de, de)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'FnCall',
								_1: A4(
									dv3,
									_user$project$Types$FnCall,
									did,
									_elm_lang$core$Json_Decode$string,
									_elm_lang$core$Json_Decode$list(de))
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'Lambda',
									_1: A4(
										dv3,
										_user$project$Types$Lambda,
										did,
										_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string),
										de)
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'Variable',
										_1: A3(dv2, _user$project$Types$Variable, did, _elm_lang$core$Json_Decode$string)
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: 'Thread',
											_1: A3(
												dv2,
												_user$project$Types$Thread,
												did,
												_elm_lang$core$Json_Decode$list(de))
										},
										_1: {
											ctor: '::',
											_0: {
												ctor: '_Tuple2',
												_0: 'FieldAccess',
												_1: A4(
													dv3,
													_user$project$Types$FieldAccess,
													did,
													de,
													_user$project$JSON$decodeHoleOr(_elm_lang$core$Json_Decode$string))
											},
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}
			}
		});
}();
var _user$project$RPC$decodeAST = _user$project$RPC$decodeExpr;
var _user$project$RPC$decodeHandler = function () {
	var toHandler = F2(
		function (ast, spec) {
			return {ast: ast, spec: spec};
		});
	return A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'spec',
		_user$project$RPC$decodeHandlerSpec,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'ast',
			_user$project$RPC$decodeAST,
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(toHandler)));
}();
var _user$project$RPC$decodeToplevel = function () {
	var variant = _user$project$JSON$decodeVariants(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'Handler',
				_1: A2(_user$project$JSON$decodeVariant1, _user$project$Types$TLHandler, _user$project$RPC$decodeHandler)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'DB',
					_1: A2(_user$project$JSON$decodeVariant1, _user$project$Types$TLDB, _user$project$RPC$decodeDB)
				},
				_1: {ctor: '[]'}
			}
		});
	var toToplevel = F4(
		function (id, x, y, data) {
			return {
				id: id,
				pos: {x: x, y: y},
				data: data
			};
		});
	return A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'data',
		variant,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
			{
				ctor: '::',
				_0: 'pos',
				_1: {
					ctor: '::',
					_0: 'y',
					_1: {ctor: '[]'}
				}
			},
			_elm_lang$core$Json_Decode$int,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
				{
					ctor: '::',
					_0: 'pos',
					_1: {
						ctor: '::',
						_0: 'x',
						_1: {ctor: '[]'}
					}
				},
				_elm_lang$core$Json_Decode$int,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
					'tlid',
					_user$project$JSON$decodeTLID,
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(toToplevel)))));
}();
var _user$project$RPC$decodeRPC = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'analyses',
	_elm_lang$core$Json_Decode$list(_user$project$RPC$decodeTLAResult),
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'toplevels',
		_elm_lang$core$Json_Decode$list(_user$project$RPC$decodeToplevel),
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}))));
var _user$project$RPC$encodeAST = function (expr) {
	var ev = _user$project$JSON$encodeVariant;
	var eid = _user$project$JSON$encodeID;
	var e = _user$project$RPC$encodeAST;
	var _p1 = expr;
	switch (_p1.ctor) {
		case 'FnCall':
			return A2(
				ev,
				'FnCall',
				{
					ctor: '::',
					_0: eid(_p1._0),
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$string(_p1._1),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$list(
								A2(_elm_lang$core$List$map, e, _p1._2)),
							_1: {ctor: '[]'}
						}
					}
				});
		case 'Let':
			return A2(
				ev,
				'Let',
				{
					ctor: '::',
					_0: eid(_p1._0),
					_1: {
						ctor: '::',
						_0: A2(_user$project$JSON$encodeHoleOr, _p1._1, _elm_lang$core$Json_Encode$string),
						_1: {
							ctor: '::',
							_0: e(_p1._2),
							_1: {
								ctor: '::',
								_0: e(_p1._3),
								_1: {ctor: '[]'}
							}
						}
					}
				});
		case 'Lambda':
			return A2(
				ev,
				'Lambda',
				{
					ctor: '::',
					_0: eid(_p1._0),
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p1._1)),
						_1: {
							ctor: '::',
							_0: e(_p1._2),
							_1: {ctor: '[]'}
						}
					}
				});
		case 'If':
			return A2(
				ev,
				'If',
				{
					ctor: '::',
					_0: eid(_p1._0),
					_1: {
						ctor: '::',
						_0: e(_p1._1),
						_1: {
							ctor: '::',
							_0: e(_p1._2),
							_1: {
								ctor: '::',
								_0: e(_p1._3),
								_1: {ctor: '[]'}
							}
						}
					}
				});
		case 'Variable':
			return A2(
				ev,
				'Variable',
				{
					ctor: '::',
					_0: eid(_p1._0),
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$string(_p1._1),
						_1: {ctor: '[]'}
					}
				});
		case 'Value':
			return A2(
				ev,
				'Value',
				{
					ctor: '::',
					_0: eid(_p1._0),
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$string(_p1._1),
						_1: {ctor: '[]'}
					}
				});
		case 'Hole':
			return A2(
				ev,
				'Hole',
				{
					ctor: '::',
					_0: eid(_p1._0),
					_1: {ctor: '[]'}
				});
		case 'Thread':
			return A2(
				ev,
				'Thread',
				{
					ctor: '::',
					_0: eid(_p1._0),
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, e, _p1._1)),
						_1: {ctor: '[]'}
					}
				});
		default:
			return A2(
				ev,
				'FieldAccess',
				{
					ctor: '::',
					_0: eid(_p1._0),
					_1: {
						ctor: '::',
						_0: e(_p1._1),
						_1: {
							ctor: '::',
							_0: A2(_user$project$JSON$encodeHoleOr, _p1._2, _elm_lang$core$Json_Encode$string),
							_1: {ctor: '[]'}
						}
					}
				});
	}
};
var _user$project$RPC$encodeRPC = F2(
	function (m, call) {
		var ev = _user$project$JSON$encodeVariant;
		var encodePos = function (_p2) {
			var _p3 = _p2;
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'x',
						_1: _elm_lang$core$Json_Encode$int(_p3.x)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'y',
							_1: _elm_lang$core$Json_Encode$int(_p3.y)
						},
						_1: {ctor: '[]'}
					}
				});
		};
		var _p4 = call;
		switch (_p4.ctor) {
			case 'SetHandler':
				var _p6 = _p4._0;
				var _p5 = _p4._2;
				var hs = _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'name',
							_1: A2(_user$project$JSON$encodeHoleOr, _p5.spec.name, _elm_lang$core$Json_Encode$string)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'module',
								_1: A2(_user$project$JSON$encodeHoleOr, _p5.spec.module_, _elm_lang$core$Json_Encode$string)
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'modifier',
									_1: A2(_user$project$JSON$encodeHoleOr, _p5.spec.modifier, _elm_lang$core$Json_Encode$string)
								},
								_1: {ctor: '[]'}
							}
						}
					});
				var handler = _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'tlid',
							_1: _user$project$JSON$encodeTLID(_p6)
						},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'spec', _1: hs},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'ast',
									_1: _user$project$RPC$encodeAST(_p5.ast)
								},
								_1: {ctor: '[]'}
							}
						}
					});
				return A2(
					ev,
					'SetHandler',
					{
						ctor: '::',
						_0: _user$project$JSON$encodeTLID(_p6),
						_1: {
							ctor: '::',
							_0: encodePos(_p4._1),
							_1: {
								ctor: '::',
								_0: handler,
								_1: {ctor: '[]'}
							}
						}
					});
			case 'CreateDB':
				return A2(
					ev,
					'CreateDB',
					{
						ctor: '::',
						_0: _user$project$JSON$encodeTLID(_p4._0),
						_1: {
							ctor: '::',
							_0: encodePos(_p4._1),
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Json_Encode$string(_p4._2),
								_1: {ctor: '[]'}
							}
						}
					});
			case 'AddDBCol':
				return A2(
					ev,
					'AddDBCol',
					{
						ctor: '::',
						_0: _user$project$JSON$encodeTLID(_p4._0),
						_1: {
							ctor: '::',
							_0: _user$project$JSON$encodeID(_p4._1),
							_1: {
								ctor: '::',
								_0: _user$project$JSON$encodeID(_p4._2),
								_1: {ctor: '[]'}
							}
						}
					});
			case 'SetDBColName':
				return A2(
					ev,
					'SetDBColName',
					{
						ctor: '::',
						_0: _user$project$JSON$encodeTLID(_p4._0),
						_1: {
							ctor: '::',
							_0: _user$project$JSON$encodeID(_p4._1),
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Json_Encode$string(_p4._2),
								_1: {ctor: '[]'}
							}
						}
					});
			case 'SetDBColType':
				return A2(
					ev,
					'SetDBColType',
					{
						ctor: '::',
						_0: _user$project$JSON$encodeTLID(_p4._0),
						_1: {
							ctor: '::',
							_0: _user$project$JSON$encodeID(_p4._1),
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Json_Encode$string(_p4._2),
								_1: {ctor: '[]'}
							}
						}
					});
			case 'NoOp':
				return A2(
					ev,
					'NoOp',
					{ctor: '[]'});
			case 'DeleteAll':
				return A2(
					ev,
					'DeleteAll',
					{ctor: '[]'});
			case 'Savepoint':
				return A2(
					ev,
					'Savepoint',
					{ctor: '[]'});
			case 'Undo':
				return A2(
					ev,
					'Undo',
					{ctor: '[]'});
			case 'Redo':
				return A2(
					ev,
					'Redo',
					{ctor: '[]'});
			case 'DeleteTL':
				return A2(
					ev,
					'DeleteTL',
					{
						ctor: '::',
						_0: _user$project$JSON$encodeTLID(_p4._0),
						_1: {ctor: '[]'}
					});
			default:
				return A2(
					ev,
					'MoveTL',
					{
						ctor: '::',
						_0: _user$project$JSON$encodeTLID(_p4._0),
						_1: {
							ctor: '::',
							_0: encodePos(_p4._1),
							_1: {ctor: '[]'}
						}
					});
		}
	});
var _user$project$RPC$encodeRPCs = F2(
	function (m, calls) {
		return _elm_lang$core$Json_Encode$list(
			A2(
				_elm_lang$core$List$map,
				_user$project$RPC$encodeRPC(m),
				function (cs) {
					return (_elm_lang$core$Native_Utils.eq(
						cs,
						{
							ctor: '::',
							_0: _user$project$Types$Undo,
							_1: {ctor: '[]'}
						}) || (_elm_lang$core$Native_Utils.eq(
						cs,
						{
							ctor: '::',
							_0: _user$project$Types$Redo,
							_1: {ctor: '[]'}
						}) || _elm_lang$core$Native_Utils.eq(
						cs,
						{ctor: '[]'}))) ? cs : {ctor: '::', _0: _user$project$Types$Savepoint, _1: cs};
				}(
					A2(
						_elm_lang$core$List$filter,
						F2(
							function (x, y) {
								return !_elm_lang$core$Native_Utils.eq(x, y);
							})(_user$project$Types$NoOp),
						calls))));
	});
var _user$project$RPC$postString = function (url) {
	return _elm_lang$http$Http$request(
		{
			method: 'POST',
			headers: {ctor: '[]'},
			url: url,
			body: _elm_lang$http$Http$emptyBody,
			expect: _elm_lang$http$Http$expectString,
			timeout: _elm_lang$core$Maybe$Nothing,
			withCredentials: false
		});
};
var _user$project$RPC$saveTest = function () {
	var url = '/admin/api/save_test';
	var request = _user$project$RPC$postString(url);
	return A2(_elm_lang$http$Http$send, _user$project$Types$SaveTestCallBack, request);
}();
var _user$project$RPC$rpc_ = F4(
	function (m, url, callback, calls) {
		var payload = A2(_user$project$RPC$encodeRPCs, m, calls);
		var json = _elm_lang$http$Http$jsonBody(payload);
		var request = A3(_elm_lang$http$Http$post, url, json, _user$project$RPC$decodeRPC);
		return A2(
			_elm_lang$http$Http$send,
			callback(calls),
			request);
	});
var _user$project$RPC$integrationRpc = F2(
	function (m, name) {
		return A4(
			_user$project$RPC$rpc_,
			m,
			'/admin/api/rpc',
			A2(
				_user$project$Types$RPCCallBack,
				_user$project$Types$FocusNothing,
				_user$project$Types$TriggerIntegrationTest(name)),
			{ctor: '[]'});
	});
var _user$project$RPC$rpc = F3(
	function (m, focus, calls) {
		return A4(
			_user$project$RPC$rpc_,
			m,
			'/admin/api/rpc',
			A2(_user$project$Types$RPCCallBack, focus, _user$project$Types$NoChange),
			calls);
	});

var _user$project$ViewAST$vVarBind = function (v) {
	var _p0 = v;
	if (_p0.ctor === 'Full') {
		return _user$project$Types$Leaf(
			{
				ctor: '_Tuple3',
				_0: _elm_lang$core$Maybe$Just(_p0._0),
				_1: 'varname atom',
				_2: _p0._1
			});
	} else {
		return _user$project$Types$Leaf(
			{
				ctor: '_Tuple3',
				_0: _elm_lang$core$Maybe$Just(_p0._0),
				_1: 'hole atom',
				_2: '＿＿＿＿＿＿'
			});
	}
};
var _user$project$ViewAST$vVarname = F2(
	function (mId, v) {
		return _user$project$Types$Leaf(
			{ctor: '_Tuple3', _0: mId, _1: 'varname atom', _2: v});
	});
var _user$project$ViewAST$vFn = function (name) {
	var _p1 = A2(_elm_lang$core$String$split, '::', name);
	if (((_p1.ctor === '::') && (_p1._1.ctor === '::')) && (_p1._1._1.ctor === '[]')) {
		return A2(
			_user$project$Types$Nested,
			{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'namegroup atom'},
			{
				ctor: '::',
				_0: _user$project$Types$Leaf(
					{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'module', _2: _p1._0}),
				_1: {
					ctor: '::',
					_0: _user$project$Types$Leaf(
						{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'moduleseparator', _2: '::'}),
					_1: {
						ctor: '::',
						_0: _user$project$Types$Leaf(
							{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'fnname', _2: _p1._1._0}),
						_1: {ctor: '[]'}
					}
				}
			});
	} else {
		return _user$project$Types$Leaf(
			{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'fnname atom', _2: name});
	}
};
var _user$project$ViewAST$depthString = function (n) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'precedence-',
		_elm_lang$core$Basics$toString(n));
};
var _user$project$ViewAST$vInfix = F4(
	function (id, name, exprs, nesting) {
		var _p2 = exprs;
		if (((_p2.ctor === '::') && (_p2._1.ctor === '::')) && (_p2._1._1.ctor === '[]')) {
			return A2(
				_user$project$Types$Nested,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$Maybe$Just(id),
					_1: A2(
						_elm_lang$core$Basics_ops['++'],
						'fncall infix ',
						_user$project$ViewAST$depthString(nesting))
				},
				{
					ctor: '::',
					_0: A2(
						_user$project$Types$Nested,
						{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'lhs'},
						{
							ctor: '::',
							_0: A2(_user$project$ViewAST$vExpr, nesting + 1, _p2._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_user$project$Types$Nested,
							{
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Nothing,
								_1: A2(_elm_lang$core$Basics_ops['++'], 'op ', name)
							},
							{
								ctor: '::',
								_0: _user$project$ViewAST$vFn(name),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_user$project$Types$Nested,
								{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'rhs'},
								{
									ctor: '::',
									_0: A2(_user$project$ViewAST$vExpr, nesting, _p2._1._0),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}
				});
		} else {
			return A4(
				_user$project$ViewAST$vPrefix,
				id,
				A2(
					_elm_lang$core$Basics_ops['++'],
					'(',
					A2(_elm_lang$core$Basics_ops['++'], name, ')')),
				exprs,
				nesting);
		}
	});
var _user$project$ViewAST$vExpr = F2(
	function (nest, expr) {
		var _p3 = expr;
		switch (_p3.ctor) {
			case 'Value':
				var _p4 = _p3._1;
				var valu = _user$project$Runtime$isString(_p4) ? A2(
					_elm_lang$core$Basics_ops['++'],
					'“',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_community$string_extra$String_Extra$unquote(_p4),
						'”')) : _p4;
				var cssClass = _elm_lang$core$String$toLower(
					_elm_lang$core$Basics$toString(
						_user$project$Runtime$tipeOf(_p4)));
				return _user$project$Types$Leaf(
					{
						ctor: '_Tuple3',
						_0: _elm_lang$core$Maybe$Just(_p3._0),
						_1: A2(_elm_lang$core$Basics_ops['++'], 'atom value ', cssClass),
						_2: valu
					});
			case 'Let':
				return A2(
					_user$project$Types$Nested,
					{
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(_p3._0),
						_1: 'letexpr'
					},
					{
						ctor: '::',
						_0: _user$project$Types$Leaf(
							{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'let keyword atom', _2: 'let'}),
						_1: {
							ctor: '::',
							_0: A2(
								_user$project$Types$Nested,
								{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'letbinding'},
								{
									ctor: '::',
									_0: _user$project$ViewAST$vVarBind(_p3._1),
									_1: {
										ctor: '::',
										_0: _user$project$Types$Leaf(
											{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'letbind atom', _2: '='}),
										_1: {
											ctor: '::',
											_0: A2(_user$project$ViewAST$vExpr, nest, _p3._2),
											_1: {ctor: '[]'}
										}
									}
								}),
							_1: {
								ctor: '::',
								_0: _user$project$Types$Leaf(
									{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'in keyword atom', _2: 'in'}),
								_1: {
									ctor: '::',
									_0: A2(
										_user$project$Types$Nested,
										{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'letbody'},
										{
											ctor: '::',
											_0: A2(_user$project$ViewAST$vExpr, nest, _p3._3),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}
							}
						}
					});
			case 'If':
				return A2(
					_user$project$Types$Nested,
					{
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(_p3._0),
						_1: 'ifexpr'
					},
					{
						ctor: '::',
						_0: _user$project$Types$Leaf(
							{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'if keyword atom', _2: 'if'}),
						_1: {
							ctor: '::',
							_0: A2(
								_user$project$Types$Nested,
								{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'cond'},
								{
									ctor: '::',
									_0: A2(_user$project$ViewAST$vExpr, nest + 1, _p3._1),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_user$project$Types$Nested,
									{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'ifbody'},
									{
										ctor: '::',
										_0: A2(_user$project$ViewAST$vExpr, 0, _p3._2),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: _user$project$Types$Leaf(
										{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'else keyword atom', _2: 'else'}),
									_1: {
										ctor: '::',
										_0: A2(
											_user$project$Types$Nested,
											{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'elsebody'},
											{
												ctor: '::',
												_0: A2(_user$project$ViewAST$vExpr, 0, _p3._3),
												_1: {ctor: '[]'}
											}),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					});
			case 'Variable':
				return A2(
					_user$project$ViewAST$vVarname,
					_elm_lang$core$Maybe$Just(_p3._0),
					_p3._1);
			case 'FnCall':
				var _p7 = _p3._1;
				var _p6 = _p3._0;
				var _p5 = _p3._2;
				return _user$project$AST$isInfix(_p7) ? A4(_user$project$ViewAST$vInfix, _p6, _p7, _p5, nest) : A4(_user$project$ViewAST$vPrefix, _p6, _p7, _p5, nest);
			case 'Lambda':
				return A2(
					_user$project$Types$Nested,
					{
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(_p3._0),
						_1: 'lambdaexpr'
					},
					{
						ctor: '::',
						_0: A2(
							_user$project$Types$Nested,
							{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'lambdabinding'},
							A2(
								_elm_lang$core$List$map,
								_user$project$ViewAST$vVarname(_elm_lang$core$Maybe$Nothing),
								_p3._1)),
						_1: {
							ctor: '::',
							_0: _user$project$Types$Leaf(
								{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'arrow atom', _2: '->'}),
							_1: {
								ctor: '::',
								_0: A2(
									_user$project$Types$Nested,
									{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'lambdabody'},
									{
										ctor: '::',
										_0: A2(_user$project$ViewAST$vExpr, 0, _p3._2),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						}
					});
			case 'Hole':
				return _user$project$Types$Leaf(
					{
						ctor: '_Tuple3',
						_0: _elm_lang$core$Maybe$Just(_p3._0),
						_1: 'hole atom',
						_2: '＿＿＿＿＿＿'
					});
			case 'Thread':
				return A2(
					_user$project$Types$Nested,
					{
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(_p3._0),
						_1: 'threadexpr'
					},
					A2(
						_elm_lang$core$List$intersperse,
						_user$project$Types$Leaf(
							{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'thread atom', _2: '|>'}),
						A2(
							_elm_lang$core$List$map,
							function (e) {
								return A2(
									_user$project$Types$Nested,
									{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'threadmember'},
									{
										ctor: '::',
										_0: A2(_user$project$ViewAST$vExpr, 0, e),
										_1: {ctor: '[]'}
									});
							},
							_p3._1)));
			default:
				return A2(
					_user$project$Types$Nested,
					{
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(_p3._0),
						_1: 'fieldaccessexpr'
					},
					{
						ctor: '::',
						_0: A2(
							_user$project$Types$Nested,
							{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: 'fieldobject'},
							{
								ctor: '::',
								_0: A2(_user$project$ViewAST$vExpr, 0, _p3._1),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: _user$project$Types$Leaf(
								{ctor: '_Tuple3', _0: _elm_lang$core$Maybe$Nothing, _1: 'fieldaccessop operator atom', _2: '.'}),
							_1: {
								ctor: '::',
								_0: _user$project$ViewAST$vVarBind(_p3._2),
								_1: {ctor: '[]'}
							}
						}
					});
		}
	});
var _user$project$ViewAST$vPrefix = F4(
	function (id, name, exprs, nest) {
		return A2(
			_user$project$Types$Nested,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Maybe$Just(id),
				_1: A2(
					_elm_lang$core$Basics_ops['++'],
					'fncall prefix ',
					_user$project$ViewAST$depthString(nest))
			},
			{
				ctor: '::',
				_0: A2(
					_user$project$Types$Nested,
					{
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Nothing,
						_1: A2(_elm_lang$core$Basics_ops['++'], 'op ', name)
					},
					{
						ctor: '::',
						_0: _user$project$ViewAST$vFn(name),
						_1: {ctor: '[]'}
					}),
				_1: A2(
					_elm_lang$core$List$map,
					_user$project$ViewAST$vExpr(nest + 1),
					exprs)
			});
	});
var _user$project$ViewAST$walk = _user$project$ViewAST$vExpr(0);
var _user$project$ViewAST$elemToHtml = F2(
	function (state, elem) {
		var hover = function (id) {
			return _elm_community$maybe_extra$Maybe_Extra$toList(
				A2(
					_elm_lang$core$Maybe$map,
					function (lv) {
						return _elm_lang$html$Html_Attributes$title(lv.value);
					},
					A2(
						_elm_lang$core$Maybe$andThen,
						function (_p8) {
							var _p9 = _p8;
							return A2(_elm_lang$core$Dict$get, _p9._0, state.liveValues);
						},
						id)));
		};
		var _p10 = elem;
		if (_p10.ctor === 'Leaf') {
			var _p13 = _p10._0._0;
			var _p12 = _p10._0._1;
			var newContent = (_elm_lang$core$Native_Utils.eq(
				_p13,
				_elm_lang$core$Maybe$Just(state.selectedID)) && state.isFilling) ? state.fillingHtml : _elm_lang$html$Html$text(_p10._0._2);
			var newClasses = (_elm_lang$core$Native_Utils.eq(
				_p13,
				_elm_lang$core$Maybe$Just(state.selectedID)) && (!state.isFilling)) ? _elm_lang$html$Html_Attributes$class(
				A2(_elm_lang$core$Basics_ops['++'], 'leaf selected ', _p12)) : _elm_lang$html$Html_Attributes$class(
				A2(_elm_lang$core$Basics_ops['++'], 'leaf ', _p12));
			var idAttrs = function () {
				var _p11 = _p13;
				if (_p11.ctor === 'Just') {
					return {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$id(
							_elm_lang$core$Basics$toString(_p11._0._0)),
						_1: {ctor: '[]'}
					};
				} else {
					return {ctor: '[]'};
				}
			}();
			return A2(
				_elm_lang$html$Html$div,
				A2(
					_elm_lang$core$Basics_ops['++'],
					{
						ctor: '::',
						_0: newClasses,
						_1: {ctor: '[]'}
					},
					A2(
						_elm_lang$core$Basics_ops['++'],
						idAttrs,
						hover(_p13))),
				{
					ctor: '::',
					_0: newContent,
					_1: {ctor: '[]'}
				});
		} else {
			var _p15 = _p10._0._0;
			var _p14 = _p10._0._1;
			var newClasses = _elm_lang$core$Native_Utils.eq(
				_p15,
				_elm_lang$core$Maybe$Just(state.selectedID)) ? _elm_lang$html$Html_Attributes$class(
				A2(_elm_lang$core$Basics_ops['++'], 'nested selected ', _p14)) : _elm_lang$html$Html_Attributes$class(
				A2(_elm_lang$core$Basics_ops['++'], 'nested ', _p14));
			return A2(
				_elm_lang$html$Html$div,
				A2(
					_elm_lang$core$Basics_ops['++'],
					{
						ctor: '::',
						_0: newClasses,
						_1: {ctor: '[]'}
					},
					hover(_p15)),
				A2(
					_elm_lang$core$List$map,
					_user$project$ViewAST$elemToHtml(state),
					_p10._1));
		}
	});
var _user$project$ViewAST$toHtml = F2(
	function (visitState, expr) {
		return A2(
			_user$project$ViewAST$elemToHtml,
			visitState,
			_user$project$ViewAST$walk(expr));
	});
var _user$project$ViewAST$HtmlVisitState = F4(
	function (a, b, c, d) {
		return {selectedID: a, isFilling: b, fillingHtml: c, liveValues: d};
	});

var _user$project$View$decodeClickEvent = function (fn) {
	var toA = F3(
		function (px, py, button) {
			return fn(
				{
					pos: {vx: px, vy: py},
					button: button
				});
		});
	return A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'button',
		_elm_lang$core$Json_Decode$int,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'pageY',
			_elm_lang$core$Json_Decode$int,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'pageX',
				_elm_lang$core$Json_Decode$int,
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(toA))));
};
var _user$project$View$svgLine = F6(
	function (m, p1a, p2a, sourcedebug, targetdebug, attrs) {
		var p2v = A2(_user$project$Viewport$toViewport, m, p2a);
		var p1v = A2(_user$project$Viewport$toViewport, m, p1a);
		return A2(
			_elm_lang$svg$Svg$line,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$x1(
						_elm_lang$core$Basics$toString(p1v.vx)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$y1(
							_elm_lang$core$Basics$toString(p1v.vy)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$x2(
								_elm_lang$core$Basics$toString(p2v.vx)),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$y2(
									_elm_lang$core$Basics$toString(p2v.vy)),
								_1: {
									ctor: '::',
									_0: A2(_elm_lang$virtual_dom$VirtualDom$attribute, 'source', sourcedebug),
									_1: {
										ctor: '::',
										_0: A2(_elm_lang$virtual_dom$VirtualDom$attribute, 'target', targetdebug),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				},
				attrs),
			{ctor: '[]'});
	});
var _user$project$View$placeHtml = F3(
	function (m, pos, html) {
		var rcpos = A2(_user$project$Viewport$toViewport, m, pos);
		return A2(
			_elm_lang$svg$Svg$foreignObject,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$x(
					_elm_lang$core$Basics$toString(rcpos.vx)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$y(
						_elm_lang$core$Basics$toString(rcpos.vy)),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: html,
				_1: {ctor: '[]'}
			});
	});
var _user$project$View$escapeCSSName = function (s) {
	return A3(_user$project$Util$replace, '[^0-9a-zA-Z_-]', '_', s);
};
var _user$project$View$prefixify = function (hs) {
	var _p0 = hs;
	if (_p0.ctor === '[]') {
		return hs;
	} else {
		if (_p0._1.ctor === '[]') {
			return hs;
		} else {
			var _p7 = _p0._1;
			var _p6 = _p0._0;
			var _p1 = _p6.name;
			if (_p1.ctor === 'Nothing') {
				return {
					ctor: '::',
					_0: _p6,
					_1: _user$project$View$prefixify(_p7)
				};
			} else {
				var _p5 = _p1._0;
				var isPrefixOf = function (h2) {
					var _p2 = h2.name;
					if (_p2.ctor === 'Nothing') {
						return false;
					} else {
						return A2(_elm_lang$core$String$startsWith, _p5, _p2._0);
					}
				};
				var len = _elm_lang$core$String$length(_p5);
				var makePrefix = function (h2) {
					var _p3 = h2.name;
					if (_p3.ctor === 'Just') {
						var newName = A2(_elm_lang$core$String$dropLeft, len, _p3._0);
						return _elm_lang$core$Native_Utils.update(
							h2,
							{
								name: _elm_lang$core$Maybe$Just(newName),
								prefix: A2(
									_elm_lang$core$Basics_ops['++'],
									_p6.prefix,
									{
										ctor: '::',
										_0: _p5,
										_1: {ctor: '[]'}
									})
							});
					} else {
						return h2;
					}
				};
				var _p4 = A2(
					_elm_community$list_extra$List_Extra$splitWhen,
					function (h2) {
						return !isPrefixOf(h2);
					},
					_p7);
				if (_p4.ctor === 'Nothing') {
					return {
						ctor: '::',
						_0: _p6,
						_1: _user$project$View$prefixify(
							A2(_elm_lang$core$List$map, makePrefix, _p7))
					};
				} else {
					return {
						ctor: '::',
						_0: _p6,
						_1: _user$project$View$prefixify(
							A2(
								_elm_lang$core$Basics_ops['++'],
								A2(_elm_lang$core$List$map, makePrefix, _p4._0._0),
								_p4._0._1))
					};
				}
			}
		}
	}
};
var _user$project$View$collapseHandlers = function (handlers) {
	var asCollapsed = A2(
		_elm_lang$core$List$sortBy,
		function (c) {
			return A2(_elm_lang$core$Maybe$withDefault, 'ZZZZZZ', c.name);
		},
		A2(
			_elm_lang$core$List$map,
			function (h) {
				return {
					name: function () {
						var _p8 = h.spec.name;
						if (_p8.ctor === 'Full') {
							return _elm_lang$core$Maybe$Just(_p8._1);
						} else {
							return _elm_lang$core$Maybe$Nothing;
						}
					}(),
					prefix: {ctor: '[]'},
					verbs: function () {
						var _p9 = h.spec.modifier;
						if (_p9.ctor === 'Full') {
							return {
								ctor: '::',
								_0: _p9._1,
								_1: {ctor: '[]'}
							};
						} else {
							return {ctor: '[]'};
						}
					}()
				};
			},
			handlers));
	return _user$project$View$prefixify(
		A3(
			_elm_lang$core$List$foldr,
			F2(
				function (curr, list) {
					var _p10 = list;
					if (_p10.ctor === '[]') {
						return {
							ctor: '::',
							_0: curr,
							_1: {ctor: '[]'}
						};
					} else {
						var _p12 = _p10._1;
						var _p11 = _p10._0;
						if (_elm_lang$core$Native_Utils.eq(_p11.name, curr.name)) {
							var $new = _elm_lang$core$Native_Utils.update(
								_p11,
								{
									verbs: A2(_elm_lang$core$Basics_ops['++'], _p11.verbs, curr.verbs)
								});
							return {ctor: '::', _0: $new, _1: _p12};
						} else {
							return {
								ctor: '::',
								_0: curr,
								_1: {ctor: '::', _0: _p11, _1: _p12}
							};
						}
					}
				}),
			{ctor: '[]'},
			asCollapsed));
};
var _user$project$View$viewRoutingTable = function (m) {
	var link = function (h) {
		if (A2(_elm_lang$core$List$member, 'GET', h.verbs)) {
			var _p13 = h.name;
			if (_p13.ctor === 'Just') {
				var source = A2(
					_elm_lang$core$String$join,
					'',
					A2(
						_elm_lang$core$Basics_ops['++'],
						h.prefix,
						{
							ctor: '::',
							_0: _p13._0,
							_1: {ctor: '[]'}
						}));
				return A2(
					_elm_lang$html$Html$a,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('external'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$href(source),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$target('_blank'),
								_1: {ctor: '[]'}
							}
						}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$i,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('fa fa-external-link'),
								_1: {ctor: '[]'}
							},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					});
			} else {
				return A2(
					_elm_lang$html$Html$div,
					{ctor: '[]'},
					{ctor: '[]'});
			}
		} else {
			return A2(
				_elm_lang$html$Html$div,
				{ctor: '[]'},
				{ctor: '[]'});
		}
	};
	var def = function (s) {
		return A2(_elm_lang$core$Maybe$withDefault, '<not entered>', s);
	};
	var handlers = _user$project$View$collapseHandlers(
		_user$project$Toplevel$handlers(m.toplevels));
	var handlerCount = _elm_lang$core$List$length(handlers);
	var div = F2(
		function ($class, subs) {
			return A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class($class),
					_1: {ctor: '[]'}
				},
				subs);
		});
	var span = F2(
		function ($class, subs) {
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class($class),
					_1: {ctor: '[]'}
				},
				subs);
		});
	var text = F2(
		function ($class, msg) {
			return A2(
				span,
				$class,
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(msg),
					_1: {ctor: '[]'}
				});
		});
	var missing = A2(text, 'no-handlers', 'No HTTP handlers yet');
	var header = A2(
		div,
		'header',
		{
			ctor: '::',
			_0: A2(text, 'http', 'HTTP'),
			_1: {
				ctor: '::',
				_0: A2(text, 'parens', '('),
				_1: {
					ctor: '::',
					_0: A2(
						text,
						'count',
						_elm_lang$core$Basics$toString(handlerCount)),
					_1: {
						ctor: '::',
						_0: A2(text, 'parens', ')'),
						_1: {ctor: '[]'}
					}
				}
			}
		});
	var handlerHtml = function (h) {
		return A2(
			div,
			'handler',
			{
				ctor: '::',
				_0: A2(
					div,
					'name',
					A2(
						_elm_lang$core$Basics_ops['++'],
						A2(
							_elm_lang$core$List$map,
							text('p'),
							h.prefix),
						{
							ctor: '::',
							_0: A2(
								text,
								'n',
								def(h.name)),
							_1: {ctor: '[]'}
						})),
				_1: {
					ctor: '::',
					_0: link(h),
					_1: {
						ctor: '::',
						_0: A2(
							span,
							'verbs',
							A2(
								_elm_lang$core$List$map,
								text('verb'),
								h.verbs)),
						_1: {ctor: '[]'}
					}
				}
			});
	};
	var routes = A2(
		div,
		'routes',
		A2(_elm_lang$core$List$map, handlerHtml, handlers));
	var html = A2(
		div,
		'routing-table',
		{
			ctor: '::',
			_0: header,
			_1: {
				ctor: '::',
				_0: routes,
				_1: {ctor: '[]'}
			}
		});
	return A3(
		_user$project$View$placeHtml,
		m,
		{x: 0, y: 0},
		_elm_lang$core$Native_Utils.eq(
			handlers,
			{ctor: '[]'}) ? missing : html);
};
var _user$project$View$normalEntryHtml = function (m) {
	var _p14 = A2(_user$project$Autocomplete$compareSuggestionWithActual, m.complete, m.complete.value);
	var indent = _p14._0;
	var suggestion = _p14._1;
	var search = _p14._2;
	var indentHtml = A2(
		_elm_lang$core$Basics_ops['++'],
		'<span style=\"font-family:sans-serif; font-size:14px;\">',
		A2(_elm_lang$core$Basics_ops['++'], indent, '</span>'));
	var _p15 = _user$project$Util$htmlSize(indentHtml);
	var width = _p15._0;
	var w = A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(width),
		'px');
	var searchInput = A2(
		_elm_lang$html$Html$input,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id(_user$project$Defaults$entryID),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onInput(_user$project$Types$EntryInputMsg),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'text-indent', _1: w},
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$value(search),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$spellcheck(false),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$autocomplete(false),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}
		},
		{ctor: '[]'});
	var suggestionInput = A2(
		_elm_lang$html$Html$input,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id('suggestionBox'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$disabled(true),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$value(suggestion),
					_1: {ctor: '[]'}
				}
			}
		},
		{ctor: '[]'});
	var input = A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id('search-container'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: searchInput,
			_1: {
				ctor: '::',
				_0: suggestionInput,
				_1: {ctor: '[]'}
			}
		});
	var autocompleteList = A2(
		_elm_lang$core$List$indexedMap,
		F2(
			function (i, item) {
				var types = A2(
					_elm_lang$html$Html$span,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('types'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_user$project$Autocomplete$asTypeString(item)),
						_1: {ctor: '[]'}
					});
				var str = _user$project$Autocomplete$asName(item);
				var name = A2(
					_elm_lang$html$Html$span,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(str),
						_1: {ctor: '[]'}
					});
				var highlighted = _elm_lang$core$Native_Utils.eq(m.complete.index, i);
				var hlClass = highlighted ? ' highlighted' : '';
				var $class = A2(_elm_lang$core$Basics_ops['++'], 'autocomplete-item', hlClass);
				return A2(
					_elm_lang$html$Html$li,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class($class),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: name,
						_1: {
							ctor: '::',
							_0: types,
							_1: {ctor: '[]'}
						}
					});
			}),
		m.complete.completions);
	var autocompletions = function () {
		var _p16 = {ctor: '_Tuple2', _0: m.state, _1: m.complete.index};
		return autocompleteList;
	}();
	var autocomplete = A2(
		_elm_lang$html$Html$ul,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id('autocomplete-holder'),
			_1: {ctor: '[]'}
		},
		autocompletions);
	var viewForm = A2(
		_elm_lang$html$Html$form,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Events$onSubmit(_user$project$Types$EntrySubmitMsg),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: input,
			_1: {
				ctor: '::',
				_0: autocomplete,
				_1: {ctor: '[]'}
			}
		});
	var wrapper = A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('entry'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$width(100),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: viewForm,
			_1: {ctor: '[]'}
		});
	return wrapper;
};
var _user$project$View$transformFromStringEntry = function (s) {
	var s2 = A3(_user$project$Util$replace, '\"', '\\\"', s);
	return A2(
		_elm_lang$core$Debug$log,
		'fromStringEntry',
		A2(
			_elm_lang$core$Basics_ops['++'],
			'\"',
			A2(_elm_lang$core$Basics_ops['++'], s2, '\"')));
};
var _user$project$View$transformToStringEntry = function (s_) {
	var s = A2(_elm_lang$core$String$endsWith, '\"', s_) ? s_ : A2(_elm_lang$core$Basics_ops['++'], s_, '\"');
	return A2(
		_elm_lang$core$Debug$log,
		'toStringEntry',
		A3(
			_user$project$Util$replace,
			'\\\\\"',
			'\"',
			A2(
				_elm_lang$core$String$dropRight,
				1,
				A2(_elm_lang$core$String$dropLeft, 1, s))));
};
var _user$project$View$stringEntryHtml = function (m) {
	var classes = 'string-entry';
	var value = _user$project$View$transformToStringEntry(m.complete.value);
	var smallInput = A2(
		_elm_lang$html$Html$input,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id(_user$project$Defaults$entryID),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onInput(
					function (_p17) {
						return _user$project$Types$EntryInputMsg(
							_user$project$View$transformFromStringEntry(_p17));
					}),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$value(value),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$spellcheck(false),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$autocomplete(false),
							_1: {ctor: '[]'}
						}
					}
				}
			}
		},
		{ctor: '[]'});
	var largeInput = A2(
		_elm_lang$html$Html$textarea,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id(_user$project$Defaults$entryID),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onInput(
					function (_p18) {
						return _user$project$Types$EntryInputMsg(
							_user$project$View$transformFromStringEntry(_p18));
					}),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$value(value),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$spellcheck(false),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$cols(50),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$rows(
									5 + A2(_elm_community$string_extra$String_Extra$countOccurrences, '\n', value)),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$autocomplete(false),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			}
		},
		{ctor: '[]'});
	var stringInput = _user$project$Autocomplete$isSmallStringEntry(m.complete) ? smallInput : largeInput;
	var input = A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('string-container'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: stringInput,
			_1: {ctor: '[]'}
		});
	var viewForm = A2(
		_elm_lang$html$Html$form,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Events$onSubmit(_user$project$Types$EntrySubmitMsg),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: input,
			_1: {ctor: '[]'}
		});
	var wrapper = A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class(classes),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$width(100),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: viewForm,
			_1: {ctor: '[]'}
		});
	return wrapper;
};
var _user$project$View$entryHtml = function (m) {
	return _user$project$Autocomplete$isStringEntry(m.complete) ? _user$project$View$stringEntryHtml(m) : _user$project$View$normalEntryHtml(m);
};
var _user$project$View$viewEntry = function (m) {
	var _p19 = m.state;
	if ((_p19.ctor === 'Entering') && (_p19._0.ctor === 'Creating')) {
		return {
			ctor: '::',
			_0: A3(
				_user$project$View$placeHtml,
				m,
				_p19._0._0,
				_user$project$View$entryHtml(m)),
			_1: {ctor: '[]'}
		};
	} else {
		return {ctor: '[]'};
	}
};
var _user$project$View$selectedFullHtml = function (s) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('selected'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(s),
			_1: {ctor: '[]'}
		});
};
var _user$project$View$unselectedHoleHtml = A2(
	_elm_lang$html$Html$div,
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('hole'),
		_1: {ctor: '[]'}
	},
	{
		ctor: '::',
		_0: _elm_lang$html$Html$text('＿＿＿＿＿＿'),
		_1: {ctor: '[]'}
	});
var _user$project$View$selectedHoleHtml = A2(
	_elm_lang$html$Html$div,
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('hole selected'),
		_1: {ctor: '[]'}
	},
	{
		ctor: '::',
		_0: _elm_lang$html$Html$text('＿＿＿＿＿＿'),
		_1: {ctor: '[]'}
	});
var _user$project$View$viewHoleOrText = F2(
	function (m, h) {
		var _p20 = h;
		if (_p20.ctor === 'Empty') {
			var _p22 = _p20._0;
			var _p21 = _user$project$Types$unwrapState(m.state);
			_v12_2:
			do {
				switch (_p21.ctor) {
					case 'Selecting':
						if (_p21._1.ctor === 'Just') {
							return _elm_lang$core$Native_Utils.eq(_p22, _p21._1._0) ? _user$project$View$selectedHoleHtml : _user$project$View$unselectedHoleHtml;
						} else {
							break _v12_2;
						}
					case 'Entering':
						if (_p21._0.ctor === 'Filling') {
							return _elm_lang$core$Native_Utils.eq(_p22, _p21._0._1) ? _user$project$View$entryHtml(m) : _user$project$View$unselectedHoleHtml;
						} else {
							break _v12_2;
						}
					default:
						break _v12_2;
				}
			} while(false);
			return _user$project$View$unselectedHoleHtml;
		} else {
			var _p25 = _p20._1;
			var _p24 = _p20._0;
			var _p23 = _user$project$Types$unwrapState(m.state);
			_v13_2:
			do {
				switch (_p23.ctor) {
					case 'Selecting':
						if (_p23._1.ctor === 'Just') {
							return _elm_lang$core$Native_Utils.eq(_p24, _p23._1._0) ? _user$project$View$selectedFullHtml(_p25) : _elm_lang$html$Html$text(_p25);
						} else {
							break _v13_2;
						}
					case 'Entering':
						if (_p23._0.ctor === 'Filling') {
							return _elm_lang$core$Native_Utils.eq(_p24, _p23._0._1) ? _user$project$View$entryHtml(m) : _elm_lang$html$Html$text(_p25);
						} else {
							break _v13_2;
						}
					default:
						break _v13_2;
				}
			} while(false);
			return _elm_lang$html$Html$text(_p25);
		}
	});
var _user$project$View$viewDB = F3(
	function (m, tl, db) {
		var coldivs = A2(
			_elm_lang$core$List$map,
			function (_p26) {
				var _p27 = _p26;
				return A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('col'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('name'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(_user$project$View$viewHoleOrText, m, _p27._0),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('type'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: A2(_user$project$View$viewHoleOrText, m, _p27._1),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					});
			},
			db.cols);
		var namediv = A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('dbname'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(db.name),
				_1: {ctor: '[]'}
			});
		return {
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('db'),
					_1: {ctor: '[]'}
				},
				{ctor: '::', _0: namediv, _1: coldivs}),
			_1: {ctor: '[]'}
		};
	});
var _user$project$View$viewHandler = F3(
	function (m, tl, h) {
		var header = A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('header'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('module'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(_user$project$View$viewHoleOrText, m, h.spec.module_),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('name'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(_user$project$View$viewHoleOrText, m, h.spec.name),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('modifier'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(_user$project$View$viewHoleOrText, m, h.spec.modifier),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				}
			});
		var lvs = A2(_user$project$Analysis$getLiveValuesDict, m, tl.id);
		var _p28 = function () {
			var _p29 = _user$project$Types$unwrapState(m.state);
			_v15_2:
			do {
				switch (_p29.ctor) {
					case 'Selecting':
						if (_p29._1.ctor === 'Just') {
							return {ctor: '_Tuple2', _0: _p29._1._0, _1: false};
						} else {
							break _v15_2;
						}
					case 'Entering':
						if (_p29._0.ctor === 'Filling') {
							return {ctor: '_Tuple2', _0: _p29._0._1, _1: true};
						} else {
							break _v15_2;
						}
					default:
						break _v15_2;
				}
			} while(false);
			return {
				ctor: '_Tuple2',
				_0: _user$project$Types$ID(0),
				_1: false
			};
		}();
		var id = _p28._0;
		var filling = _p28._1;
		var ast = A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('ast'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_user$project$ViewAST$toHtml,
					{
						selectedID: id,
						isFilling: filling,
						fillingHtml: _user$project$View$entryHtml(m),
						liveValues: lvs
					},
					h.ast),
				_1: {ctor: '[]'}
			});
		return {
			ctor: '::',
			_0: ast,
			_1: {
				ctor: '::',
				_0: header,
				_1: {ctor: '[]'}
			}
		};
	});
var _user$project$View$viewTL = F2(
	function (m, tl) {
		var $class = function () {
			var _p30 = _user$project$Types$unwrapState(m.state);
			_v16_2:
			do {
				switch (_p30.ctor) {
					case 'Selecting':
						return _elm_lang$core$Native_Utils.eq(_p30._0, tl.id) ? 'selected' : '';
					case 'Entering':
						if (_p30._0.ctor === 'Filling') {
							return _elm_lang$core$Native_Utils.eq(_p30._0._0, tl.id) ? 'selected' : '';
						} else {
							break _v16_2;
						}
					default:
						break _v16_2;
				}
			} while(false);
			return '';
		}();
		var events = {
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html_Events$on,
				'mousedown',
				_user$project$View$decodeClickEvent(
					_user$project$Types$ToplevelClickDown(tl))),
			_1: {
				ctor: '::',
				_0: A3(
					_elm_lang$html$Html_Events$onWithOptions,
					'mouseup',
					{stopPropagation: true, preventDefault: false},
					_user$project$View$decodeClickEvent(
						_user$project$Types$ToplevelClickUp(tl.id))),
				_1: {ctor: '[]'}
			}
		};
		var body = function () {
			var _p31 = tl.data;
			if (_p31.ctor === 'TLHandler') {
				return A3(_user$project$View$viewHandler, m, tl, _p31._0);
			} else {
				return A3(_user$project$View$viewDB, m, tl, _p31._0);
			}
		}();
		var html = A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class(
					A2(_elm_lang$core$Basics_ops['++'], 'toplevel ', $class)),
				_1: events
			},
			body);
		return A3(_user$project$View$placeHtml, m, tl.pos, html);
	});
var _user$project$View$viewCanvas = function (m) {
	var routing = _user$project$View$viewRoutingTable(m);
	var xaxis = A6(
		_user$project$View$svgLine,
		m,
		{x: 2000, y: 0},
		{x: -2000, y: 0},
		'',
		'',
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$strokeWidth('1px'),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$stroke('#777'),
				_1: {ctor: '[]'}
			}
		});
	var yaxis = A6(
		_user$project$View$svgLine,
		m,
		{x: 0, y: 2000},
		{x: 0, y: -2000},
		'',
		'',
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$strokeWidth('1px'),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$stroke('#777'),
				_1: {ctor: '[]'}
			}
		});
	var asts = A2(
		_elm_lang$core$List$map,
		_user$project$View$viewTL(m),
		m.toplevels);
	var entry = _user$project$View$viewEntry(m);
	var allSvgs = {
		ctor: '::',
		_0: xaxis,
		_1: {
			ctor: '::',
			_0: yaxis,
			_1: {
				ctor: '::',
				_0: routing,
				_1: A2(_elm_lang$core$Basics_ops['++'], asts, entry)
			}
		}
	};
	return allSvgs;
};
var _user$project$View$viewError = function (mMsg) {
	var _p32 = mMsg;
	if (_p32.ctor === 'Just') {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$id('darkErrors'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(_p32._0),
				_1: {ctor: '[]'}
			});
	} else {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$id('darkErrors'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text('Dark'),
				_1: {ctor: '[]'}
			});
	}
};
var _user$project$View$viewButtons = function (m) {
	var integrationTestButton = function () {
		var _p33 = m.integrationTestState;
		switch (_p33.ctor) {
			case 'IntegrationTestExpectation':
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$a,
						{
							ctor: '::',
							_0: A3(
								_elm_lang$html$Html_Events$onWithOptions,
								'mouseup',
								{stopPropagation: true, preventDefault: false},
								_user$project$View$decodeClickEvent(
									function (_p34) {
										return _user$project$Types$FinishIntegrationTest;
									})),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$src(''),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$id('finishIntegrationTest'),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('specialButton'),
										_1: {ctor: '[]'}
									}
								}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('Finish integration tests'),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			case 'IntegrationTestFinished':
				var _p35 = _p33._0;
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$id('integrationTestSignal'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class(
									_p35 ? 'success' : 'failure'),
								_1: {ctor: '[]'}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(
								_p35 ? 'success' : 'failure'),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			default:
				return {ctor: '[]'};
		}
	}();
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id('buttons'),
			_1: {ctor: '[]'}
		},
		A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$a,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onClick(_user$project$Types$AddRandom),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$src(''),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('specialButton'),
								_1: {ctor: '[]'}
							}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('Random'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$a,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onClick(_user$project$Types$ClearGraph),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$src(''),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('specialButton'),
									_1: {ctor: '[]'}
								}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('Clear'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$a,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Events$onClick(_user$project$Types$SaveTestButton),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$src(''),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('specialButton'),
										_1: {ctor: '[]'}
									}
								}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('SaveTest'),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('specialButton'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(
										_elm_lang$core$Basics$toString(m.center)),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$span,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('specialButton'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											A2(
												_elm_lang$core$Basics_ops['++'],
												'Active tests: ',
												_elm_lang$core$Basics$toString(m.tests))),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			},
			integrationTestButton));
};
var _user$project$View$view = function (m) {
	var _p36 = _user$project$Util$windowSize(
		{ctor: '_Tuple0'});
	var w = _p36._0;
	var h = _p36._1;
	var grid = A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id('grid'),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html_Events$on,
					'mouseup',
					_user$project$View$decodeClickEvent(_user$project$Types$GlobalClick)),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: _user$project$View$viewError(m.error),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$svg,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$width('100%'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$height(
								_elm_lang$core$Basics$toString(h)),
							_1: {ctor: '[]'}
						}
					},
					_user$project$View$viewCanvas(m)),
				_1: {
					ctor: '::',
					_0: _user$project$View$viewButtons(m),
					_1: {ctor: '[]'}
				}
			}
		});
	return grid;
};
var _user$project$View$Collapsed = F3(
	function (a, b, c) {
		return {name: a, prefix: b, verbs: c};
	});

var _user$project$Window_Events$onSelfMsg = F3(
	function (router, _p0, state) {
		var _p1 = _p0;
		var _p2 = A2(_elm_lang$core$Dict$get, _p1.eventName, state);
		if (_p2.ctor === 'Just') {
			var $try = function (decoder) {
				var _p3 = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, _p1.event);
				if (_p3.ctor === 'Ok') {
					return _elm_lang$core$Maybe$Just(
						A2(_elm_lang$core$Platform$sendToApp, router, _p3._0));
				} else {
					return _elm_lang$core$Maybe$Nothing;
				}
			};
			return A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Basics$always(
					_elm_lang$core$Task$succeed(state)),
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$filterMap, $try, _p2._0.decoders)));
		} else {
			return _elm_lang$core$Task$succeed(state);
		}
	});
var _user$project$Window_Events$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _user$project$Window_Events$groupByEventName = function () {
	var listify = function (value) {
		return function (_p4) {
			return _elm_lang$core$Maybe$Just(
				A2(
					_elm_lang$core$Maybe$withDefault,
					{
						ctor: '::',
						_0: value,
						_1: {ctor: '[]'}
					},
					A2(
						_elm_lang$core$Maybe$map,
						F2(
							function (x, y) {
								return {ctor: '::', _0: x, _1: y};
							})(value),
						_p4)));
		};
	};
	var go = function (_p5) {
		var _p6 = _p5;
		return A2(
			_elm_lang$core$Dict$update,
			_p6._0,
			listify(_p6._1));
	};
	return A2(_elm_lang$core$List$foldl, go, _elm_lang$core$Dict$empty);
}();
var _user$project$Window_Events$subscription = _elm_lang$core$Native_Platform.leaf('Window.Events');
var _user$project$Window_Events$Listener = F2(
	function (a, b) {
		return {decoders: a, pid: b};
	});
var _user$project$Window_Events$Msg = F2(
	function (a, b) {
		return {eventName: a, event: b};
	});
var _user$project$Window_Events$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F2(
			function (eventName, decoders) {
				return _elm_lang$core$Task$andThen(
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										eventName,
										A2(_user$project$Window_Events$Listener, decoders, pid),
										state));
							},
							_elm_lang$core$Process$spawn(
								A3(
									_elm_lang$dom$Dom_LowLevel$onWindow,
									eventName,
									_elm_lang$core$Json_Decode$value,
									function (_p7) {
										return A2(
											_elm_lang$core$Platform$sendToSelf,
											router,
											A2(_user$project$Window_Events$Msg, eventName, _p7));
									})));
					});
			});
		var bothStep = F3(
			function (eventName, _p8, decoders) {
				var _p9 = _p8;
				return _elm_lang$core$Task$map(
					A2(
						_elm_lang$core$Dict$insert,
						eventName,
						A2(_user$project$Window_Events$Listener, decoders, _p9.pid)));
			});
		var leftStep = F3(
			function (_p11, _p10, task) {
				var _p12 = _p10;
				return A2(
					_elm_lang$core$Task$andThen,
					_elm_lang$core$Basics$always(task),
					_elm_lang$core$Process$kill(_p12.pid));
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_user$project$Window_Events$groupByEventName(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _user$project$Window_Events$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _user$project$Window_Events$onWindow = F2(
	function (eventName, decoder) {
		return _user$project$Window_Events$subscription(
			A2(_user$project$Window_Events$MySub, eventName, decoder));
	});
var _user$project$Window_Events$subMap = F2(
	function (func, _p13) {
		var _p14 = _p13;
		return A2(
			_user$project$Window_Events$MySub,
			_p14._0,
			A2(_elm_lang$core$Json_Decode$map, func, _p14._1));
	});
_elm_lang$core$Native_Platform.effectManagers['Window.Events'] = {pkg: 'user/project', init: _user$project$Window_Events$init, onEffects: _user$project$Window_Events$onEffects, onSelfMsg: _user$project$Window_Events$onSelfMsg, tag: 'sub', subMap: _user$project$Window_Events$subMap};

var _user$project$VariantTesting$splitOnEquals = function (s) {
	if (A2(_elm_lang$core$String$contains, '=', s)) {
		var _p0 = A2(_elm_lang$core$String$split, '=', s);
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			if (_p0._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p2 = _p0._0;
				var _p1 = _elm_lang$core$String$toLower(
					A2(_elm_lang$core$String$join, '=', _p0._1));
				switch (_p1) {
					case 'true':
						return _elm_lang$core$Maybe$Just(
							{ctor: '_Tuple2', _0: _p2, _1: true});
					case '1':
						return _elm_lang$core$Maybe$Just(
							{ctor: '_Tuple2', _0: _p2, _1: true});
					case 'false':
						return _elm_lang$core$Maybe$Just(
							{ctor: '_Tuple2', _0: _p2, _1: false});
					default:
						return _elm_lang$core$Maybe$Nothing;
				}
			}
		}
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _user$project$VariantTesting$uniqueTests = function (xs) {
	return A2(
		_elm_community$list_extra$List_Extra$uniqueBy,
		function (x) {
			var _p3 = x;
			return 'SV';
		},
		xs);
};
var _user$project$VariantTesting$toCSSClass = function (vt) {
	var test = function () {
		var _p4 = vt;
		return 'stub';
	}();
	return A2(_elm_lang$core$Basics_ops['++'], test, '-variant');
};
var _user$project$VariantTesting$toVariantTest = function (s) {
	var _p5 = s;
	if (_p5._1 === false) {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		var _p6 = _elm_lang$core$String$toLower(_p5._0);
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _user$project$VariantTesting$variantIsActive = F2(
	function (m, vt) {
		return A2(_elm_lang$core$List$member, vt, m.tests);
	});
var _user$project$VariantTesting$parseVariantTestsFromQueryString = function (s) {
	var _p7 = _elm_lang$core$String$uncons(s);
	if (_p7.ctor === 'Just') {
		if ((_p7._0.ctor === '_Tuple2') && (_p7._0._0.valueOf() === '?')) {
			return _elm_lang$core$Maybe$Just(
				_user$project$VariantTesting$uniqueTests(
					A2(
						_elm_lang$core$List$filterMap,
						_user$project$VariantTesting$toVariantTest,
						A2(
							_elm_lang$core$List$filterMap,
							_user$project$VariantTesting$splitOnEquals,
							A2(_elm_lang$core$String$split, '&', _p7._0._1)))));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};

var _user$project$Selection$delete = F3(
	function (m, tlid, cur) {
		var tl = A2(_user$project$Toplevel$getTL, m, tlid);
		var _p0 = tl.data;
		if (_p0.ctor === 'TLHandler') {
			var _p2 = _p0._0;
			var _p1 = A2(_user$project$AST$deleteExpr, cur, _p2.ast);
			var id = _p1._0;
			var replacement = _p1._1;
			return _user$project$Types$RPC(
				{
					ctor: '_Tuple2',
					_0: {
						ctor: '::',
						_0: A3(
							_user$project$Types$SetHandler,
							tlid,
							tl.pos,
							_elm_lang$core$Native_Utils.update(
								_p2,
								{ast: replacement})),
						_1: {ctor: '[]'}
					},
					_1: A2(_user$project$Types$FocusExact, tlid, id)
				});
		} else {
			return _user$project$Types$NoChange;
		}
	});
var _user$project$Selection$nextHole = F3(
	function (m, tlid, cur) {
		var tl = A2(_user$project$Toplevel$getTL, m, tlid);
		return A2(
			_elm_lang$core$Maybe$withDefault,
			A2(
				_user$project$Types$Select,
				tlid,
				_user$project$Toplevel$firstHole(tl)),
			A2(
				_elm_lang$core$Maybe$map,
				function (h) {
					return A2(
						_user$project$Types$Select,
						tlid,
						_elm_lang$core$Maybe$Just(h));
				},
				A2(
					_user$project$Toplevel$getNextHole,
					tl,
					A2(
						_elm_lang$core$Maybe$andThen,
						function (c) {
							var _p3 = A2(_user$project$Toplevel$holeType, tl, c);
							if (_p3.ctor === 'NotAHole') {
								return _elm_lang$core$Maybe$Nothing;
							} else {
								return _elm_lang$core$Maybe$Just(c);
							}
						},
						cur))));
	});
var _user$project$Selection$previousSibling = F3(
	function (m, tlid, cur) {
		var tl = A2(_user$project$Toplevel$getTL, m, tlid);
		return A2(
			_elm_lang$core$Maybe$withDefault,
			A2(_user$project$Types$Select, tlid, cur),
			A2(
				_elm_lang$core$Maybe$map,
				function (s) {
					return A2(
						_user$project$Types$Select,
						tlid,
						_elm_lang$core$Maybe$Just(s));
				},
				A2(
					_elm_lang$core$Maybe$map,
					_user$project$Toplevel$getPrevSibling(tl),
					cur)));
	});
var _user$project$Selection$nextSibling = F3(
	function (m, tlid, cur) {
		var tl = A2(_user$project$Toplevel$getTL, m, tlid);
		return A2(
			_elm_lang$core$Maybe$withDefault,
			A2(_user$project$Types$Select, tlid, cur),
			A2(
				_elm_lang$core$Maybe$map,
				function (s) {
					return A2(
						_user$project$Types$Select,
						tlid,
						_elm_lang$core$Maybe$Just(s));
				},
				A2(
					_elm_lang$core$Maybe$map,
					_user$project$Toplevel$getNextSibling(tl),
					cur)));
	});
var _user$project$Selection$downLevel = F3(
	function (m, tlid, cur) {
		var tl = A2(_user$project$Toplevel$getTL, m, tlid);
		return A2(
			_elm_lang$core$Maybe$withDefault,
			A2(
				_user$project$Types$Select,
				tlid,
				_user$project$Toplevel$rootOf(tl)),
			A2(
				_elm_lang$core$Maybe$map,
				function (c) {
					return A2(
						_user$project$Types$Select,
						tlid,
						_elm_lang$core$Maybe$Just(c));
				},
				A2(
					_elm_lang$core$Maybe$andThen,
					_user$project$Toplevel$firstChild(tl),
					cur)));
	});
var _user$project$Selection$enter = F3(
	function (m, tlid, cur) {
		var tl = A2(_user$project$Toplevel$getTL, m, tlid);
		var _p4 = A2(_user$project$Toplevel$holeType, tl, cur);
		if (_p4.ctor === 'NotAHole') {
			var _p5 = tl.data;
			if (_p5.ctor === 'TLHandler') {
				var _p7 = _p5._0;
				if (A2(_user$project$AST$isLeaf, cur, _p7.ast)) {
					var se = A2(_user$project$AST$subtree, cur, _p7.ast);
					return _user$project$Types$Many(
						{
							ctor: '::',
							_0: _user$project$Types$Enter(
								A2(_user$project$Types$Filling, tlid, cur)),
							_1: {
								ctor: '::',
								_0: _user$project$Types$AutocompleteMod(
									_user$project$Types$ACSetQuery(
										_user$project$AST$toContent(se))),
								_1: {ctor: '[]'}
							}
						});
				} else {
					if (A2(_user$project$Toplevel$isFilledSpec, _p7, cur)) {
						var content = A2(
							_elm_lang$core$Maybe$withDefault,
							'',
							A2(
								_elm_lang$core$Maybe$andThen,
								function (s) {
									var _p6 = s;
									if (_p6.ctor === 'Full') {
										return _elm_lang$core$Maybe$Just(_p6._1);
									} else {
										return _elm_lang$core$Maybe$Nothing;
									}
								},
								A2(_user$project$Toplevel$getSpec, _p7, cur)));
						return _user$project$Types$Many(
							{
								ctor: '::',
								_0: _user$project$Types$Enter(
									A2(_user$project$Types$Filling, tlid, cur)),
								_1: {
									ctor: '::',
									_0: _user$project$Types$AutocompleteMod(
										_user$project$Types$ACSetQuery(content)),
									_1: {ctor: '[]'}
								}
							});
					} else {
						return A3(
							_user$project$Selection$downLevel,
							m,
							tlid,
							_elm_lang$core$Maybe$Just(cur));
					}
				}
			} else {
				return _user$project$Types$NoChange;
			}
		} else {
			return _user$project$Types$Enter(
				A2(_user$project$Types$Filling, tlid, cur));
		}
	});
var _user$project$Selection$upLevel = F3(
	function (m, tlid, cur) {
		var tl = A2(_user$project$Toplevel$getTL, m, tlid);
		return A2(
			_elm_lang$core$Maybe$withDefault,
			A2(_user$project$Types$Select, tlid, _elm_lang$core$Maybe$Nothing),
			A2(
				_elm_lang$core$Maybe$map,
				function (p) {
					return A2(
						_user$project$Types$Select,
						tlid,
						_elm_lang$core$Maybe$Just(p));
				},
				A2(
					_elm_lang$core$Maybe$andThen,
					_user$project$Toplevel$getParentOf(tl),
					cur)));
	});

var _user$project$Main$subscriptions = function (m) {
	var dragSubs = function () {
		var _p0 = m.state;
		if (_p0.ctor === 'Dragging') {
			return {
				ctor: '::',
				_0: _elm_lang$mouse$Mouse$moves(
					_user$project$Types$DragToplevel(_p0._0)),
				_1: {ctor: '[]'}
			};
		} else {
			return {ctor: '[]'};
		}
	}();
	var keySubs = {
		ctor: '::',
		_0: A2(
			_user$project$Window_Events$onWindow,
			'keydown',
			A2(_elm_lang$core$Json_Decode$map, _user$project$Types$GlobalKeyPress, _Gizra$elm_keyboard_event$Keyboard_Event$decodeKeyboardEvent)),
		_1: {ctor: '[]'}
	};
	return _elm_lang$core$Platform_Sub$batch(
		_elm_lang$core$List$concat(
			{
				ctor: '::',
				_0: keySubs,
				_1: {
					ctor: '::',
					_0: dragSubs,
					_1: {ctor: '[]'}
				}
			}));
};
var _user$project$Main$isFieldAccessDot = F2(
	function (state, baseStr) {
		var str = A3(_user$project$Util$replace, '\\.*$', '', baseStr);
		var _p1 = state;
		if (_p1.ctor === 'Entering') {
			return (A2(_elm_lang$core$String$startsWith, '\"', str) || _user$project$Runtime$isInt(str)) ? false : true;
		} else {
			return false;
		}
	});
var _user$project$Main$update_ = F2(
	function (msg, m) {
		var _p2 = {ctor: '_Tuple2', _0: msg, _1: m.state};
		_v2_17:
		do {
			if (_p2.ctor === '_Tuple2') {
				switch (_p2._0.ctor) {
					case 'GlobalKeyPress':
						var _p22 = _p2._0._0;
						if (_p22.ctrlKey && (_elm_lang$core$Native_Utils.eq(_p22.keyCode, _SwiftsNamesake$proper_keyboard$Keyboard_Key$Z) || _elm_lang$core$Native_Utils.eq(_p22.keyCode, _SwiftsNamesake$proper_keyboard$Keyboard_Key$Y))) {
							var _p3 = _p22.keyCode;
							switch (_p3.ctor) {
								case 'Z':
									return _user$project$Types$RPC(
										{
											ctor: '_Tuple2',
											_0: {
												ctor: '::',
												_0: _user$project$Types$Undo,
												_1: {ctor: '[]'}
											},
											_1: _user$project$Types$FocusNothing
										});
								case 'Y':
									return _user$project$Types$RPC(
										{
											ctor: '_Tuple2',
											_0: {
												ctor: '::',
												_0: _user$project$Types$Redo,
												_1: {ctor: '[]'}
											},
											_1: _user$project$Types$FocusNothing
										});
								default:
									return _user$project$Types$NoChange;
							}
						} else {
							var _p4 = _p2._1;
							switch (_p4.ctor) {
								case 'Selecting':
									var _p10 = _p4._0;
									var _p9 = _p4._1;
									var _p5 = _p22.keyCode;
									switch (_p5.ctor) {
										case 'Backspace':
											var _p6 = _p9;
											if (_p6.ctor === 'Nothing') {
												return _user$project$Types$Many(
													{
														ctor: '::',
														_0: _user$project$Types$RPC(
															{
																ctor: '_Tuple2',
																_0: {
																	ctor: '::',
																	_0: _user$project$Types$DeleteTL(_p10),
																	_1: {ctor: '[]'}
																},
																_1: _user$project$Types$FocusNothing
															}),
														_1: {
															ctor: '::',
															_0: _user$project$Types$Deselect,
															_1: {ctor: '[]'}
														}
													});
											} else {
												return A3(_user$project$Selection$delete, m, _p10, _p6._0);
											}
										case 'Escape':
											return _user$project$Types$Deselect;
										case 'Enter':
											if (_p22.shiftKey) {
												var _p7 = function (_) {
													return _.data;
												}(
													A2(_user$project$Toplevel$getTL, m, _p10));
												if (_p7.ctor === 'TLDB') {
													return _user$project$Types$RPC(
														{
															ctor: '_Tuple2',
															_0: {
																ctor: '::',
																_0: A3(
																	_user$project$Types$AddDBCol,
																	_p10,
																	_user$project$Types$gid(
																		{ctor: '_Tuple0'}),
																	_user$project$Types$gid(
																		{ctor: '_Tuple0'})),
																_1: {ctor: '[]'}
															},
															_1: A2(_user$project$Types$FocusNext, _p10, _elm_lang$core$Maybe$Nothing)
														});
												} else {
													return _user$project$Types$NoChange;
												}
											} else {
												var _p8 = _p9;
												if (_p8.ctor === 'Just') {
													return A3(_user$project$Selection$enter, m, _p10, _p8._0);
												} else {
													return A3(_user$project$Selection$downLevel, m, _p10, _p9);
												}
											}
										case 'Up':
											return A3(_user$project$Selection$upLevel, m, _p10, _p9);
										case 'Down':
											return A3(_user$project$Selection$downLevel, m, _p10, _p9);
										case 'Right':
											return A3(_user$project$Selection$nextSibling, m, _p10, _p9);
										case 'Left':
											return A3(_user$project$Selection$previousSibling, m, _p10, _p9);
										case 'Tab':
											return A3(_user$project$Selection$nextHole, m, _p10, _p9);
										case 'O':
											return _p22.ctrlKey ? A3(_user$project$Selection$upLevel, m, _p10, _p9) : _user$project$Types$NoChange;
										case 'I':
											return _p22.ctrlKey ? A3(_user$project$Selection$downLevel, m, _p10, _p9) : _user$project$Types$NoChange;
										case 'N':
											return _p22.ctrlKey ? A3(_user$project$Selection$nextSibling, m, _p10, _p9) : _user$project$Types$NoChange;
										case 'P':
											return _p22.ctrlKey ? A3(_user$project$Selection$previousSibling, m, _p10, _p9) : _user$project$Types$NoChange;
										default:
											return _user$project$Types$NoChange;
									}
								case 'Entering':
									var _p20 = _p4._0;
									if (_p22.ctrlKey) {
										var _p11 = _p22.keyCode;
										switch (_p11.ctor) {
											case 'P':
												return _user$project$Types$AutocompleteMod(_user$project$Types$ACSelectUp);
											case 'N':
												return _user$project$Types$AutocompleteMod(_user$project$Types$ACSelectDown);
											case 'Enter':
												return _user$project$Autocomplete$isLargeStringEntry(m.complete) ? A4(_user$project$Entry$submit, m, _p20, _user$project$Entry$ContinueThread, m.complete.value) : (_user$project$Autocomplete$isSmallStringEntry(m.complete) ? _user$project$Types$Many(
													{
														ctor: '::',
														_0: _user$project$Types$AutocompleteMod(
															_user$project$Types$ACAppendQuery('\n')),
														_1: {
															ctor: '::',
															_0: _user$project$Types$MakeCmd(_user$project$Entry$focusEntry),
															_1: {ctor: '[]'}
														}
													}) : _user$project$Types$NoChange);
											default:
												return _user$project$Types$NoChange;
										}
									} else {
										if (_p22.shiftKey && _elm_lang$core$Native_Utils.eq(_p22.keyCode, _SwiftsNamesake$proper_keyboard$Keyboard_Key$Enter)) {
											var _p12 = _p20;
											if (_p12.ctor === 'Filling') {
												var tl = A2(_user$project$Toplevel$getTL, m, _p12._0);
												var _p13 = tl.data;
												if (_p13.ctor === 'TLDB') {
													return _user$project$Types$NoChange;
												} else {
													var name = _user$project$Autocomplete$getValue(m.complete);
													return A4(_user$project$Entry$submit, m, _p20, _user$project$Entry$StartThread, name);
												}
											} else {
												var name = _user$project$Autocomplete$getValue(m.complete);
												return A4(_user$project$Entry$submit, m, _p20, _user$project$Entry$StartThread, name);
											}
										} else {
											var _p14 = _p22.keyCode;
											switch (_p14.ctor) {
												case 'Enter':
													if (_user$project$Autocomplete$isLargeStringEntry(m.complete)) {
														return _user$project$Types$AutocompleteMod(
															_user$project$Types$ACSetQuery(m.complete.value));
													} else {
														var name = _user$project$Autocomplete$getValue(m.complete);
														return A4(_user$project$Entry$submit, m, _p20, _user$project$Entry$ContinueThread, name);
													}
												case 'Unknown':
													if (_elm_lang$core$Native_Utils.eq(
														_p22.key,
														_elm_lang$core$Maybe$Just('.')) && A2(_user$project$Main$isFieldAccessDot, m.state, m.complete.value)) {
														var name = _user$project$Autocomplete$getValue(m.complete);
														return A4(
															_user$project$Entry$submit,
															m,
															_p20,
															_user$project$Entry$ContinueThread,
															A2(_elm_lang$core$Basics_ops['++'], '.', name));
													} else {
														return _user$project$Types$NoChange;
													}
												case 'Escape':
													var _p15 = _p20;
													if (_p15.ctor === 'Creating') {
														return _user$project$Types$Many(
															{
																ctor: '::',
																_0: _user$project$Types$Deselect,
																_1: {
																	ctor: '::',
																	_0: _user$project$Types$AutocompleteMod(_user$project$Types$ACReset),
																	_1: {ctor: '[]'}
																}
															});
													} else {
														var _p19 = _p15._0;
														var _p18 = _p15._1;
														var tl = A2(_user$project$Toplevel$getTL, m, _p19);
														var _p16 = tl.data;
														if (_p16.ctor === 'TLHandler') {
															var _p17 = _p16._0;
															var replacement = _user$project$AST$closeThread(_p17.ast);
															return _elm_lang$core$Native_Utils.eq(replacement, _p17.ast) ? _user$project$Types$Many(
																{
																	ctor: '::',
																	_0: A2(
																		_user$project$Types$Select,
																		_p19,
																		_elm_lang$core$Maybe$Just(_p18)),
																	_1: {
																		ctor: '::',
																		_0: _user$project$Types$AutocompleteMod(_user$project$Types$ACReset),
																		_1: {ctor: '[]'}
																	}
																}) : _user$project$Types$RPC(
																{
																	ctor: '_Tuple2',
																	_0: {
																		ctor: '::',
																		_0: A3(
																			_user$project$Types$SetHandler,
																			tl.id,
																			tl.pos,
																			_elm_lang$core$Native_Utils.update(
																				_p17,
																				{ast: replacement})),
																		_1: {ctor: '[]'}
																	},
																	_1: A2(_user$project$Types$FocusNext, tl.id, _elm_lang$core$Maybe$Nothing)
																});
														} else {
															return _user$project$Types$Many(
																{
																	ctor: '::',
																	_0: A2(
																		_user$project$Types$Select,
																		_p19,
																		_elm_lang$core$Maybe$Just(_p18)),
																	_1: {
																		ctor: '::',
																		_0: _user$project$Types$AutocompleteMod(_user$project$Types$ACReset),
																		_1: {ctor: '[]'}
																	}
																});
														}
													}
												case 'Up':
													return _user$project$Types$AutocompleteMod(_user$project$Types$ACSelectUp);
												case 'Down':
													return _user$project$Types$Many(
														{
															ctor: '::',
															_0: _user$project$Types$AutocompleteMod(
																_user$project$Types$ACOpen(true)),
															_1: {
																ctor: '::',
																_0: _user$project$Types$AutocompleteMod(_user$project$Types$ACSelectDown),
																_1: {ctor: '[]'}
															}
														});
												case 'Right':
													var sp = _user$project$Autocomplete$sharedPrefix(m.complete);
													return _elm_lang$core$Native_Utils.eq(sp, '') ? _user$project$Types$NoChange : _user$project$Types$AutocompleteMod(
														_user$project$Types$ACSetQuery(sp));
												default:
													return _user$project$Types$NoChange;
											}
										}
									}
								case 'Deselected':
									var _p21 = _p22.keyCode;
									switch (_p21.ctor) {
										case 'Enter':
											return _user$project$Entry$createFindSpace(m);
										case 'Up':
											return _user$project$Types$SetCenter(
												_user$project$Viewport$moveUp(m.center));
										case 'Down':
											return _user$project$Types$SetCenter(
												_user$project$Viewport$moveDown(m.center));
										case 'Left':
											return _user$project$Types$SetCenter(
												_user$project$Viewport$moveLeft(m.center));
										case 'Right':
											return _user$project$Types$SetCenter(
												_user$project$Viewport$moveRight(m.center));
										default:
											return _user$project$Types$NoChange;
									}
								default:
									return _user$project$Types$NoChange;
							}
						}
					case 'EntryInputMsg':
						var _p23 = _p2._0._0;
						return (A2(_elm_lang$core$String$endsWith, '.', _p23) && A2(_user$project$Main$isFieldAccessDot, m.state, _p23)) ? _user$project$Types$NoChange : _user$project$Types$Many(
							{
								ctor: '::',
								_0: _user$project$Types$AutocompleteMod(
									_user$project$Types$ACSetQuery(_p23)),
								_1: {
									ctor: '::',
									_0: _user$project$Types$MakeCmd(_user$project$Entry$focusEntry),
									_1: {ctor: '[]'}
								}
							});
					case 'EntrySubmitMsg':
						return _user$project$Types$NoChange;
					case 'GlobalClick':
						var _p24 = _p2._0._0;
						return _elm_lang$core$Native_Utils.eq(_p24.button, _user$project$Defaults$leftButton) ? _user$project$Types$Many(
							{
								ctor: '::',
								_0: _user$project$Types$AutocompleteMod(_user$project$Types$ACReset),
								_1: {
									ctor: '::',
									_0: _user$project$Types$Enter(
										_user$project$Types$Creating(
											A2(_user$project$Viewport$toAbsolute, m, _p24.pos))),
									_1: {ctor: '[]'}
								}
							}) : _user$project$Types$NoChange;
					case 'ToplevelClickDown':
						var _p25 = _p2._0._1;
						return _elm_lang$core$Native_Utils.eq(_p25.button, _user$project$Defaults$leftButton) ? A4(_user$project$Types$Drag, _p2._0._0.id, _p25.pos, false, m.state) : _user$project$Types$NoChange;
					case 'DragToplevel':
						var _p29 = _p2._0._1;
						var _p26 = m.state;
						if (_p26.ctor === 'Dragging') {
							var _p28 = _p26._0;
							var _p27 = _p26._1;
							var yDiff = _p29.y - _p27.vy;
							var xDiff = _p29.x - _p27.vx;
							var m2 = A4(_user$project$Toplevel$move, _p28, xDiff, yDiff, m);
							return _user$project$Types$Many(
								{
									ctor: '::',
									_0: A2(_user$project$Types$SetToplevels, m2.toplevels, m2.analysis),
									_1: {
										ctor: '::',
										_0: A4(
											_user$project$Types$Drag,
											_p28,
											{vx: _p29.x, vy: _p29.y},
											true,
											_p26._3),
										_1: {ctor: '[]'}
									}
								});
						} else {
							return _user$project$Types$NoChange;
						}
					case 'ToplevelClickUp':
						var _p34 = _p2._0._1;
						if (_elm_lang$core$Native_Utils.eq(_p34.button, _user$project$Defaults$leftButton)) {
							var _p30 = m.state;
							if (_p30.ctor === 'Dragging') {
								var _p32 = _p30._0;
								var _p31 = _p30._1;
								var yDiff = _p34.pos.vy - _p31.vy;
								var xDiff = _p34.pos.vx - _p31.vx;
								var m2 = A4(_user$project$Toplevel$move, _p32, xDiff, yDiff, m);
								var tl = A2(_user$project$Toplevel$getTL, m2, _p32);
								return _p30._2 ? _user$project$Types$Many(
									{
										ctor: '::',
										_0: _user$project$Types$SetState(_p30._3),
										_1: {
											ctor: '::',
											_0: _user$project$Types$RPC(
												{
													ctor: '_Tuple2',
													_0: {
														ctor: '::',
														_0: A2(_user$project$Types$MoveTL, tl.id, tl.pos),
														_1: {ctor: '[]'}
													},
													_1: _user$project$Types$FocusSame
												}),
											_1: {ctor: '[]'}
										}
									}) : A2(_user$project$Types$Select, _p32, _elm_lang$core$Maybe$Nothing);
							} else {
								return _elm_lang$core$Native_Utils.crashCase(
									'Main',
									{
										start: {line: 481, column: 9},
										end: {line: 493, column: 58}
									},
									_p30)('it can never not be dragging');
							}
						} else {
							return _user$project$Types$NoChange;
						}
					case 'ClearGraph':
						return _user$project$Types$Many(
							{
								ctor: '::',
								_0: _user$project$Types$RPC(
									{
										ctor: '_Tuple2',
										_0: {
											ctor: '::',
											_0: _user$project$Types$DeleteAll,
											_1: {ctor: '[]'}
										},
										_1: _user$project$Types$FocusNothing
									}),
								_1: {
									ctor: '::',
									_0: _user$project$Types$Deselect,
									_1: {ctor: '[]'}
								}
							});
					case 'SaveTestButton':
						return _user$project$Types$MakeCmd(_user$project$RPC$saveTest);
					case 'FinishIntegrationTest':
						return _user$project$Types$EndIntegrationTest;
					case 'RPCCallBack':
						if (_p2._0._3.ctor === 'Ok') {
							if (_p2._0._3._0.ctor === '_Tuple2') {
								var _p41 = _p2._0._3._0._0;
								var m2 = _elm_lang$core$Native_Utils.update(
									m,
									{toplevels: _p41});
								var newState = function () {
									var _p35 = _p2._0._0;
									switch (_p35.ctor) {
										case 'FocusNext':
											var _p37 = _p35._0;
											var tl = A2(_user$project$Toplevel$getTL, m2, _p37);
											var nh = A2(_user$project$Toplevel$getNextHole, tl, _p35._1);
											var _p36 = nh;
											if (_p36.ctor === 'Just') {
												return _user$project$Types$Enter(
													A2(_user$project$Types$Filling, _p37, _p36._0));
											} else {
												return A2(_user$project$Types$Select, _p37, _elm_lang$core$Maybe$Nothing);
											}
										case 'FocusExact':
											var _p40 = _p35._0;
											var _p39 = _p35._1;
											var tl = A2(_user$project$Toplevel$getTL, m2, _p40);
											var ht = A2(_user$project$Toplevel$holeType, tl, _p39);
											var _p38 = ht;
											if (_p38.ctor === 'NotAHole') {
												return A2(
													_user$project$Types$Select,
													_p40,
													_elm_lang$core$Maybe$Just(_p39));
											} else {
												return _user$project$Types$Enter(
													A2(_user$project$Types$Filling, _p40, _p39));
											}
										default:
											return _user$project$Types$NoChange;
									}
								}();
								return _user$project$Types$Many(
									{
										ctor: '::',
										_0: A2(_user$project$Types$SetToplevels, _p41, _p2._0._3._0._1),
										_1: {
											ctor: '::',
											_0: _user$project$Types$AutocompleteMod(_user$project$Types$ACReset),
											_1: {
												ctor: '::',
												_0: _user$project$Types$ClearError,
												_1: {
													ctor: '::',
													_0: newState,
													_1: {
														ctor: '::',
														_0: _p2._0._1,
														_1: {ctor: '[]'}
													}
												}
											}
										}
									});
							} else {
								break _v2_17;
							}
						} else {
							switch (_p2._0._3._0.ctor) {
								case 'BadStatus':
									return _user$project$Types$Error(
										A2(_elm_lang$core$Basics_ops['++'], 'Error: ', _p2._0._3._0._0.body));
								case 'NetworkError':
									return _user$project$Types$Error('Network error: is the server running?');
								default:
									break _v2_17;
							}
						}
					case 'SaveTestCallBack':
						if (_p2._0._0.ctor === 'Ok') {
							return _user$project$Types$Error(
								A2(_elm_lang$core$Basics_ops['++'], 'Success! ', _p2._0._0._0));
						} else {
							return _user$project$Types$Error(
								A2(
									_elm_lang$core$Basics_ops['++'],
									'Error: ',
									_elm_lang$core$Basics$toString(_p2._0._0._0)));
						}
					case 'FocusEntry':
						return _user$project$Types$NoChange;
					case 'FocusAutocompleteItem':
						return _user$project$Types$NoChange;
					default:
						break _v2_17;
				}
			} else {
				break _v2_17;
			}
		} while(false);
		return _user$project$Types$Error(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'Dark Client Error: nothing for ',
				_elm_lang$core$Basics$toString(_p2)));
	});
var _user$project$Main$processAutocompleteMods = F2(
	function (m, mods) {
		var complete = A3(
			_elm_lang$core$List$foldl,
			F2(
				function (mod, complete) {
					return A2(_user$project$Autocomplete$update, mod, complete);
				}),
			m.complete,
			mods);
		return {
			ctor: '_Tuple2',
			_0: complete,
			_1: _user$project$Autocomplete$focusItem(complete.index)
		};
	});
var _user$project$Main$updateMod = F3(
	function (origm, mod, _p42) {
		var _p43 = _p42;
		var _p63 = _p43._0;
		var _p44 = function () {
			var _p45 = mod;
			switch (_p45.ctor) {
				case 'Error':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{
								error: _elm_lang$core$Maybe$Just(_p45._0)
							}),
						{ctor: '[]'});
				case 'ClearError':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{error: _elm_lang$core$Maybe$Nothing}),
						{ctor: '[]'});
				case 'RPC':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_p63,
						{
							ctor: '::',
							_0: A3(_user$project$RPC$rpc, _p63, _p45._0._1, _p45._0._0),
							_1: {ctor: '[]'}
						});
				case 'NoChange':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_p63,
						{ctor: '[]'});
				case 'TriggerIntegrationTest':
					var expect = _user$project$IntegrationTest$trigger(_p45._0);
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{integrationTestState: expect}),
						{ctor: '[]'});
				case 'EndIntegrationTest':
					var expectationFn = function () {
						var _p46 = _p63.integrationTestState;
						switch (_p46.ctor) {
							case 'IntegrationTestExpectation':
								return _p46._0;
							case 'IntegrationTestFinished':
								return _elm_lang$core$Native_Utils.crashCase(
									'Main',
									{
										start: {line: 154, column: 13},
										end: {line: 159, column: 85}
									},
									_p46)('Attempted to end integration test but one ran + was already finished');
							default:
								return _elm_lang$core$Native_Utils.crashCase(
									'Main',
									{
										start: {line: 154, column: 13},
										end: {line: 159, column: 85}
									},
									_p46)('Attempted to end integration test but none was running');
						}
					}();
					var result = expectationFn(_p63);
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{
								integrationTestState: _user$project$Types$IntegrationTestFinished(result)
							}),
						{ctor: '[]'});
				case 'MakeCmd':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_p63,
						{
							ctor: '::',
							_0: _p45._0,
							_1: {ctor: '[]'}
						});
				case 'SetState':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{state: _p45._0}),
						{ctor: '[]'});
				case 'Select':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{
								state: A2(_user$project$Types$Selecting, _p45._0, _p45._1)
							}),
						{ctor: '[]'});
				case 'Enter':
					var _p61 = _p45._0;
					var lv = function () {
						var _p49 = _p61;
						if (_p49.ctor === 'Creating') {
							return _elm_lang$core$Maybe$Nothing;
						} else {
							var _p54 = _p49._0;
							var _p53 = _p49._1;
							var tl = A2(_user$project$Toplevel$getTL, _p63, _p54);
							var obj = function () {
								var _p50 = A2(_user$project$Toplevel$holeType, tl, _p53);
								switch (_p50.ctor) {
									case 'ExprHole':
										var handler = _user$project$Util$deMaybe(
											_user$project$Toplevel$asHandler(tl));
										var p = A2(_user$project$AST$parentOf_, _p53, handler.ast);
										var _p51 = p;
										if ((_p51.ctor === 'Just') && (_p51._0.ctor === 'Thread')) {
											var ids = A2(_elm_lang$core$List$map, _user$project$AST$toID, _p51._0._1);
											var selfi = A2(_elm_community$list_extra$List_Extra$elemIndex, _p53, ids);
											var prev = A2(
												_elm_lang$core$Maybe$map,
												function (x) {
													return x - 1;
												},
												selfi);
											return A2(
												_elm_lang$core$Maybe$andThen,
												function (i) {
													return A2(_elm_community$list_extra$List_Extra$getAt, i, ids);
												},
												prev);
										} else {
											return _elm_lang$core$Maybe$Nothing;
										}
									case 'FieldHole':
										var handler = _user$project$Util$deMaybe(
											_user$project$Toplevel$asHandler(tl));
										var p = A2(_user$project$AST$parentOf, _p53, handler.ast);
										var _p52 = p;
										if (_p52.ctor === 'FieldAccess') {
											return _elm_lang$core$Maybe$Just(
												_user$project$AST$toID(_p52._1));
										} else {
											return _elm_lang$core$Maybe$Nothing;
										}
									default:
										return _elm_lang$core$Maybe$Nothing;
								}
							}();
							return A2(
								_elm_lang$core$Maybe$andThen,
								A2(_user$project$Analysis$getLiveValue, _p63, _p54),
								obj);
						}
					}();
					var showFunctions = function () {
						var _p55 = _p61;
						if (_p55.ctor === 'Creating') {
							return true;
						} else {
							var _p58 = _p55._1;
							var tl = A2(_user$project$Toplevel$getTL, _p63, _p55._0);
							var _p56 = A2(_user$project$Toplevel$holeType, tl, _p58);
							switch (_p56.ctor) {
								case 'ExprHole':
									return true;
								case 'FieldHole':
									return false;
								case 'BindHole':
									return false;
								case 'SpecHole':
									return false;
								case 'DBColNameHole':
									return false;
								case 'DBColTypeHole':
									return false;
								default:
									var _p57 = tl.data;
									if (_p57.ctor === 'TLHandler') {
										return A2(_user$project$Toplevel$isExpression, _p57._0, _p58);
									} else {
										return false;
									}
							}
						}
					}();
					var varnames = function () {
						var _p59 = _p61;
						if (_p59.ctor === 'Creating') {
							return {ctor: '[]'};
						} else {
							return A3(_user$project$Analysis$getAvailableVarnames, _p63, _p59._0, _p59._1);
						}
					}();
					var _p60 = A2(
						_user$project$Main$processAutocompleteMods,
						_p63,
						{
							ctor: '::',
							_0: _user$project$Types$ACSetAvailableVarnames(varnames),
							_1: {
								ctor: '::',
								_0: _user$project$Types$ACShowFunctions(showFunctions),
								_1: {
									ctor: '::',
									_0: _user$project$Types$ACFilterByLiveValue(lv),
									_1: {ctor: '[]'}
								}
							}
						});
					var complete = _p60._0;
					var acCmd = _p60._1;
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{
								state: _user$project$Types$Entering(_p61),
								complete: complete
							}),
						{
							ctor: '::',
							_0: acCmd,
							_1: {
								ctor: '::',
								_0: _user$project$Entry$focusEntry,
								_1: {ctor: '[]'}
							}
						});
				case 'SetToplevels':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{toplevels: _p45._0, analysis: _p45._1}),
						{ctor: '[]'});
				case 'SetCenter':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{center: _p45._0}),
						{ctor: '[]'});
				case 'Drag':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{
								state: A4(_user$project$Types$Dragging, _p45._0, _p45._1, _p45._2, _p45._3)
							}),
						{ctor: '[]'});
				case 'Deselect':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							_p63,
							{state: _user$project$Types$Deselected}),
						{ctor: '[]'});
				case 'AutocompleteMod':
					var _p62 = A2(
						_user$project$Main$processAutocompleteMods,
						_p63,
						{
							ctor: '::',
							_0: _p45._0,
							_1: {ctor: '[]'}
						});
					var complete = _p62._0;
					var cmd = _p62._1;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							_p63,
							{complete: complete}),
						_1: cmd
					};
				default:
					return A3(
						_elm_lang$core$List$foldl,
						_user$project$Main$updateMod(origm),
						{ctor: '_Tuple2', _0: _p63, _1: _elm_lang$core$Platform_Cmd$none},
						_p45._0);
			}
		}();
		var newm = _p44._0;
		var newcmd = _p44._1;
		return {
			ctor: '_Tuple2',
			_0: newm,
			_1: _elm_lang$core$Platform_Cmd$batch(
				{
					ctor: '::',
					_0: _p43._1,
					_1: {
						ctor: '::',
						_0: newcmd,
						_1: {ctor: '[]'}
					}
				})
		};
	});
var _user$project$Main$selectCenter = F2(
	function (old, $new) {
		var fakeCenter = _user$project$Defaults$initialPos;
		var _p64 = _user$project$Util$windowSize(
			{ctor: '_Tuple0'});
		var xSize = _p64._0;
		var ySize = _p64._1;
		var xThreshold = (xSize / 10) | 0;
		var yThreshold = (ySize / 10) | 0;
		var newY = ((_elm_lang$core$Native_Utils.cmp($new.y, (old.y + (ySize - fakeCenter.vy)) - yThreshold) > 0) || (_elm_lang$core$Native_Utils.cmp($new.y, (old.y - fakeCenter.vy) + yThreshold) < 0)) ? $new.y : old.y;
		var newX = ((_elm_lang$core$Native_Utils.cmp($new.x, (old.x + (xSize - fakeCenter.vx)) - xThreshold) > 0) || (_elm_lang$core$Native_Utils.cmp($new.x, (old.x - fakeCenter.vx) + xThreshold) < 0)) ? $new.x : old.x;
		return {x: newX, y: newY};
	});
var _user$project$Main$flag2function = function (fn) {
	return {
		name: fn.name,
		description: fn.description,
		returnTipe: _user$project$Runtime$str2tipe(fn.return_type),
		parameters: A2(
			_elm_lang$core$List$map,
			function (p) {
				return {
					name: p.name,
					tipe: _user$project$Runtime$str2tipe(p.tipe),
					block_args: p.block_args,
					optional: p.optional,
					description: p.description
				};
			},
			fn.parameters)
	};
};
var _user$project$Main$init = F2(
	function (_p65, location) {
		var _p66 = _p65;
		var integrationTestName = 'test_empty_integration_test';
		var shouldRunIntegrationTest = _elm_lang$core$Native_Utils.eq('/admin/integration_test', location.pathname);
		var tests = function () {
			var _p67 = _user$project$VariantTesting$parseVariantTestsFromQueryString(location.search);
			if (_p67.ctor === 'Just') {
				return _p67._0;
			} else {
				return {ctor: '[]'};
			}
		}();
		var editor = function () {
			var _p68 = _p66.editorState;
			if (_p68.ctor === 'Just') {
				return _p68._0;
			} else {
				return _user$project$Defaults$defaultEditor;
			}
		}();
		var m = _user$project$Defaults$defaultModel(editor);
		var m2 = _elm_lang$core$Native_Utils.update(
			m,
			{
				complete: _user$project$Autocomplete$init(
					A2(_elm_lang$core$List$map, _user$project$Main$flag2function, _p66.complete)),
				tests: tests,
				toplevels: {ctor: '[]'}
			});
		return shouldRunIntegrationTest ? A2(
			_elm_lang$core$Platform_Cmd_ops['!'],
			m2,
			{
				ctor: '::',
				_0: A2(_user$project$RPC$integrationRpc, m, integrationTestName),
				_1: {ctor: '[]'}
			}) : A2(
			_elm_lang$core$Platform_Cmd_ops['!'],
			m2,
			{
				ctor: '::',
				_0: A3(
					_user$project$RPC$rpc,
					m,
					_user$project$Types$FocusNothing,
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			});
	});
var _user$project$Main$setStorage = _elm_lang$core$Native_Platform.outgoingPort(
	'setStorage',
	function (v) {
		return {};
	});
var _user$project$Main$update = F2(
	function (msg, m) {
		var mods = A2(_user$project$Main$update_, msg, m);
		var _p69 = A3(
			_user$project$Main$updateMod,
			m,
			mods,
			{ctor: '_Tuple2', _0: m, _1: _elm_lang$core$Platform_Cmd$none});
		var newm = _p69._0;
		var newc = _p69._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				newm,
				{lastMsg: msg, lastMod: mods}),
			_1: _elm_lang$core$Platform_Cmd$batch(
				{
					ctor: '::',
					_0: newc,
					_1: {
						ctor: '::',
						_0: _user$project$Main$setStorage(
							_user$project$Defaults$model2editor(m)),
						_1: {ctor: '[]'}
					}
				})
		};
	});
var _user$project$Main$main = A2(
	_elm_lang$navigation$Navigation$programWithFlags,
	_user$project$Types$LocationChange,
	{init: _user$project$Main$init, view: _user$project$View$view, update: _user$project$Main$update, subscriptions: _user$project$Main$subscriptions})(
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (complete) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (editorState) {
					return _elm_lang$core$Json_Decode$succeed(
						{complete: complete, editorState: editorState});
				},
				A2(
					_elm_lang$core$Json_Decode$field,
					'editorState',
					_elm_lang$core$Json_Decode$oneOf(
						{
							ctor: '::',
							_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$core$Json_Decode$map,
									_elm_lang$core$Maybe$Just,
									_elm_lang$core$Json_Decode$succeed(
										{})),
								_1: {ctor: '[]'}
							}
						})));
		},
		A2(
			_elm_lang$core$Json_Decode$field,
			'complete',
			_elm_lang$core$Json_Decode$list(
				A2(
					_elm_lang$core$Json_Decode$andThen,
					function (description) {
						return A2(
							_elm_lang$core$Json_Decode$andThen,
							function (name) {
								return A2(
									_elm_lang$core$Json_Decode$andThen,
									function (parameters) {
										return A2(
											_elm_lang$core$Json_Decode$andThen,
											function (return_type) {
												return _elm_lang$core$Json_Decode$succeed(
													{description: description, name: name, parameters: parameters, return_type: return_type});
											},
											A2(_elm_lang$core$Json_Decode$field, 'return_type', _elm_lang$core$Json_Decode$string));
									},
									A2(
										_elm_lang$core$Json_Decode$field,
										'parameters',
										_elm_lang$core$Json_Decode$list(
											A2(
												_elm_lang$core$Json_Decode$andThen,
												function (block_args) {
													return A2(
														_elm_lang$core$Json_Decode$andThen,
														function (description) {
															return A2(
																_elm_lang$core$Json_Decode$andThen,
																function (name) {
																	return A2(
																		_elm_lang$core$Json_Decode$andThen,
																		function (optional) {
																			return A2(
																				_elm_lang$core$Json_Decode$andThen,
																				function (tipe) {
																					return _elm_lang$core$Json_Decode$succeed(
																						{block_args: block_args, description: description, name: name, optional: optional, tipe: tipe});
																				},
																				A2(_elm_lang$core$Json_Decode$field, 'tipe', _elm_lang$core$Json_Decode$string));
																		},
																		A2(_elm_lang$core$Json_Decode$field, 'optional', _elm_lang$core$Json_Decode$bool));
																},
																A2(_elm_lang$core$Json_Decode$field, 'name', _elm_lang$core$Json_Decode$string));
														},
														A2(_elm_lang$core$Json_Decode$field, 'description', _elm_lang$core$Json_Decode$string));
												},
												A2(
													_elm_lang$core$Json_Decode$field,
													'block_args',
													_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string))))));
							},
							A2(_elm_lang$core$Json_Decode$field, 'name', _elm_lang$core$Json_Decode$string));
					},
					A2(_elm_lang$core$Json_Decode$field, 'description', _elm_lang$core$Json_Decode$string))))));

var Elm = {};
Elm['Main'] = Elm['Main'] || {};
if (typeof _user$project$Main$main !== 'undefined') {
    _user$project$Main$main(Elm['Main'], 'Main', {"types":{"unions":{"Dict.LeafColor":{"args":[],"tags":{"LBBlack":[],"LBlack":[]}},"Platform.Cmd.Cmd":{"args":["msg"],"tags":{"Cmd":[]}},"Types.Expr":{"args":[],"tags":{"If":["Types.ID","Types.Expr","Types.Expr","Types.Expr"],"Value":["Types.ID","String"],"Variable":["Types.ID","Types.VarName"],"Hole":["Types.ID"],"Lambda":["Types.ID","List Types.VarName","Types.Expr"],"FnCall":["Types.ID","Types.FnName","List Types.Expr"],"Let":["Types.ID","Types.VarBind","Types.Expr","Types.Expr"],"Thread":["Types.ID","List Types.Expr"],"FieldAccess":["Types.ID","Types.Expr","Types.Field"]}},"Types.TLID":{"args":[],"tags":{"TLID":["Int"]}},"Dom.Error":{"args":[],"tags":{"NotFound":["String"]}},"Types.Modification":{"args":[],"tags":{"SetToplevels":["List Types.Toplevel","List Types.TLAResult"],"EndIntegrationTest":[],"SetState":["Types.State"],"MakeCmd":["Platform.Cmd.Cmd Types.Msg"],"AutocompleteMod":["Types.AutocompleteMod"],"Enter":["Types.EntryCursor"],"Error":["String"],"RPC":["( List Types.RPC, Types.Focus )"],"SetCenter":["Types.Pos"],"NoChange":[],"TriggerIntegrationTest":["String"],"Select":["Types.TLID","Maybe.Maybe Types.ID"],"Drag":["Types.TLID","Types.VPos","Types.HasMoved","Types.State"],"Many":["List Types.Modification"],"ClearError":[],"Deselect":[]}},"Types.EntryCursor":{"args":[],"tags":{"Filling":["Types.TLID","Types.ID"],"Creating":["Types.Pos"]}},"Dict.Dict":{"args":["k","v"],"tags":{"RBNode_elm_builtin":["Dict.NColor","k","v","Dict.Dict k v","Dict.Dict k v"],"RBEmpty_elm_builtin":["Dict.LeafColor"]}},"Maybe.Maybe":{"args":["a"],"tags":{"Just":["a"],"Nothing":[]}},"Types.RPC":{"args":[],"tags":{"SetDBColType":["Types.TLID","Types.ID","Types.DBColType"],"Redo":[],"AddDBCol":["Types.TLID","Types.ID","Types.ID"],"DeleteAll":[],"SetDBColName":["Types.TLID","Types.ID","Types.DBColName"],"DeleteTL":["Types.TLID"],"MoveTL":["Types.TLID","Types.Pos"],"SetHandler":["Types.TLID","Types.Pos","Types.Handler"],"CreateDB":["Types.TLID","Types.Pos","Types.DBName"],"Savepoint":[],"Undo":[],"NoOp":[]}},"Types.Msg":{"args":[],"tags":{"FocusAutocompleteItem":["Result.Result Dom.Error ()"],"GlobalClick":["Types.MouseEvent"],"Initialization":[],"GlobalKeyPress":["Keyboard.Event.KeyboardEvent"],"EntryInputMsg":["String"],"SaveTestCallBack":["Result.Result Http.Error String"],"ToplevelClickUp":["Types.TLID","Types.MouseEvent"],"EntrySubmitMsg":[],"AddRandom":[],"ClearGraph":[],"DragToplevel":["Types.TLID","Mouse.Position"],"RPCCallBack":["Types.Focus","Types.Modification","List Types.RPC","Result.Result Http.Error Types.RPCResult"],"FocusEntry":["Result.Result Dom.Error ()"],"FinishIntegrationTest":[],"LocationChange":["Navigation.Location"],"ToplevelClickDown":["Types.Toplevel","Types.MouseEvent"],"SaveTestButton":[]}},"Dict.NColor":{"args":[],"tags":{"BBlack":[],"Red":[],"NBlack":[],"Black":[]}},"Types.State":{"args":[],"tags":{"Dragging":["Types.TLID","Types.VPos","Types.HasMoved","Types.State"],"Deselected":[],"Entering":["Types.EntryCursor"],"Selecting":["Types.TLID","Maybe.Maybe Types.ID"]}},"Types.TLData":{"args":[],"tags":{"TLHandler":["Types.Handler"],"TLDB":["Types.DB"]}},"Types.AutocompleteMod":{"args":[],"tags":{"ACFilterByLiveValue":["Maybe.Maybe Types.LiveValue"],"ACClear":[],"ACOpen":["Bool"],"ACAppendQuery":["String"],"ACReset":[],"ACSelectUp":[],"ACSetQuery":["String"],"ACSetAvailableVarnames":["List Types.VarName"],"ACShowFunctions":["Bool"],"ACSelectDown":[]}},"Types.Tipe":{"args":[],"tags":{"TChar":[],"TFloat":[],"TInt":[],"TList":[],"TStr":[],"TNull":[],"TBool":[],"TDB":[],"TAny":[],"TIncomplete":[],"TBlock":[],"TResp":[],"TObj":[]}},"Keyboard.Key.Key":{"args":[],"tags":{"Nine":[],"Ambiguous":["List Keyboard.Key.Key"],"L":[],"Five":[],"Multiply":[],"F1":[],"Command":[],"Backspace":[],"Home":[],"Insert":[],"G":[],"W":[],"Down":[],"CapsLock":[],"J":[],"Z":[],"Six":[],"Up":[],"NumLock":[],"PrintScreen":[],"F12":[],"M":[],"NumpadThree":[],"Shift":["Maybe.Maybe Keyboard.Key.Side"],"P":[],"Unknown":["Keyboard.KeyCode"],"F6":[],"NumpadSeven":[],"Enter":[],"K":[],"Left":[],"Seven":[],"N":[],"F8":[],"F11":[],"NumpadZero":[],"F7":[],"A":[],"Ctrl":["Maybe.Maybe Keyboard.Key.Side"],"Escape":[],"PageDown":[],"Q":[],"NumpadSix":[],"Add":[],"F2":[],"Subtract":[],"Two":[],"D":[],"One":[],"NumpadEight":[],"T":[],"End":[],"NumpadOne":[],"Tab":[],"Eight":[],"F9":[],"F10":[],"NumpadFive":[],"NumpadNine":[],"NumpadTwo":[],"Windows":[],"O":[],"B":[],"R":[],"Zero":[],"F4":[],"E":[],"NumpadFour":[],"U":[],"PauseBreak":[],"Spacebar":[],"F3":[],"ChromeSearch":[],"H":[],"X":[],"Right":[],"Divide":[],"F5":[],"Decimal":[],"C":[],"S":[],"PageUp":[],"V":[],"Four":[],"Three":[],"F":[],"ScrollLock":[],"Alt":[],"Y":[],"Delete":[],"I":[]}},"Types.ID":{"args":[],"tags":{"ID":["Int"]}},"Http.Error":{"args":[],"tags":{"BadUrl":["String"],"NetworkError":[],"Timeout":[],"BadStatus":["Http.Response String"],"BadPayload":["String","Http.Response String"]}},"Result.Result":{"args":["error","value"],"tags":{"Ok":["value"],"Err":["error"]}},"Types.HoleOr":{"args":["a"],"tags":{"Empty":["Types.ID"],"Full":["Types.ID","a"]}},"Types.Focus":{"args":[],"tags":{"FocusNext":["Types.TLID","Types.Predecessor"],"FocusExact":["Types.TLID","Types.ID"],"Refocus":["Types.TLID"],"FocusSame":[],"FocusNothing":[]}},"Keyboard.Key.Side":{"args":[],"tags":{"RightHand":[],"LeftHand":[]}}},"aliases":{"Types.VarName":{"args":[],"type":"String"},"Types.FnName":{"args":[],"type":"String"},"Types.HasMoved":{"args":[],"type":"Bool"},"Http.Response":{"args":["body"],"type":"{ url : String , status : { code : Int, message : String } , headers : Dict.Dict String String , body : body }"},"Types.TLAResult":{"args":[],"type":"{ id : Types.TLID , astValue : Types.LiveValue , liveValues : Types.LVDict , availableVarnames : Types.AVDict }"},"Types.Handler":{"args":[],"type":"{ ast : Types.AST, spec : Types.HandlerSpec }"},"Types.Predecessor":{"args":[],"type":"Maybe.Maybe Types.ID"},"Types.Toplevel":{"args":[],"type":"{ id : Types.TLID, pos : Types.Pos, data : Types.TLData }"},"Types.AVDict":{"args":[],"type":"Dict.Dict Int (List Types.VarName)"},"Types.FieldName":{"args":[],"type":"String"},"Types.DB":{"args":[],"type":"{ name : Types.DBName , cols : List ( Types.HoleOr Types.DBColName, Types.HoleOr Types.DBColType ) }"},"Types.AST":{"args":[],"type":"Types.Expr"},"Mouse.Position":{"args":[],"type":"{ x : Int, y : Int }"},"Keyboard.KeyCode":{"args":[],"type":"Int"},"Types.Pos":{"args":[],"type":"{ x : Int, y : Int }"},"Types.DBColName":{"args":[],"type":"String"},"Types.DBName":{"args":[],"type":"String"},"Types.VPos":{"args":[],"type":"{ vx : Int, vy : Int }"},"Types.Field":{"args":[],"type":"Types.HoleOr Types.FieldName"},"Types.Exception":{"args":[],"type":"{ short : String , long : String , tipe : String , actual : String , actualType : String , result : String , resultType : String , expected : String , info : Dict.Dict String String , workarounds : List String }"},"Types.RPCResult":{"args":[],"type":"( List Types.Toplevel, List Types.TLAResult )"},"Types.LVDict":{"args":[],"type":"Dict.Dict Int Types.LiveValue"},"Keyboard.Event.KeyboardEvent":{"args":[],"type":"{ altKey : Bool , ctrlKey : Bool , key : Maybe.Maybe String , keyCode : Keyboard.Key.Key , metaKey : Bool , repeat : Bool , shiftKey : Bool }"},"Types.MouseEvent":{"args":[],"type":"{ pos : Types.VPos, button : Int }"},"Types.LiveValue":{"args":[],"type":"{ value : String , tipe : Types.Tipe , json : String , exc : Maybe.Maybe Types.Exception }"},"Types.HandlerSpec":{"args":[],"type":"{ module_ : Types.HoleOr String , name : Types.HoleOr String , modifier : Types.HoleOr String }"},"Types.DBColType":{"args":[],"type":"String"},"Types.VarBind":{"args":[],"type":"Types.HoleOr Types.VarName"},"Navigation.Location":{"args":[],"type":"{ href : String , host : String , hostname : String , protocol : String , origin : String , port_ : String , pathname : String , search : String , hash : String , username : String , password : String }"}},"message":"Types.Msg"},"versions":{"elm":"0.18.0"}});
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

