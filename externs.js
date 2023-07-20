var process = {
  exit: function() {},
  env: {},
  version: {},
  stdin: {},
  stdout: {}
};
// var Buffer = {
//   from: function() {},
//   alloc: function() {},
//   concat: function() {}
// };
var global;
var console;
function require(name) {}

/**
 * @interface
 */
function Buffer() {}

/**
 * @param {!string} str
 * @param {?string=} encoding
 */
Buffer.from = function(str, encoding) {};
Buffer.alloc = function(size) {};
Buffer.concat = function(bufs) {};

/**
 * @override
 * @suppress{checkTypes}
 * @param{!string} encoding
 * @param{number=} ofs
 * @param{number=} len
 */
Buffer.prototype.toString = function(encoding, ofs, len) {};

/**
 * @param{!number} chr
 * @param{number=} ofs
 */
Buffer.prototype.indexOf = function(chr, ofs) {};
Buffer.prototype.slice = function(ofs, len) {};
Buffer.prototype.length;
Buffer.prototype.writeUInt32LE = function(val, ofs) {};

/**
 * @interface
 */
function PrettierAPI() {}

PrettierAPI.prototype.resolveConfig = function(filePath, options, callback) {};

/**
 * @param {!string} filePath
 * @param {?{editorconfig: (boolean|undefined), useCache: (boolean|undefined)}=} options
 */
PrettierAPI.prototype.resolveConfig.sync = function(filePath, options) {};
PrettierAPI.prototype.format = function(body, options) {};
PrettierAPI.prototype.getSupportInfo = function() {};
PrettierAPI.prototype.getFileInfo = function(filePath, options, callback) {};
PrettierAPI.prototype.getFileInfo.sync = function(filePath, options) {};
/** @type {string} */
PrettierAPI.prototype.version;
