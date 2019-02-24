/**
 * @fileoverview prettier-el main server process
 */

// Copyright (c) 2018-present Julian Scheid

// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// Hack to circumvent Closure Compiler CommonJS resolution
const externalRequire = require;

const fs = externalRequire("fs");
const path = externalRequire("path");
const vm = externalRequire("vm");
const execSync = externalRequire("child_process")["execSync"];

// We return this exit code whenever there's unexpected data on the wire
const EXIT_CODE_PROTOCOL_ERROR = 1;

// Keep track of loaded Prettier instances by file path.
const prettierCache = new Map();

const Z = createResponseHeader("Z", 0);
const newline = Buffer.from("\n");

const otherParserName = new Map([["babylon", "babel"], ["babel", "babylon"]]);

/** @type{PrettierAPI} */
let globalPrettier;

/**
 * Slurp the given number of bytes from the given file descriptor, return a
 * buffer.
 *
 * @param {!number} fd File descriptor to read from.
 * @param {!number} numBytes Number of bytes to read.
 *
 * @return {!Buffer} The data read.
 */
function readFully(fd, numBytes) {
  const buf = Buffer.alloc(numBytes);
  let offset = 0;
  while (offset < numBytes) {
    const numRead = fs["readSync"](fd, buf, offset, numBytes - offset, null);
    if (numRead <= 0) {
      throw new Error("EOF");
    }
    offset += numRead;
  }
  return buf;
}

/**
 * Bail out when a protocol error occurs.
 */
function protocolError() {
  process.exit(EXIT_CODE_PROTOCOL_ERROR);
}

/**
 * @param{!string} chr
 * @param{!number} val
 * @return{!Buffer}
 */
function createResponseHeader(chr, val) {
  return Buffer.from(chr + val.toString(16) + "\n");
}

/**
 * Return a Buffer containing the given string encoded as Base64, split into
 * chunks to ensure no line is too long.
 *
 * @param{!string} str
 * @return{!Buffer}
 */
function createBase64Buffer(str) {
  return Buffer.from(
    Buffer.from(str)
      .toString("base64")
      .match(/.{1,64}/g)
      .join("\n")
      .trim()
  );
}

/**
 * Return a Buffer containing the given number as an unsigned 32-bit integer,
 * little-endian encoded.
 *
 * @param{!number} val
 * @return{!Buffer}
 */
function makeU32(val) {
  const buf = Buffer.alloc(4);
  buf.writeUInt32LE(val, 0);
  return buf;
}

/**
 * Find a globally installed Prettier. Throw if not found. Memoize results for
 * future lookups.
 *
 * @return {!PrettierAPI}
 */
function getGlobalPrettier() {
  if (globalPrettier) {
    return globalPrettier;
  }

  let npmGlobalPath;
  let yarnGlobalPath;

  const execSyncOptions = {
    ["encoding"]: "utf-8",
    ["stdio"]: ["ignore", "pipe", "ignore"]
  };

  try {
    npmGlobalPath = path["join"](
      execSync("npm root -g", execSyncOptions).trim(),
      "prettier"
    );
  } catch (e) {
    // ignore
  }

  try {
    yarnGlobalPath = path["join"](
      execSync("yarn global dir", execSyncOptions).trim(),
      "node_modules",
      "prettier"
    );
  } catch (e) {
    // ignore
  }

  const pathOptions = ["prettier", npmGlobalPath, yarnGlobalPath];

  for (let i = 0; i < pathOptions.length; ++i) {
    if (pathOptions[i]) {
      try {
        globalPrettier = externalRequire(pathOptions[i]);
        break;
      } catch (e) {
        if (!(e instanceof Error) || e["code"] !== "MODULE_NOT_FOUND") {
          throw e;
        }
      }
    }
  }

  if (globalPrettier) {
    return globalPrettier;
  } else {
    throw new Error("Cannot find prettier anywhere");
  }
}

/**
 * Find the Prettier package to use for the given directory, falling back to a
 * global package. Throw if neither is found. Memoize results for future
 * lookups.
 *
 * @param {!string} directory The directory for which to find the Prettier
 *    package.
 *
 * @return {!PrettierAPI} The Prettier package found.
 */
function getPrettierForDirectory(directory) {
  if (fs["existsSync"](path["join"](directory, "package.json"))) {
    const prettierCandidate = path["join"](
      directory,
      "node_modules",
      "prettier"
    );
    try {
      const stat = fs["statSync"](prettierCandidate);
      prettierCache.set(
        directory,
        stat.isDirectory()
          ? externalRequire(prettierCandidate)
          : getGlobalPrettier()
      );
    } catch (e) {
      if (e.code === "ENOENT") {
        return getGlobalPrettier();
      }
    }
  } else {
    const parent = path["dirname"](directory);
    prettierCache.set(
      directory,
      parent !== directory
        ? getPrettierForDirectory(parent)
        : getGlobalPrettier()
    );
  }
  return prettierCache.get(directory);
}

/**
 * Find the Prettier package to use for the given file, falling back
 * to a global package. Throw if neither is found. Memoize results
 * for future lookups.
 *
 * @param {!string} filepath
 *
 * @return {!PrettierAPI} The Prettier package found.
 */
function getPrettierForPath(filepath) {
  if (!path["isAbsolute"](filepath)) {
    return getGlobalPrettier();
  }
  let prettierPath = prettierCache.get(filepath);
  if (!prettierPath) {
    const directory = path["dirname"](filepath);
    prettierPath = getPrettierForDirectory(directory);
    prettierCache.set(filepath, prettierPath);
  }
  return prettierPath;
}

function parseParsers(parsersString) {
  return parsersString === "-"
    ? null
    : parsersString
        .split(",")
        .reduce(
          (accu, parser) =>
            accu.concat(parser === "babel" ? ["babel", "babylon"] : [parser]),
          []
        );
}

function bestParser(prettier, parsers, options, filepath) {
  if (parsers !== null) {
    const supportedParsers = prettier.getSupportInfo
      ? prettier
          .getSupportInfo()
          ["languages"].reduce((accu, lang) => accu.concat(lang["parsers"]), [])
      : [];
    const result = parsers.find(parser => supportedParsers.includes(parser));
    if (result) {
      return result;
    }
  }
  if (filepath) {
    return prettier["getFileInfo"].sync(filepath, null)["inferredParser"];
  }
  return null;
}

/** @suppress {uselessCode} */ (baseScript, cacheFilename, inp) => {
  const diff = require("./node_modules/diff-match-patch/index.js");

  /**
   * Write an error response item.
   *
   * @param {!Error} err
   */
  function writeError(err) {
    const errBuf = createBase64Buffer(err.toString());

    process.stdout.write(
      Buffer.concat([
        createResponseHeader("E", errBuf.length),
        errBuf,
        newline,
        Z
      ])
    );
  }

  /**
   * Handle a request to warm up the engines for a given file.
   *
   * @param {!Buffer} packet
   */
  function handleWarmup(packet) {
    try {
      const newlineIndex1 = packet.indexOf(10);
      if (newlineIndex1 < 0) {
        protocolError();
      }

      const editorconfig = packet[1] === "E".charCodeAt(0);
      const filepath = packet.toString("utf-8", 2, newlineIndex1);
      const prettier = getPrettierForPath(filepath);
      if (filepath.length > 0) {
        prettier.resolveConfig.sync(filepath, {
          editorconfig
        });
      }
    } catch (e) {
      // ignore this -- the client isn't waiting for our response and
      // we have nowhere else to report it.
    }
  }

  /**
   * Handle a request for formatting a file.
   *
   * @param {!Buffer} packet
   */
  function handleFormat(packet) {
    const stripTrailing = packet[1] === "S".charCodeAt(0);
    const editorconfig = packet[2] === "E".charCodeAt(0);
    const inferParser = packet[3] === "I".charCodeAt(0);
    const newlineIndex1 = packet.indexOf(10);
    if (newlineIndex1 < 0) {
      protocolError();
    }
    const filepath = packet.toString("utf-8", 4, newlineIndex1);
    const newlineIndex2 = packet.indexOf(10, newlineIndex1 + 1);
    if (newlineIndex2 < 0) {
      protocolError();
    }

    const parsersString = packet.toString(
      "utf-8",
      newlineIndex1 + 1,
      newlineIndex2
    );
    const newlineIndex3 = packet.indexOf(10, newlineIndex2 + 1);
    if (newlineIndex3 < 0) {
      protocolError();
    }

    const cursorOffset = parseInt(
      packet.toString("ascii", newlineIndex2 + 1, newlineIndex3),
      16
    );
    const filename = packet
      .slice(newlineIndex3 + 1, packet.length - 2)
      .toString("ascii");

    const body = fs["readFileSync"](filename, "utf8");

    try {
      const prettier = getPrettierForPath(filepath);

      const timeBeforeFormat = Date.now();

      let options = {};
      if (path["isAbsolute"](filepath)) {
        options =
          prettier.resolveConfig.sync(filepath, {
            editorconfig
          }) || {};
      }

      const parsers = parseParsers(parsersString);
      const parser = bestParser(
        prettier,
        parsers,
        options,
        inferParser ? filepath : null
      );

      const out = [];

      const prettierVersion = createBase64Buffer(prettier.version);
      const parserBuf = createBase64Buffer(parser || "none");
      out.push(createResponseHeader("P", parserBuf.length), parserBuf, newline);
      out.push(
        createResponseHeader("V", prettierVersion.length),
        prettierVersion,
        newline
      );

      if (inferParser && !parser) {
        out.push(Z);
        process.stdout.write(Buffer.concat(out));
        return;
      }

      options["cursorOffset"] = cursorOffset;
      options["filepath"] = filepath;
      options["rangeStart"] = undefined;
      options["rangeEnd"] = undefined;
      options["parser"] = parser;

      const result = prettier.formatWithCursor(body, options);

      const timeAfterFormat = Date.now();
      const diffResult = new diff().diff_main(body, result["formatted"]);

      for (let index = 0; index < diffResult.length; index++) {
        const [kind, str] = diffResult[index];
        switch (kind) {
          case 1:
            {
              const insertStr =
                stripTrailing && index === diffResult.length - 1
                  ? str.replace(/[ \n]+$/g, "")
                  : str;
              if (insertStr.length > 0) {
                const strBuf = createBase64Buffer(insertStr);
                out.push(
                  createResponseHeader("I", strBuf.length),
                  strBuf,
                  newline
                );
              }
            }
            break;
          case -1:
            out.push(createResponseHeader("D", str.length));
            break;
          case 0:
            if (index < diffResult.length - 1) {
              out.push(createResponseHeader("M", str.length));
            }
        }
      }
      out.push(createResponseHeader("T", timeAfterFormat));
      out.push(createResponseHeader("T", timeBeforeFormat));

      if (Number.isFinite(result["cursorOffset"])) {
        out.push(createResponseHeader("C", result["cursorOffset"]));
      }
      out.push(Z);
      process.stdout.write(Buffer.concat(out));
    } catch (e) {
      writeError(e);
    }
  }

  /**
   * Handle a request for options.
   *
   * - Extract the filepath from the packet
   *
   * - Find the corresponding Prettier version
   *
   * - Resolve Prettier configuration for the file
   *
   * - Respond with an object containing the Prettier configuration along with
   *   miscellaneous info, serialized to JSON, encoded as Base 64.
   *
   * @param {!Buffer} packet
   */
  function handleOptions(packet) {
    try {
      const newlineIndex1 = packet.indexOf(10);
      if (newlineIndex1 < 0) {
        protocolError();
      }
      const editorconfig = packet[1] === "E".charCodeAt(0);
      const inferParser = packet[2] === "I".charCodeAt(0);
      const filepath = packet.toString("utf-8", 3, newlineIndex1);

      const newlineIndex2 = packet.indexOf(10, newlineIndex1 + 1);
      if (newlineIndex2 < 0) {
        protocolError();
      }

      const parsersString = packet.toString(
        "utf-8",
        newlineIndex1 + 1,
        newlineIndex2
      );
      const parsers = parseParsers(parsersString);

      const prettier = getPrettierForPath(filepath);

      const options =
        prettier.resolveConfig.sync(filepath, { editorconfig }) || {};

      let optionsStr;
      options["parser"] = function(_text, _parsers, options) {
        optionsStr = JSON.stringify({
          ["versions"]: Object.assign({}, process["versions"], {
            ["prettier"]: prettier.version
          }),
          ["options"]: options,
          ["bestParser"]: bestParser(
            prettier,
            parsers,
            options,
            inferParser ? filepath : null
          )
        });
        return { type: "NullLiteral" };
      };

      prettier.format(".", options);

      const optionsBuf = createBase64Buffer(optionsStr);
      process.stdout.write(
        Buffer.concat([
          createResponseHeader("O", optionsBuf.length),
          optionsBuf,
          newline,
          Z
        ])
      );
    } catch (e) {
      writeError(e);
    }
  }

  /**
   * Handle a data packet -- a stream of bytes received on stdin, ended by a
   * double linefeed.
   *
   * @param {!Array<!Buffer>} packetBuffers is an array of Buffers that make up
   *   the packet when concatenated.
   */
  function handlePacket(packetBuffers) {
    const packet = Buffer.concat(packetBuffers);
    if (packet[0] === "f".charCodeAt(0)) {
      handleFormat(packet);
    } else if (packet[0] === "o".charCodeAt(0)) {
      handleOptions(packet);
    } else if (packet[0] === "w".charCodeAt(0)) {
      handleWarmup(packet);
    } else {
      protocolError();
    }
  }

  /*
   * Main loop: receive data from stdin; when a double newline is received, pass
   * all data received so far to `handlePacket`.
   */
  const buffers = [];
  process.stdin["on"]("data", slice => {
    while (slice.length > 0) {
      const lastBuffer =
        buffers.length > 0 ? buffers[buffers.length - 1] : null;
      if (
        lastBuffer &&
        lastBuffer[lastBuffer.length - 1] == 10 &&
        slice[0] == 10
      ) {
        buffers.push(slice.slice(0, 1));
        slice = slice.slice(1);
        handlePacket(buffers);
        buffers.length = 0;
      } else {
        const index = slice.indexOf("\n\n");
        if (index < 0) {
          buffers.push(slice);
          break;
        } else {
          buffers.push(slice.slice(0, index + 2));
          slice = slice.slice(index + 2);
          handlePacket(buffers);
          buffers.length = 0;
        }
      }
    }
  });
};
