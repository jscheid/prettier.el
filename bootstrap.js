/**
 * @fileoverview prettier.el bootstrap script
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

const nodeVersion = process["version"];
if (Number(nodeVersion["split"](/[v.]/)[1]) < 6) {
  process.exit(3);
}

const externalRequire = require;

const gunzipSync = externalRequire("zlib")["gunzipSync"];
const vm = externalRequire("vm");
const fs = externalRequire("fs");

function readFully(fd, size) {
  const buf = Buffer["alloc"](size);
  let offset = 0;
  while (offset < size) {
    const numRead = fs["readSync"](fd, buf, offset, size - offset, null);
    if (numRead <= 0) {
      throw new Error("EOF");
    }
    offset += numRead;
  }
  return buf;
}

const script = new vm["Script"](
  gunzipSync(
    Buffer.from(
      readFully(0, Number(process["argv"][1])).toString("ascii"),
      "base64"
    )
  ).toString("utf-8")
);

script["runInThisContext"]()(script);
