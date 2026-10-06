#!/usr/bin/env rexx
/* json2dir: create the directory tree a JSON document describes, in the current directory.
   Implements RFC J2D-1 (https://github.com/kitsunoff/awesome-json2dir/blob/main/spec/rfc-json2dir.md).
   Open Object Rexx 5, classic REXX style: hand-written UTF-8 check and JSON parser,
   file system work through the bundled RexxUtil and rxunixsys libraries only. */

if .syscargs~items > 0 then do
  call lineout 'STDERR', 'usage: json2dir < document.json'
  exit 2
end

signal on syntax name failed

/* read all of standard input, binary-safe */
data = ''
do forever
  chunk = charin('STDIN', , 65536)
  data = data || chunk
  if length(chunk) < 65536 then leave
end

if left(data, 3) == 'EFBBBF'x then data = substr(data, 4)   /* §3: a BOM may be ignored */
call checkUtf8

/* parse: nodes are kept in stems. type.n = 'O' | 'S' | 'L' | 'X' (script) */
nodes = 0
pos = 1
len = length(data)
call skipWs
if substr(data, pos, 1) \== '{' then call fail 'the root of the document must be an object'
root = parseValue('.')
call skipWs
if pos <= len then call fail 'input is not valid JSON: trailing data at byte' pos

/* the whole document is valid now: create the tree */
oldMask = SysUmask(0)
call SysUmask oldMask
fileMode = 438 - bitAndNum(438, oldMask)          /* 0666 & ~umask */
scriptMode = permString(fileMode + 73 - bitAndNum(fileMode, 73))  /* | 0111 */
call apply '.', root
exit 0

failed:
  call fail condition('O')~message

fail:
  call lineout 'STDERR', 'json2dir:' arg(1)
  exit 1

/* ---------------------------------------------------------------- UTF-8 */

checkUtf8:
  ascii = xrange('00'x, '7F'x)
  i = verify(data, ascii)
  do while i > 0
    b = c2d(substr(data, i, 1))
    select
      when b >= 194 & b <= 223 then do; n = 1; lo = 128; hi = 191; end
      when b = 224 then do; n = 2; lo = 160; hi = 191; end
      when b >= 225 & b <= 236 then do; n = 2; lo = 128; hi = 191; end
      when b = 237 then do; n = 2; lo = 128; hi = 159; end
      when b >= 238 & b <= 239 then do; n = 2; lo = 128; hi = 191; end
      when b = 240 then do; n = 3; lo = 144; hi = 191; end
      when b >= 241 & b <= 243 then do; n = 3; lo = 128; hi = 191; end
      when b = 244 then do; n = 3; lo = 128; hi = 143; end
      otherwise call fail 'input is not valid UTF-8'
    end
    if i + n > length(data) then call fail 'input is not valid UTF-8'
    c = c2d(substr(data, i + 1, 1))
    if c < lo | c > hi then call fail 'input is not valid UTF-8'
    do k = 2 to n
      c = c2d(substr(data, i + k, 1))
      if c < 128 | c > 191 then call fail 'input is not valid UTF-8'
    end
    i = verify(data, ascii, , i + n + 1)
  end
  return

/* ---------------------------------------------------------------- JSON */

skipWs:
  pos = verify(data, '20090A0D'x, , pos)
  if pos = 0 then pos = len + 1
  return

parseValue: procedure expose data pos len nodes type. val. cnt. key. kid.
  where = arg(1)
  call skipWs
  ch = substr(data, pos, 1)
  select
    when ch == '{' then return parseObject(where)
    when ch == '"' then do
      nodes = nodes + 1
      type.nodes = 'S'
      val.nodes = parseString()
      return nodes
    end
    when ch == '[' then do
      /* only ["link", target] and ["script", content] mean anything */
      pos = pos + 1
      call skipWs
      if substr(data, pos, 1) \== '"' then call fail where': an array must be ["link", target] or ["script", content]'
      kind = parseString()
      call skipWs
      if substr(data, pos, 1) \== ',' then call fail where': an array must be ["link", target] or ["script", content]'
      pos = pos + 1
      call skipWs
      if substr(data, pos, 1) \== '"' then call fail where': an array must be ["link", target] or ["script", content]'
      payload = parseString()
      call skipWs
      if substr(data, pos, 1) \== ']' then call fail where': an array must be ["link", target] or ["script", content]'
      pos = pos + 1
      nodes = nodes + 1
      select
        when kind == 'link' then do
          /* §10: a link target with NUL cannot be created */
          if pos('00'x, payload) > 0 then call fail where': a link target cannot contain NUL'
          type.nodes = 'L'
        end
        when kind == 'script' then type.nodes = 'X'
        otherwise call fail where': unknown array kind "'kind'"'
      end
      val.nodes = payload
      return nodes
    end
    when ch == '' then call fail 'input is not valid JSON: unexpected end of input'
    otherwise
      if verify(ch, '-0123456789tfn') = 0 then
        call fail where': numbers, true, false and null are not allowed'
      call fail 'input is not valid JSON: unexpected character at byte' pos
  end

parseObject: procedure expose data pos len nodes type. val. cnt. key. kid.
  where = arg(1)
  nodes = nodes + 1
  me = nodes
  type.me = 'O'
  cnt.me = 0
  pos = pos + 1
  call skipWs
  if substr(data, pos, 1) == '}' then do
    pos = pos + 1
    return me
  end
  do forever
    call skipWs
    if substr(data, pos, 1) \== '"' then call fail 'input is not valid JSON: expected a member name at byte' pos
    name = parseString()
    if where == '.' then child = name; else child = where'/'name
    if name == '' | name == '.' | name == '..' | pos('/', name) > 0 | pos('00'x, name) > 0 then
      call fail child': invalid name'
    call skipWs
    if substr(data, pos, 1) \== ':' then call fail 'input is not valid JSON: expected ":" at byte' pos
    pos = pos + 1
    v = parseValue(child)
    /* duplicate names: the last occurrence wins */
    found = 0
    do i = 1 to cnt.me
      if key.me.i == name then do
        kid.me.i = v
        found = 1
        leave
      end
    end
    if \found then do
      i = cnt.me + 1
      cnt.me = i
      key.me.i = name
      kid.me.i = v
    end
    call skipWs
    ch = substr(data, pos, 1)
    pos = pos + 1
    if ch == '}' then leave
    if ch \== ',' then call fail 'input is not valid JSON: expected "," or "}" at byte' (pos - 1)
  end
  return me

/* parse a string at pos (which is at the opening quote); returns its UTF-8 bytes */
parseString: procedure expose data pos len
  pos = pos + 1
  out = ''
  do forever
    stop = verify(data, '"\' || xrange('00'x, '1F'x), 'M', pos)
    if stop = 0 then call fail 'input is not valid JSON: unterminated string'
    out = out || substr(data, pos, stop - pos)
    pos = stop
    ch = substr(data, pos, 1)
    if ch == '"' then do
      pos = pos + 1
      return out
    end
    if ch \== '\' then call fail 'input is not valid JSON: control character in string at byte' pos
    e = substr(data, pos + 1, 1)
    pos = pos + 2
    select
      when e == '"' then out = out || '"'
      when e == '\' then out = out || '\'
      when e == '/' then out = out || '/'
      when e == 'b' then out = out || '08'x
      when e == 'f' then out = out || '0C'x
      when e == 'n' then out = out || '0A'x
      when e == 'r' then out = out || '0D'x
      when e == 't' then out = out || '09'x
      when e == 'u' then do
        cp = hex4()
        if cp >= 55296 & cp <= 56319 then do        /* high surrogate: a low one must follow */
          if substr(data, pos, 2) \== '\u' then call fail 'string contains an unpaired surrogate'
          pos = pos + 2
          lo = hex4()
          if lo < 56320 | lo > 57343 then call fail 'string contains an unpaired surrogate'
          cp = 65536 + (cp - 55296) * 1024 + (lo - 56320)
        end
        else if cp >= 56320 & cp <= 57343 then call fail 'string contains an unpaired surrogate'
        out = out || utf8(cp)
      end
      otherwise call fail 'input is not valid JSON: bad escape at byte' (pos - 2)
    end
  end

hex4: procedure expose data pos
  h = substr(data, pos, 4)
  if length(h) < 4 | verify(h, '0123456789abcdefABCDEF') > 0 then
    call fail 'input is not valid JSON: bad \u escape at byte' pos
  pos = pos + 4
  return x2d(h)

utf8: procedure
  cp = arg(1)
  if cp < 128 then return d2c(cp, 1)
  if cp < 2048 then return d2c(192 + cp % 64) || d2c(128 + cp // 64)
  if cp < 65536 then return d2c(224 + cp % 4096) || d2c(128 + (cp % 64) // 64) || d2c(128 + cp // 64)
  return d2c(240 + cp % 262144) || d2c(128 + (cp % 4096) // 64) || d2c(128 + (cp % 64) // 64) || d2c(128 + cp // 64)

/* ---------------------------------------------------------------- file system */

bitAndNum: procedure
  return c2d(bitand(d2c(arg(1), 2), d2c(arg(2), 2)))

permString: procedure
  m = arg(1)
  s = ''
  letters = 'xwr'
  do i = 0 to 8
    if m // 2 = 1 then s = substr(letters, i // 3 + 1, 1) || s
    else s = '-' || s
    m = m % 2
  end
  return s

/* §5: the entry's state without following a symlink: 'L', 'D', 'F' or '' */
entryKind: procedure
  p = arg(1)
  if SysIsFileLink(p) then return 'L'
  if SysIsFileDirectory(p) then return 'D'
  if SysFileExists(p) then return 'F'
  return ''

clear: procedure
  p = arg(1)
  kind = entryKind(p)
  if kind == 'D' then call fail p': a directory is in the way'
  if kind \== '' then
    if SysUnlink(p) \= 0 then call fail p': cannot remove the existing entry'
  return

writeFile: procedure
  p = arg(1); content = arg(2)
  if stream(p, 'C', 'OPEN WRITE REPLACE') \== 'READY:' then call fail p': cannot create file'
  if content \== '' then
    if charout(p, content) \= 0 then call fail p': cannot write file'
  call stream p, 'C', 'CLOSE'
  return

/* members in ascending order of the UTF-8 bytes of their names, like the reference */
apply: procedure expose type. val. cnt. key. kid. scriptMode
  dir = arg(1); n = arg(2)
  m = cnt.n
  do i = 1 to m
    order.i = i
  end
  do i = 2 to m
    t = order.i
    j = i - 1
    do while j >= 1
      o = order.j
      if \(key.n.t << key.n.o) then leave
      k = j + 1
      order.k = o
      j = j - 1
    end
    k = j + 1
    order.k = t
  end
  do i = 1 to m
    o = order.i
    p = dir'/'key.n.o
    c = kid.n.o
    select
      when type.c == 'S' then do
        call clear p
        call writeFile p, val.c
      end
      when type.c == 'X' then do
        call clear p
        call writeFile p, val.c
        if SysChmod(p, scriptMode) \= 0 then call fail p': cannot make file executable'
      end
      when type.c == 'L' then do
        call clear p
        if SysSymlink(val.c, p) \= 0 then call fail p': cannot create symbolic link'
      end
      otherwise
        kind = entryKind(p)
        if kind \== 'D' then do
          if kind \== '' then
            if SysUnlink(p) \= 0 then call fail p': cannot remove the existing entry'
          if SysMkDir(p) \= 0 then call fail p': cannot create directory'
        end
        call apply p, c
    end
  end
  return

::requires 'rxunixsys' LIBRARY
