#!/usr/bin/env python3
"""Regenerate the chunked `instance Bin Flags` block in GenABin.hs so its
positional arity matches the current Flags record. Also resolves any
conflict markers in the file first: tag hunk -> ours (canonical mid-assembly
tag), instance hunks -> ours (the chunked form), then regenerates the block.
Usage: python3 rc3-genflags.py [--tag bsc-ba-20260618-1]
"""
import re, sys, subprocess

TAG = 'bsc-ba-20260618-1'
if '--tag' in sys.argv:
    TAG = sys.argv[sys.argv.index('--tag') + 1]

P = 'src/comp/GenABin.hs'
CHUNK = 15

# current Flags record arity
flags_src = open('src/comp/Flags.hs').read()
m = re.search(r'^data Flags = Flags \{(.*?)\n\s*\}', flags_src, re.S | re.M)
n = len(re.findall(r'^\s+[a-zA-Z_0-9]+\s*::', m.group(1), re.M))

s = open(P).read()

# resolve tag hunk -> ours form with canonical tag
s = re.sub(r'<<<<<<< HEAD\nheader = B\.unpack \$ TE\.encodeUtf8 \$ T\.pack "[^"]+"\n\nheaderBS :: B\.ByteString\nheaderBS = B\.pack header\n=======\nheader = B\.unpack \$ TE\.encodeUtf8 \$ T\.pack "[^"]+"\n>>>>>>> [^\n]+\n',
           f'header = B.unpack $ TE.encodeUtf8 $ T.pack "{TAG}"\n\nheaderBS :: B.ByteString\nheaderBS = B.pack header\n', s)
# resolve any remaining hunks -> ours
s = re.sub(r'<<<<<<< HEAD\n(.*?)=======\n.*?>>>>>>> [^\n]+\n', r'\1', s, flags=re.S)

def names(lo, hi):
    return [f'a_{i:03d}' for i in range(lo, hi)]

def wrap(items, per, indent):
    out = []
    for i in range(0, len(items), per):
        out.append(indent + ' '.join(items[i:i+per]))
    return '\n'.join(out)

nch = (n + CHUNK - 1) // CHUNK
pat = wrap(names(0, n), 10, '                ')
calls_w = wrap([f'wr_chunk{i};' for i in range(nch - 1)] + [f'wr_chunk{nch-1}'], 5, '          ')

wr_chunks = []
for c in range(nch):
    lo, hi = c * CHUNK, min((c + 1) * CHUNK, n)
    body = []
    ns = names(lo, hi)
    for i in range(0, len(ns), 5):
        line = ' '.join(f'toBin {x};' for x in ns[i:i+5])
        body.append('             ' + line)
    body_s = '\n'.join(body).rstrip(';').rstrip()
    if body_s.endswith(';'):
        body_s = body_s[:-1]
    wr_chunks.append(f'        {{-# NOINLINE wr_chunk{c} #-}}\n        wr_chunk{c} =\n          do\n{body_s}')

rd_binds = []
for c in range(nch):
    lo, hi = c * CHUNK, min((c + 1) * CHUNK, n)
    tup = wrap([x + ',' for x in names(lo, hi)], 8, '           ').rstrip(',')
    tup = tup.replace('(', '').rstrip(',')
    rd_binds.append('          (' + tup.lstrip() + f') <- rd_chunk{c}')

rd_chunks = []
for c in range(nch):
    lo, hi = c * CHUNK, min((c + 1) * CHUNK, n)
    ns = names(lo, hi)
    body = []
    for i in range(0, len(ns), 5):
        body.append('             ' + ' '.join(f'{x} <- fromBin;' for x in ns[i:i+5]))
    body_s = '\n'.join(body)
    body_s = body_s.rstrip()[:-1] if body_s.rstrip().endswith(';') else body_s
    tup = wrap([x + ',' for x in ns], 8, '                     ').rstrip(',')
    rd_chunks.append(f'        {{-# NOINLINE rd_chunk{c} #-}}\n        rd_chunk{c} =\n          do\n{body_s}\n             return ({tup.lstrip()})')

cons = wrap(names(0, n), 10, '                ')

block = f'''instance Bin Flags where
    writeBytes (Flags
{pat}) =
       do
{calls_w}
      where
        -- The {n}-field serialization is split into NOINLINE chunks so
        -- that GHC optimizes bounded pieces: compiling it as a single
        -- monadic chain needs more than 15GB of heap.
{chr(10).join(wr_chunks)}
    readBytes =
       do
{chr(10).join(rd_binds)}
          return (Flags
{cons})
      where
{chr(10).join(rd_chunks)}
'''

m2 = re.search(r'instance Bin Flags where\n.*?\n(?=\n-- -{4,})', s, re.S)
if not m2:
    sys.exit('could not locate instance Bin Flags block')
s = s[:m2.start()] + block + s[m2.end():]
open(P, 'w').write(s)
print(f'regenerated Bin Flags: {n} fields, {nch} chunks')
