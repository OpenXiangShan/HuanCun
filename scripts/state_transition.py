import itertools
import inspect
from enum import Enum, unique

@unique
class Block(Enum):
  NULL = 0
  F = 1
  G = 2
  H = 3
  I = 4

@unique
class TLState(Enum):
  INVALID = 0
  BRANCH = 1
  TRUNK = 2
  TIP = 3

@unique
class DirtyState(Enum):
  CLEAN = 0
  DIRTY = 1

@unique
class HitState(Enum):
  MISS = 0
  HIT = 1

class ClientDir:
  def __init__(self, tl_state, hit_state):
    self.tl_state = tl_state
    self.hit_state = hit_state
  def __str__(self):
    return (f"{self.tl_state} {self.hit_state}")

class SelfDir:
  def __init__(self, tl_state, dirty_state, hit_state, client_tl_states):
    self.tl_state = tl_state
    self.dirty_state = dirty_state
    self.hit_state = hit_state
    self.client_tl_states = client_tl_states
  def __str__(self):
    client_str = ""
    for i, s in enumerate(self.client_tl_states):
      client_str += f"Client {i}: {s}\n"
    return (f"{client_str}Self: {self.tl_state} {self.dirty_state} {self.hit_state}")

class BlockState:

  def __init__(self, req_block, self_block, client_blocks):
    self.req_block = req_block
    self.self_block = self_block
    self.client_blocks = client_blocks

  def __str__(self):
    req_str = f"req: {self.req_block}\n"
    client_str = ""
    for i, b in enumerate(self.client_blocks):
      client_str += f"Client {i}: {b}\n"
    self_str = f"Self: {self.self_block}\n"
    return req_str + client_str + self_str

class DirState:
  def __init__(self, self_dir, client_dirs, block_state):
    self.self_dir = self_dir
    self.client_dirs = client_dirs
    self.block_state = block_state
  def __str__(self):
    delim = "---------------------------------------------------\n"
    client_str = "Client Dir:\n"
    for i, c in enumerate(self.client_dirs):
      client_str += f"Client {i}: {c}\n"
    self_str = "Self Dir:\n" + str(self.self_dir) + "\n"
    block_str = f"BlockState:\n{self.block_state}"
    return delim + client_str + self_str + block_str + delim

def get_all_block_states():
  block_states = []
  blocks = list(Block)[1:] # NULL block is special
  def dfs(idx, acc):
    if idx == len(blocks):
      req_block, self_block = acc[:2]
      client_blocks = acc[2:]
      s = BlockState(req_block, self_block, client_blocks)
      block_states.append(s)
      return
    for i in range(0, idx+1):
      acc_next = acc.copy()
      acc_next.append(blocks[i])
      dfs(idx+1, acc_next)
    if i != 0:
      acc_next = acc.copy()
      acc_next.append(Block.NULL)
      dfs(idx+1, acc_next)
  dfs(0, [])
  # for s in block_states:
  #   print(s)
  # print(len(block_states))
  return block_states

NUM_CLIENTS = 2

self_fields = [
  list(TLState),
  list(DirtyState),
  list(HitState),
  get_all_block_states()
]

client_fields = []

for i in range(0, NUM_CLIENTS):
  self_fields.append(list(TLState))
  client_fields.append(list(TLState))
  client_fields.append(list(HitState))

def get_all_states(log = False):
  all_states = []
  for x in itertools.product(*self_fields):
    self_state, dirty, hit, block_state = x[:4]
    self_clients = x[4:]
    self_dir = SelfDir(self_state, dirty, hit, self_clients)
    for c in itertools.product(*client_fields):
      client_dirs = [ ClientDir(c[i], c[i+1]) for i in range(0, len(c), 2) ]
      new_state = DirState(self_dir, client_dirs, block_state)
      if log:
        print(new_state)
      all_states.append(new_state)
  if log:
    print(f"states: {len(all_states)}")
  return all_states

all_states = get_all_states()
print(f"all states: {len(all_states)}")

def invalid_filter(s):
  if s.self_dir.tl_state != TLState.INVALID and s.block_state.self_block == Block.NULL:
    return False
  for c, b in zip(s.client_dirs, s.block_state.client_blocks):
    if c.tl_state != TLState.INVALID and b == Block.NULL:
      return False
  return True

def hit_filter(s):
  if s.self_dir.hit_state == HitState.HIT:
    if s.block_state.self_block == Block.NULL:
      return False
    if s.self_dir.tl_state == TLState.INVALID:
      return False
  for c, b in zip(s.client_dirs, s.block_state.client_blocks):
    if c.hit_state == HitState.HIT:
      if b == Block.NULL:
        return False
      if c.tl_state == TLState.INVALID:
        return False
  return True

def retrieve_name(var):
  for fi in reversed(inspect.stack()):
    names = [var_name for var_name, var_val in fi.frame.f_locals.items() if var_val is var]
    if len(names) > 0:
      return names[0]

filters = [
  invalid_filter,
  hit_filter
]

for f in filters:
  all_states = list(filter(f, all_states))
  print(f"filter: {retrieve_name(f)} states: {len(all_states)}")

