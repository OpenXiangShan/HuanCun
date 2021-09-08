from functools import reduce
from typing import List

import graphviz
import itertools
import inspect
from enum import Enum, unique
import random


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
    def __init__(self, tl_state, hit_state, block):
        self.tl_state = tl_state
        self.hit_state = hit_state
        self.block = block

    def __eq__(self, other):
        tl_eq = self.tl_state == other.tl_state
        hit_eq = self.hit_state == other.hit_state
        block_eq = self.block == other.block
        return tl_eq and hit_eq and block_eq

    def __str__(self):
        return f"{self.tl_state} {self.hit_state} {self.block}"


class SelfDir:
    def __init__(self, tl_state, dirty_state, hit_state, client_tl_states, block):
        self.tl_state = tl_state
        self.dirty_state = dirty_state
        self.hit_state = hit_state
        self.client_tl_states = client_tl_states
        self.block = block

    def __eq__(self, other):
        tl_eq = self.tl_state == other.tl_state
        dirty_eq = self.dirty_state == other.dirty_state
        hit_eq = self.hit_state == other.hit_state
        block_eq = self.block == other.block
        clients_eq = reduce(lambda a, b: a and b,
                            map(lambda a, b: a == b, self.client_tl_states, other.client_tl_states)
                            )
        return tl_eq and dirty_eq and hit_eq and block_eq and clients_eq

    def __str__(self):
        client_str = ""
        for i, s in enumerate(self.client_tl_states):
            client_str += f"Client {i}: {s}\n"
        return f"{client_str}Self: {self.tl_state} {self.dirty_state} {self.hit_state} {self.block}"


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
    def __init__(self, self_dir, client_dirs):
        self.self_dir = self_dir
        self.client_dirs = client_dirs
        self.id = -1

    def __eq__(self, other):
        self_eq = self.self_dir == other.self_dir
        clients_eq = reduce(lambda a, b: a and b,
                            map(lambda a, b: a == b, self.client_dirs, other.client_dirs)
                            )
        return self_eq and clients_eq

    def __str__(self):
        delim = "---------------------------------------------------\n"
        client_str = "Client Dir:\n"
        for i, c in enumerate(self.client_dirs):
            client_str += f"Client {i}: {c}\n"
        self_str = "Self Dir:\n" + str(self.self_dir) + "\n"
        return delim + client_str + self_str + delim


def get_all_block_states():
    block_states = []
    blocks = list(Block)[1:]  # NULL block is special

    def dfs(idx, acc):
        if len(acc) == len(blocks):
            req_block, self_block = acc[:2]
            client_blocks = acc[2:]
            s = BlockState(req_block, self_block, client_blocks)
            block_states.append(s)
            return
        if idx != 0:
            acc_next = acc.copy()
            acc_next.append(Block.NULL)
            dfs(idx, acc_next)
        for i in range(0, idx + 1):
            acc_next = acc.copy()
            acc_next.append(blocks[i])
            if i == idx:
                dfs(idx + 1, acc_next)
            else:
                dfs(idx, acc_next)

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


def get_all_states(log=False):
    all_states = []
    for x in itertools.product(*self_fields):
        self_state, dirty, hit, block_state = x[:4]
        self_clients = x[4:]
        self_dir = SelfDir(self_state, dirty, hit, self_clients, block_state.self_block)
        for c in itertools.product(*client_fields):
            client_dirs = [ClientDir(c[i], c[i + 1], block_state.client_blocks[i // 2]) for i in range(0, len(c), 2)]
            new_state = DirState(self_dir, client_dirs)
            if log:
                print(new_state)
            all_states.append(new_state)
    if log:
        print(f"states: {len(all_states)}")
    return all_states


all_states = get_all_states()

print(f"all states: {len(all_states)}")


def lookup_state_id(s: DirState, lst: List[DirState]):
    for x in lst:
        if x == s:
            return x.id
    return -1


def invalid_filter(s: DirState):
    if s.self_dir.tl_state != TLState.INVALID and s.self_dir.block == Block.NULL:
        return False
    if s.self_dir.tl_state == TLState.INVALID and s.self_dir.block != Block.NULL:
        return False
    for c in s.client_dirs:
        if c.tl_state != TLState.INVALID and c.block == Block.NULL:
            return False
        if c.tl_state == TLState.INVALID and c.block != Block.NULL:
            return False
    return True


def hit_filter(s: DirState):
    if s.self_dir.hit_state == HitState.HIT:
        # F is always req block
        if s.self_dir.block != Block.F:
            return False
        if s.self_dir.tl_state == TLState.INVALID:
            return False
    for c in s.client_dirs:
        if c.hit_state == HitState.HIT:
            if c.block != Block.F:
                return False
            if c.tl_state == TLState.INVALID:
                return False
    return True


def miss_filter(s: DirState):
    if s.self_dir.hit_state == HitState.MISS:
        if s.self_dir.block == Block.F:
            return False
    for c in s.client_dirs:
        if c.hit_state == HitState.MISS:
            if c.block == Block.F:
                return False
    return True


def tilelink_filter(s: DirState):
    # no Trunk in client dir
    for c in s.client_dirs:
        if c.tl_state == TLState.TRUNK:
            return False
    for c in s.self_dir.client_tl_states:
        if c == TLState.TRUNK:
            return False

    # no Branch on top of Trunk; there must be Tip on top of Trunk
    if s.self_dir.tl_state == TLState.TRUNK:
        for c in s.client_dirs:
            if c.block == s.self_dir.block and (c.tl_state == TLState.BRANCH):
                return False
        tipflag = False
        for c in s.self_dir.client_tl_states:
            if c == TLState.BRANCH:
                return False
            if c == TLState.TIP:
                tipflag = True
        if tipflag == False:
            return False

    # no Tip on top of Tip
    if s.self_dir.tl_state == TLState.TIP:
        for c in s.client_dirs:
            if c.block == s.self_dir.block and c.tl_state == TLState.TIP:
                return False
        for c in s.self_dir.client_tl_states:
            if c == TLState.TIP:
                return False

    # one Tip at most; Tip repels Branch
    for i in range(0, len(s.client_dirs) - 1):
        for j in range(i + 1, len(s.client_dirs)):
            if s.client_dirs[i].block == s.client_dirs[j].block:
                if s.client_dirs[i].tl_state == TLState.TIP and s.client_dirs[j].tl_state == TLState.TIP:
                    return False
                if s.client_dirs[i].tl_state == TLState.TIP and s.client_dirs[j].tl_state == TLState.BRANCH:
                    return False
                if s.client_dirs[i].tl_state == TLState.BRANCH and s.client_dirs[j].tl_state == TLState.TIP:
                    return False

            if s.self_dir.client_tl_states[i] == TLState.TIP and s.self_dir.client_tl_states[j] == TLState.TIP:
                return False
            if s.self_dir.client_tl_states[i] == TLState.TIP and s.self_dir.client_tl_states[j] == TLState.BRANCH:
                return False
            if s.self_dir.client_tl_states[i] == TLState.BRANCH and s.self_dir.client_tl_states[j] == TLState.TIP:
                return False

	# Branch cannot be dirty
    if s.self_dir.tl_state == TLState.BRANCH and s.self_dir.dirty_state == DirtyState.DIRTY:
        return False

    return True


def client_miss_filter(s: DirState):
    for c in s.client_dirs:
        if c.hit_state == HitState.MISS and c.tl_state != TLState.INVALID:
            return False
    return True


def self_client_filter(s: DirState):
    if s.self_dir.hit_state == HitState.HIT:
        for i in range(len(s.client_dirs)):
            if s.self_dir.client_tl_states[i] != s.client_dirs[i].tl_state:
                return False
    return True


# if a block is NULL, we don't care:
#   1 dirty or clean
#   2 client state
def null_block_filter(s: DirState):
    if s.self_dir.block == Block.NULL:
        if s.self_dir.dirty_state != DirtyState.CLEAN:
            return False
        for c in s.self_dir.client_tl_states:
            if c != TLState.INVALID:
                return False
    return True

def acquireToB_filter(s):
	if s.client_dirs[0].block == Block.F and s.client_dirs[0].tl_state != TLState.INVALID:
		return False
	return True

def retrieve_name(var):
    for fi in reversed(inspect.stack()):
        names = [var_name for var_name, var_val in fi.frame.f_locals.items() if var_val is var]
        if len(names) > 0:
            return names[0]

def visualize(states):
    dot = graphviz.Digraph()
    for i, s in enumerate(states):
        dot.node(str(i), str(s))
    dot.render()

filters = [
    invalid_filter,
    hit_filter,
    miss_filter,
    tilelink_filter,
    client_miss_filter,
    self_client_filter,
    null_block_filter
]

for f in filters:
    all_states = list(filter(f, all_states))
    print(f"filter: {retrieve_name(f)} states: {len(all_states)}")

for i, s in enumerate(all_states):
    s.id = i

example_state = DirState(
    SelfDir(TLState.TRUNK, DirtyState.CLEAN, HitState.MISS,
            [TLState.TIP, TLState.INVALID], Block.G),
    [
		ClientDir(TLState.INVALID, HitState.MISS, Block.NULL),
        ClientDir(TLState.TIP, HitState.HIT, Block.F)
    ]
)

id = lookup_state_id(example_state, all_states)
print(f"node {id}:\n {all_states[id]}")
visualize([all_states[id]])
