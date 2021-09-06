# define some consts
Invalid = 0
Trunk = 1
Branch = 2
Tip = 3

Dirty = 0
Clean = 1

stateDumpMap = dict([(Invalid, 'Invalid'), (Trunk, 'Trunk'), (Branch, 'Branch'), (Tip, 'Tip')])

class SelfDir:
    
    def __init__(self):
        pass

    def deserialize(self, num):
        self.clientStates = num // 64
        self.dirty = (num % 64) // 32
        self.block = (num % 32) // 8
        self.state = (num % 8) // 2
        self.hit = (num % 2)
        return self

    def serialize(self):
        return self.hit + self.state * 2 + self.block * 8 + self.dirty * 32 + self.clientStates * 64

    def dump(self):
        print(stateDumpMap[self.state])

    nrPossible = 256


class ClientDir:
    def __init__(self):
        pass

    def deserialize(self, num):
        self.block = num // 8
        self.state = (num % 8) // 2
        self.hit = (num % 2)
        return self

    def serialize(self):
        return self.hit + self.state * 2 + self.block * 8

    def dump(self):
        print(stateDumpMap[self.state])

    nrPossible = 32


class DirStatus:
    def __init__(self):
        pass

    def deserialize(self, num):
        selfDir = SelfDir()
        clientDir = ClientDir()
        self.selfDir = selfDir.deserialize(num // ClientDir.nrPossible)
        self.clientDirs = clientDir.deserialize(num % ClientDir.nrPossible)
        return self

    def isValid(self):
        return True

    def dump(self):
        print('=============')
        self.selfDir.dump()
        self.clientDir.dump()

    nrPossible = ClientDir.nrPossible * SelfDir.nrPossible


if __name__ == '__main__':
    dirStatusList = []
    for i in range(DirStatus.nrPossible):
        dirStatus = DirStatus()
        if dirStatus.deserialize(i).isValid():
            dirStatusList.append(dirStatus.deserialize(i))

    for item in dirStatusList:
        item.dump()

