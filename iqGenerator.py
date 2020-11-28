#iqGenerator.py

# matrix types
# 	zig-zag sequence
# 		rules are applied between every item in sequence

# 	operator - operator rule is applied to right/top to produce left/bottom
# 		must allow for multiple applications
# 		must be symetric

# 	combination
# 		middle is combination of the sides

# possible rules
# 	matrix
# 		rotation
# 		mirror
# 		mario shift
# 		zig-zag shift
# 	char
# 		swapcase
# 		next alpha
# 		next from sample

# ToDo
#  -- improve rule selection logic
#  -- Complete domain of existing rules
#  -- Add remaining rule ideas
#  -- Add more matrix types

import string
import random
import copy
from enum import Enum

class RandomChars:

	def __init__(self, countUniqueChars, repeatLimit):
		self.totalCharCount = 0
		self.countUniqueChars = countUniqueChars
		self.repeatLimit = repeatLimit
		self.selectedChars = RandomChars.GetNChars(countUniqueChars)

	def __iter__(self):
		self.totalCharCount = 0
		self.charCounts = {c:0 for c in self.selectedChars}
		return self

	def __next__(self):
		if self.totalCharCount >= self.countUniqueChars * self.repeatLimit:
			raise StopIteration()
		else:
			while True:
				c = random.choice(self.selectedChars)
				if self.charCounts[c] < self.repeatLimit:
					self.totalCharCount += 1
					self.charCounts[c] += 1
					return c

	@staticmethod
	def RandomCaseSwap(c):
		if random.random() > .5:
			return c.swapcase()
		return c

	# returns mixed case, but without repeating a letter
	@staticmethod
	def GetNChars(n):
		selectedChars = random.sample(string.ascii_lowercase, n)
		return [RandomChars.RandomCaseSwap(c) for c in selectedChars]


class MatrixRules(Enum):
	rotate = 1
	zigZagShift = 2
	horizontalShift = 3
	verticalShift = 4
	rowShift = 5
	colShift = 6
	swapCase = 7


class Coordinate:

	def __init__(self, i, j):
		self.i = i
		self.j = j

	@classmethod
	def getRandomCoordinate(cls, size):
		return 

class CharMatrix:

	def __init__(self, size):
		chars = iter(RandomChars(size+1, size))
		self.size = size
		self.rows = []
		for i in range(size):
			row = []
			for j in range(size):
				row.append(next(chars))
			self.rows.append(row)
		self.RuleDictionary = {
			MatrixRules.rotate : self.Rotate,
			MatrixRules.zigZagShift : self.ZigZagShift,
			MatrixRules.horizontalShift : self.HorizontalShift,
			MatrixRules.verticalShift : self.VerticalShift,
			MatrixRules.rowShift : self.RowShift,
			MatrixRules.colShift : self.ColShift,
			MatrixRules.swapCase : self.SwapCase
		}

	def __iter__(self):
		self._i_ = 0
		return self

	def __next__(self):
		if self._i_ >= self.size**2:
			raise StopIteration()
		result = self.rows[ self._i_//self.size ][ self._i_%self.size ]
		self._i_ += 1
		return result

	def Rotate(self, degrees):
		rotated = []
		if degrees == 90 or degrees == -270:
			for j in range(self.size):
				newRow = []
				for i in range(self.size-1, -1, -1):
					newRow.append(self.rows[i][j])
				rotated.append(newRow)
		elif degrees == 180 or degrees == -180:
			for i in range(self.size-1, -1, -1):
				newRow = []
				for j in range(self.size-1, -1, -1):
					newRow.append(self.rows[i][j])
				rotated.append(newRow)
		elif degrees == -90 or degrees == 270:
			for j in range(self.size-1, -1, -1):
				newRow = []
				for i in range(self.size):
					newRow.append(self.rows[i][j])
				rotated.append(newRow)
		else:
			raise Exception()
		self.rows = rotated

	def ZigZagShift(self, n):
		shifted = [['_']*self.size for i in range(self.size)]
		for x in range(self.size**2):
			iOld = x // self.size
			iNew = ( (x+n) // self.size ) % self.size
			jOld = x % self.size
			jNew = (x+n) % self.size
			shifted[iNew][jNew] = self.rows[iOld][jOld]
		self.rows = shifted

	def HorizontalShift(self, n):
		for i in range(self.size):
			self.RowShift(i, n)

	def VerticalShift(self, n):
		for j in range(self.size):
			self.ColShift(j, n)

	def RowShift(self, row, n):
		n = -n
		self.rows[row] = self.rows[row][n:] + self.rows[row][:n]

	def ColShift(self, col, n):
		tmpCol = []
		for i in range(self.size):
			tmpCol.append(self.rows[i][col])
		for i in range(self.size):
			self.rows[i][col] = tmpCol[(i+n)%self.size]

	def SwapCase(self, row, col):
		self.rows[row][col] = self.rows[row][col].swapcase()

	def ApplyRule(self, rule):
		self.RuleDictionary[rule.name](*(rule.args))

	def RandomCells(self, count):
		cells = set()


	def __str__(self):
		horizontalRule = "-" * ( 4*self.size +1 )
		s = horizontalRule + '\n'
		for row in self.rows:
			for c in row:
				s += "| " + c + " "
			s += "|\n" + horizontalRule + '\n'
		return s.rstrip('\n')


class Rule:

	def __init__(self, name, args, difficulty):
		self.name = name
		self.args = args
		self.difficulty = difficulty

	def __str__(self):
		NameToDescription = {
			MatrixRules.rotate : "Rotate by {} degrees",
			MatrixRules.zigZagShift : "Rows were shifted {} positions in a zig-zag pattern",
			MatrixRules.horizontalShift : "Matrix was shifted {} positions horizontally",
			MatrixRules.verticalShift : "Matrix was shifted {} positions vertically",
			MatrixRules.rowShift : "Row {} was shifted {} positions horizontally (zero indexing)",
			MatrixRules.colShift : "Column {} was shifted {} positions vertically (zero indexing)",
			MatrixRules.swapCase : "The case of row {} column {} has been swapped (zero indexing)"
		}
		return NameToDescription[self.name].format(*self.args)

	@classmethod
	def _newRotate(cls, size):
		return cls(MatrixRules.rotate, (random.choice((90, 180, 270)),), 1)

	@classmethod
	def _newZigZagShift(cls, size):
		shift = random.choice(range(1, size**2 -1))
		difficulty = 3
		if shift%size == 0:
			difficulty = 1
		if shift%size == 1 or shift%size == -1:
			difficulty = 2
		return cls(MatrixRules.zigZagShift, (random.choice(range(1, size**2 -1)),), difficulty)

	@classmethod
	def _newHorizontalShift(cls, size):
		return cls(MatrixRules.horizontalShift, (random.choice(range(1, size-1)),), 1)

	@classmethod
	def _newVerticalShift(cls, size):
		return cls(MatrixRules.verticalShift, (random.choice(range(1, size-1)),), 1)

	@classmethod
	def _newRowShift(cls, size):
		shift = random.choice(range(1, size-1))
		difficulty = 2
		if shift == 1 or shift == size-1:
			difficulty = 1
		return Rule(MatrixRules.rowShift, (random.choice(range(size-1)), shift), difficulty)

	@classmethod
	def _newColShift(cls, size):
		shift = random.choice(range(1, size-1))
		difficulty = 2
		if shift == 1 or shift == size-1:
			difficulty = 1
		return cls(MatrixRules.colShift, (random.choice(range(size-1)), shift), difficulty)

	@classmethod
	def _newSwapCase(cls, size):
		return Rule(MatrixRules.swapCase, (random.choice(range(size)),random.choice(range(size))), 0.5)

	@classmethod
	def GetRandomRule(cls, size):
		NameToFactoryMethod = {
			MatrixRules.rotate : cls._newRotate,
			MatrixRules.zigZagShift : cls._newZigZagShift,
			MatrixRules.horizontalShift : cls._newHorizontalShift,
			MatrixRules.verticalShift : cls._newVerticalShift,
			MatrixRules.rowShift : cls._newRowShift,
			MatrixRules.colShift : cls._newColShift,
			MatrixRules.swapCase : cls._newSwapCase
		}
		name = MatrixRules(random.randrange(1, len(MatrixRules)+1))
		return NameToFactoryMethod[name](size)

	@classmethod
	def GetRandomRules(cls, size, targetDifficulty):
		difficulty = 0
		rules = []
		while difficulty <= targetDifficulty - .5:
			rule = cls.GetRandomRule(size)
			if rule.difficulty + difficulty <= targetDifficulty + .5:
				difficulty += rule.difficulty
				rules.append(rule)
		return rules

class Problem:

	def __init__(self, size):
		self.size = size
		self.matricies = []
		self.matricies.append(CharMatrix(size))
		self.rules = []
		self.problem = ""

	# linear sequence displayed left to right top to bottom
	def NewSequence(self, difficulty):
		self.problem = "sequence"
		self.rules = Rule.GetRandomRules(self.size, difficulty)
		for m in range(3):
			nextMatrix = copy.deepcopy(self.matricies[-1])
			for rule in self.rules:
				nextMatrix.ApplyRule(rule)
			self.matricies.append(nextMatrix)

	# top right and bottom left result from applying interveaning operator on the top left
	# bottom right comes from 
	def NewOperator(self):
		raise NotImplementedError()

	def NewCombination(self):
		raise NotImplementedError()

	def _addMatriciesRows(self, start, stop):
		horizontalRule = ("-" * ( 4*self.size +1 ) + "  ") * (stop - start)
		s = horizontalRule + "\n"
		for i in range(self.size):
			for matrix in self.matricies[start:stop]:
				for c in matrix.rows[i]:
					s += "| " + c + " "
				s += "|  "
			s += "\n" + horizontalRule + "\n"
		return s

	def DisplayProblem(self):
		s = ""
		s += self._addMatriciesRows(0, 3) + "\n"
		print(s)

	def DisplayOptions(self):
		raise NotImplementedError()

	def DisplaySolution(self):
		print(self.problem)
		for rule in self.rules:
			print(rule)
		print(str(self.matricies[-1]))

while  True:
	prob = Problem(4)
	prob.NewSequence(2)
	prob.DisplayProblem()
	input("Press enter for solution: ")
	prob.DisplaySolution()
	print("\n  ---------  new problem  ----------  \n")