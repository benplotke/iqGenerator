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
#  -- print user friendly transformation explanations
#  -- make difficulty logic and selection


# matrix of matricies

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


class MatrixOperations(Enum):
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
		self.OperationDictionary = {
			MatrixOperations.rotate : self.Rotate,
			MatrixOperations.zigZagShift : self.ZigZagShift,
			MatrixOperations.horizontalShift : self.HorizontalShift,
			MatrixOperations.verticalShift : self.VerticalShift,
			MatrixOperations.rowShift : self.RowShift,
			MatrixOperations.colShift : self.ColShift,
			MatrixOperations.swapCase : self.SwapCase
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

	def SwapCase(self, *cell):
		self.rows[cell[0]][cell[1]] = self.rows[cell[0]][cell[1]].swapcase()

	def ApplyOperation(self, operation, args):
		self.OperationDictionary.get(operation)(*args)

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


class Problem:

	def __init__(self, size):
		self.size = size
		self.matricies = []
		self.matricies.append(CharMatrix(size))
		self.rules = []
		self.problem = ""

	# linear sequence displayed left to right top to bottom
	def NewSequence(self, ruleCount):
		self.problem = "sequence"
		rules = [
			( MatrixOperations.rotate, (random.choice((90, 180, 270)),) ),
			( MatrixOperations.zigZagShift, (random.choice(range(1, self.size**2 -1)),) ),
			( MatrixOperations.horizontalShift, (random.choice(range(1, self.size-1)),) ),
			( MatrixOperations.verticalShift, (random.choice(range(1, self.size-1)),) ),
			( MatrixOperations.rowShift, (random.choice(range(1, self.size-1)), random.choice(range(1, self.size-1))) ),
			( MatrixOperations.colShift, (random.choice(range(1, self.size-1)), random.choice(range(1, self.size-1))) ),
			( MatrixOperations.swapCase, (random.choice(range(self.size)),random.choice(range(self.size))) )
		]
		rules = random.sample(rules, ruleCount)
		for m in range(3):
			nextMatrix = copy.deepcopy(self.matricies[-1])
			for rule in rules:
				nextMatrix.ApplyOperation(rule[0], rule[1])
			self.matricies.append(nextMatrix)
		self.rules = rules

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
			print(str(rule))
		print(str(self.matricies[-1]))

while  True:
	prob = Problem(4)
	prob.NewSequence(2)
	prob.DisplayProblem()
	input("Press enter for solution: ")
	prob.DisplaySolution()
	print("\n  ---------  new problem  ----------  \n")