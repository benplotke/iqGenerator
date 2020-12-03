# problem types
# 	sequence
# 		transformations are applied between every item in sequence
# 	combination
# 		middle is combination of the sides

# possible new sequence transformations
# 	char
# 		next alpha
# 		next from sample chars

# ToDo
#  -- Add more problem types

import string
import random
import copy

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
					return self.RandomSwapCase(c)

	@staticmethod
	def RandomSwapCase(c):
		if random.random() > .5:
			return c.swapcase()
		return c

	# returns mixed case, but without repeating a letter
	@staticmethod
	def GetNChars(n):
		selectedChars = random.sample(string.ascii_lowercase, n)
		return [c for c in selectedChars]

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

	def __iter__(self):
		self._i_ = 0
		return self

	def __next__(self):
		if self._i_ >= self.size**2:
			raise StopIteration()
		result = self.rows[ self._i_//self.size ][ self._i_%self.size ]
		self._i_ += 1
		return result

	def Reflect(self, axis):
		reflected = []
		for j in range(self.size-1, -1, -1):
			reflected.append(self.rows[j])
		self.rows = reflected
		if axis == 'x':
			pass # This if is here just so there is a complete enumeration of possibilities
		elif axis == 'y':
			self.Rotate(180)
		elif axis == 'y=x':
			self.Rotate(270)
		elif axis == 'y=-x':
			self.Rotate(90)
		else:
			raise Exception()

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

	def RowShift(self, row, n):
		n = -n
		self.rows[row] = self.rows[row][n:] + self.rows[row][:n]

	def ColShift(self, col, n):
		tmpCol = []
		for i in range(self.size):
			tmpCol.append(self.rows[i][col])
		for i in range(self.size):
			self.rows[i][col] = tmpCol[(i+n)%self.size]

	def __str__(self):
		horizontalRule = "-" * ( 4*self.size +1 )
		s = horizontalRule + '\n'
		for row in self.rows:
			for c in row:
				s += "| " + c + " "
			s += "|\n" + horizontalRule + '\n'
		return s.rstrip('\n')
		

class Transformation:

	@staticmethod
	def _conditionalDifficulty(oldTrans, newTrans):
		types = set([type(oldTrans), type(newTrans)])
		if SwapCase in types:
			if len(types) == 1 and oldTrans.row == newTrans.row and oldTrans.col == newTrans.col:
				return False
			return newTrans.difficulty
		if Rotate in types:
			if len(types) == 1 or Reflect in types:
				return False
			if HorizontalShift in types or VerticalShift in types:
				return newTrans.difficulty +1
			if RowShift in types or ColShift in types:
				return newTrans.difficulty +2
			raise Exception()
		if Reflect in types:
			if len(types) == 1:
				return False
			if HorizontalShift in types or VerticalShift in types:
				return newTrans.difficulty +1
			if RowShift in types or ColShift in types:
				return newTrans.difficulty +2
			raise Exception()
		if HorizontalShift in types:
			if len(types) == 1:
				return False
			if VerticalShift in types:
				return newTrans.difficulty
			if RowShift in types or ColShift in types:
				return newTrans.difficulty +1
			raise Exception()
		if VerticalShift in types:
			if len(types) == 1:
				return False
			if RowShift in types or ColShift in types:
				return newTrans.difficulty +1
			raise Exception()
		if RowShift in types:
			if len(types) == 1:
				if oldTrans.row == newTrans.row:
					return False
				if abs(oldTrans.row - newTrans.row) == 1 and oldTrans.shift == newTrans.shift:
					return 0.1
				return newTrans.difficulty
			if ColShift in types:
				return newTrans.difficulty +1
			raise Exception()
		if ColShift in types:
			if len(types) == 1:
				if oldTrans.col == newTrans.col:
					return False
				if abs(oldTrans.col - newTrans.col) == 1 and oldTrans.shift == newTrans.shift:
					return 0.1
				return newTrans.difficulty
		raise Exception(str(oldTrans) + ":" + str(newTrans))

	@classmethod
	def ConditionalDifficulty(cls, oldTransfs, newTrans):
		maxDifficulty = newTrans.difficulty
		minDifficulty = newTrans.difficulty
		for trans in oldTransfs:
			conditionalDifficulty = cls._conditionalDifficulty(trans, newTrans)
			if not conditionalDifficulty:
				return False
			if maxDifficulty < conditionalDifficulty:
				maxDifficulty = conditionalDifficulty
			elif minDifficulty > conditionalDifficulty:
				minDifficulty = conditionalDifficulty
		if minDifficulty < newTrans.difficulty:
			return minDifficulty
		return maxDifficulty

	@staticmethod
	def GetRandomTransformation(size):
		transformations = [Rotate, Reflect, HorizontalShift, VerticalShift, RowShift, ColShift, SwapCase]
		ct = len(transformations)
		# the 7 in ct*7 is because the 3 rotations and 4 reflacts are related.
		# To get them with equal frequency, I need to move a 1/7th from rotations to reflections
		weights = [6/(ct*7), 8/(ct*7), 1/ct, 1/ct, 1/ct, 1/ct, 6/ct]
		return random.choices(transformations, weights)[0].GetRandom(size)

	@classmethod
	def GetRandomTransformations(cls, size, targetDifficulty):
		difficulty = 0
		transformations = []
		while difficulty <= targetDifficulty - .5:
			trans = cls.GetRandomTransformation(size)
			trans.difficulty = cls.ConditionalDifficulty(transformations, trans)
			if trans.difficulty and trans.difficulty + difficulty <= targetDifficulty + .5:
				difficulty += trans.difficulty
				transformations.append(trans)
		return transformations

class Rotate(Transformation):

	def __init__(self, degrees):
		self.degrees = degrees
		self.difficulty = 2
		if degrees == 180:
			self.difficulty = 1

	def __str__(self):
		return f"Rotate by {self.degrees} degrees"

	@classmethod
	def GetRandom(cls, size):
		return cls(random.choice((90, 180, 270)))

	def Transform(self, matrix):
		matrix.Rotate(self.degrees)

class Reflect(Transformation):

	def __init__(self, axis):
		self.axis = axis
		self.difficulty = 1

	def __str__(self):
		return f"Matrix was reflected about the {self.axis} axis"

	@classmethod
	def GetRandom(cls, size):
		return cls(random.choice(('x', 'y', 'y=x', 'y=-x')))

	def Transform(self, matrix):
		matrix.Reflect(self.axis)

class HorizontalShift(Transformation):

	def __init__(self, shift):
		self.shift = shift
		self.difficulty = 1

	def __str__(self):
		return f"Matrix was shifted {self.shift} positions horizontally"

	@classmethod
	def GetRandom(cls, size):
		return cls(random.choice(range(1, size)))

	def Transform(self, matrix):
		for i in range(matrix.size):
			matrix.RowShift(i, self.shift)

class VerticalShift(Transformation):

	def __init__(self, shift):
		self.shift = shift
		self.difficulty = 1

	def __str__(self):
		return f"Matrix was shifted {self.shift} positions vertically"

	@classmethod
	def GetRandom(cls, size):
		return cls(random.choice(range(1, size)))

	def Transform(self, matrix):
		for j in range(matrix.size):
			matrix.ColShift(j, self.shift)

class RowShift(Transformation):

	def __init__(self, row, shift):
		self.row = row
		self.shift = shift
		self.difficulty = 2
		if abs(shift) == 1:
			self.difficulty = 1

	def __str__(self):
		return f"Row {self.row} was shifted {self.shift} positions horizontally (zero indexing)"

	@classmethod
	def GetRandom(cls, size):
		return cls(random.choice(range(size)), random.choice(range(1, size)))

	def Transform(self, matrix):
		matrix.RowShift(self.row, self.shift)

class ColShift(Transformation):

	def __init__(self, col, shift):
		self.col = col
		self.shift = shift
		self.difficulty = 2
		if abs(shift) == 1:
			self.difficulty = 1

	def __str__(self):
		return f"Column {self.col} was shifted {self.shift} positions vertically (zero indexing)"

	@classmethod
	def GetRandom(cls, size):
		return cls(random.choice(range(size)), random.choice(range(1, size)))

	def Transform(self, matrix):
		matrix.ColShift(self.col, self.shift)

class SwapCase(Transformation):

	def __init__(self, row, col):
		self.row = row
		self.col = col
		self.difficulty = 0.25

	def __str__(self):
		return f"The case of row {self.row} column {self.col} has been swapped (zero indexing)"

	@classmethod
	def GetRandom(cls, size):
		return cls(random.choice(range(size)), random.choice(range(size)))

	def Transform(self, matrix):
		matrix.rows[self.row][self.col] = matrix.rows[self.row][self.col].swapcase()


class Sequence:

	def __init__(self, size, difficulty):
		self.size = size
		self.matricies = []
		self.matricies.append(CharMatrix(size))
		self.transformations = Transformation.GetRandomTransformations(self.size, difficulty)
		for m in range(3):
			nextMatrix = copy.deepcopy(self.matricies[-1])
			for trans in self.transformations:
				trans.Transform(nextMatrix)
			self.matricies.append(nextMatrix)

	def _matrixRowToStr(self, start, stop):
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
		s += self._matrixRowToStr(0, 3) + "\n"
		print(s)

	def DisplaySolution(self):
		print("\n" + str(self.matricies[-1]) + "\n")
		for transformation in self.transformations:
			print(transformation)
		print()


def GetMatrixSize():
	while True:
		matrixSize = input("Enter matrix size (4): ")
		if not matrixSize:
			matrixSize = 4
		try:
			matrixSize = int(matrixSize)
			if matrixSize < 3:
				print("Matrix size must be >=3")
			else:
				return matrixSize
		except Exception:
			print("Matrix size must be an integer >=3.")

def GetDifficulty():
	while True:
		difficultyStr = input("Easy (e), Medium (m) (default), or Hard (h)?: ")
		if not difficultyStr:
			difficultyStr = 'm'
		difficultyStr = difficultyStr.lower()
		if difficultyStr == 'easy' or difficultyStr == 'e':
			return 2.5
		if difficultyStr == 'medium' or difficultyStr == 'm':
			return 4
		if difficultyStr == 'hard' or difficultyStr == 'h':
			return 6
		print("Please select a valid difficulty.")

instructions = '''Instructions:
   The leftmost matrix is the start matrix.
   The center matrix is produced by applying a series of transformations to the left matrix.
   The right matrix is produced by applying the same transformations to the center matrix.
   If we apply the same transformations to the right matrix, we will get the final matrix.
   Try to predict the final matrix.'''

def main():

	print()
	matrixSize = GetMatrixSize()
	print()
	difficulty = GetDifficulty()
	print()
	print(instructions)
	print()

	while  True:
		seq = Sequence(matrixSize, difficulty)
		seq.DisplayProblem()
		input("Press enter for solution: ")
		seq.DisplaySolution()
		input("\n Press enter for new problem: ")
		print("\n  ---------  new problem  ----------  \n")


main()