"""

Project 1
Group 10
Group members:
    Priyanka Chary
    Andrew Gjelsteen
    Ryan Yordanoff
    Lucas Zhang
    
"""

class array: 
    def __init__(self, nestedarray): 
        self.data = nestedarray
        self.size = 0
        self.shape = (0,0)
        
        if type(self.data[0]) == list:
              
            even = True
            for a in self.data: 
                if(len(a) != len(self.data[0])):
                    even = False
                    self.shape = False
                if even:
                    self.shapelist = [len(self.data),len(self.data[0])]

                    for a in self.data: 
                        for b in a:
                            self.size+=1
                            self.shape = tuple(self.shapelist)
                else: 
                    self.shape = (1, len(self.data))
   
        
    
    def getsize(self): #the number of elements per column
        return self.size
    
    def getdata(self): #the matrix itself
        return self.data
    
    def getshape(self): # return m x n specification (follow NumPy specification)
        return self.shape

    def __pow__(self, other):
        """Returns the element wise exponential for the array, to the power which the user specifies
        """
        if(self.shape):
            c = []
            if(type(self.data[0]) != list and self.shape[0] == 1):
                for a in self.data:
                    c.append(a ** other)
            else:
                for i in range(len(self.data)):
                    c.append([])
                    for j in range(len(self.data[i])):
                        c[i].append(self.data[i][j] ** other)
        return c  
        
        
    def __getitem__(self, other):
        """Returns the element in the array at position (i,j) assuming the input is a tuple"""
        if self.getshape()[0] > 1:
            return self.data[other[0]][other[1]]
        else: 
            return self.data[0][other[1]]

    def __add__(self,other): 
      '''The add method takes array(x) and can add array(x)+array(y), array(x) +y or array(x) + n where n is a scalar. It returns c, a nested list. 
      The first block of code is for array(x)+ y , if if isinstance(other, array) == True: it solves for array(x)+array(y) and the last else statement
      is to add a scalar.'''
      inner = []
      c = []
      if type(other) != int and type(other)!= float:
          if isinstance(other, array) == False:
              if type(self.data) == list:
                  if type(self.data[0]) == list:
                      if len(self.data) ==1 and len(other)!= 1:
                          for l in range(len(other)):
                              for k in range(len(other[l])):
                                  inner.append(other[l][k]+self.data[0][k])
                              c.append(inner)
                              inner = []
                      if len(other) ==1 and len(self.data) !=1:
                          for j in range(len(self.data)):
                              for i in range(len(self.data[1])):
                                  inner.append(self.data[j][i]+other[0][i])
                              c.append(inner)
                              inner =[]
                      if len(self.data) ==1 and len(other) ==1:
                          for g in range(len(self.data[0])):
                              inner.append(self.data[0][g]+other[0][g])
                          c.append(inner)
                      if len(self.data) != 1 and len(other) != 1:    
                          for n in range(len(self.data)):
                              for m in range(len(self.data[n])):
                                  inner.append(self.data[n][m]+other[n][m])
                              c.append(inner)
                              inner = []
                  else: 
                      for e in range(len(self.data)):
                          c.append(self.data[e]+other[e])
              else:
                  c = self.data+other
          if isinstance(other, array) == True:
              other_copy = other.getdata()[:]
              inner =[]
              c = []
              if len(self.data)!=1 and len(other_copy)!=1: 
                  for p in range(len(self.data)):
                      for m in range(len(self.data[p])):
                          inner.append(self.data[p][m]+other_copy[p][m])
                      c.append(inner)
                      inner = []
              if len(self.data) ==1 and len(other_copy)!= 1:
                  for l in range(len(other_copy)):
                      for k in range(len(other_copy[l])):
                          inner.append(other_copy[l][k]+self.data[0][k])
                      c.append(inner)
                      inner = []
              if len(other_copy) ==1 and len(self.data) !=1:
                  for j in range(len(self.data)):
                      for i in range(len(self.data[1])):
                          inner.append(self.data[j][i]+other_copy[0][i])
                      c.append(inner)
                      inner =[]
              if len(self.data) ==1 and len(other_copy) ==1:
                  for g in range(len(self.data[0])):
                      inner.append(self.data[0][g]+other_copy[0][g])
                  c.append(inner)


      else:
          for i in range(len(self.data)):
              c.append([])
              for j in range(len(self.data[0])):
                  c[i].append(self.data[i][j] + other)
      return c 


    def __mul__(self,other): 
      '''The mul method takes array(x) and can multiply array(x)*array(y), array(x)* y or array(x) * n where n is a scalar. It returns c, a nested list. 
      The first block of code is for array(x)* y , if if isinstance(other, array) == True: it solves for array(x)*array(y) and the last else statement
      is to multiply a scalar.'''
      inner = []
      c = []
      if type(other) != int and type(other)!= float:
          if isinstance(other, array) == False:
              if type(self.data) == list:
                  if type(self.data[0]) == list:
                      if len(self.data) ==1 and len(other)!= 1:
                          for l in range(len(other)):
                              for k in range(len(other[l])):
                                  inner.append(other[l][k]*self.data[0][k])
                              c.append(inner)
                              inner = []
                      if len(other) ==1 and len(self.data) !=1:
                          for j in range(len(self.data)):
                              for i in range(len(self.data[1])):
                                  inner.append(self.data[j][i]*other[0][i])
                              c.append(inner)
                              inner =[]
                      if len(self.data) ==1 and len(other) ==1:
                          for g in range(len(self.data[0])):
                              inner.append(self.data[0][g]*other[0][g])
                          c.append(inner)
                      if len(self.data) != 1 and len(other) != 1:    
                          for n in range(len(self.data)):
                              for m in range(len(self.data[n])):
                                  inner.append(self.data[n][m]*other[n][m])
                              c.append(inner)
                              inner = []
                  else: 
                      for e in range(len(self.data)):
                          c.append(self.data[e]*other[e])
              else:
                  c = self.data*other
          if isinstance(other, array) == True:
              other_copy = other.getdata()[:]
              inner =[]
              c = []
              if len(self.data)!=1 and len(other_copy)!=1: 
                  for p in range(len(self.data)):
                      for m in range(len(self.data[p])):
                          inner.append(self.data[p][m]*other_copy[p][m])
                      c.append(inner)
                      inner = []
              if len(self.data) ==1 and len(other_copy)!= 1:
                  for l in range(len(other_copy)):
                      for k in range(len(other_copy[l])):
                          inner.append(other_copy[l][k]*self.data[0][k])
                      c.append(inner)
                      inner = []
              if len(other_copy) ==1 and len(self.data) !=1:
                  for j in range(len(self.data)):
                      for i in range(len(self.data[1])):
                          inner.append(self.data[j][i]*other_copy[0][i])
                      c.append(inner)
                      inner =[]
              if len(self.data) ==1 and len(other_copy) ==1:
                  for g in range(len(self.data[0])):
                      inner.append(self.data[0][g]*other_copy[0][g])
                  c.append(inner)


      else:
          for i in range(len(self.data)):
              c.append([])
              for j in range(len(self.data[0])):
                  c[i].append(self.data[i][j] * other)
      return c 
    
    def __neg__(self):
        ''' The neg method allows for negative matrices and scalars (in brackets [[]]) to be used in linear 
        equations. e.g. if -A+B is used, matrix A will have a (-1) distributed to each value prior 
        to continuing with the +B operation.'''
        A = self.data
        C = [] #initialize C as an empty list
        k = 0 #dummy variable

        if type(A) == list: #if A is a list
            #first: populate C with lists of zeroes to fill shape same as input
            if type(A[0]) == list: #if A is a nested list
                for i in range(0,len(A)):
                    C.append([0]*len(A[0]))
                    #C is now a zero matrix with shape of A
                for i in range(0, len(A)): # for each row in A
                    for j in range(0,len(A[0])): # for each column in A
                        C[i][j] = (-1)*A[i][j] #turn each element into negative of itself
            else:
                for i in range(0,len(A)):
                    k = A[i]*(-1)
                    C.append(k)
        return C

    def __sub__(self,other): 
      '''The sub method takes array(x) and can sutract array(x)-array(y), array(x)- y or array(x) - n where n is a scalar. It returns c, a nested list. 
      The first block of code is for array(x)- y , if if isinstance(other, array) == True: it solves for array(x)-array(y) and the last else statement
      is to subtract a scalar.'''
      inner = []
      c = []
      if type(other) != int and type(other)!= float:
          if isinstance(other, array) == False:
              if type(self.data) == list:
                  if type(self.data[0]) == list:
                      if len(self.data) ==1 and len(other)!= 1:
                          for l in range(len(other)):
                              for k in range(len(other[l])):
                                  inner.append(other[l][k]-self.data[0][k])
                              c.append(inner)
                              inner = []
                      if len(other) ==1 and len(self.data) !=1:
                          for j in range(len(self.data)):
                              for i in range(len(self.data[1])):
                                  inner.append(self.data[j][i]-other[0][i])
                              c.append(inner)
                              inner =[]
                      if len(self.data) ==1 and len(other) ==1:
                          for g in range(len(self.data[0])):
                              inner.append(self.data[0][g]-other[0][g])
                          c.append(inner)
                      if len(self.data) != 1 and len(other) != 1:    
                          for n in range(len(self.data)):
                              for m in range(len(self.data[n])):
                                  inner.append(self.data[n][m]-other[n][m])
                              c.append(inner)
                              inner = []
                  else: 
                      for e in range(len(self.data)):
                          c.append(self.data[e]-other[e])
              else:
                  c = self.data+other
          if isinstance(other, array) == True:
              other_copy = other.getdata()[:]
              inner =[]
              c = []
              if len(self.data)!=1 and len(other_copy)!=1: 
                  for p in range(len(self.data)):
                      for m in range(len(self.data[p])):
                          inner.append(self.data[p][m]-other_copy[p][m])
                      c.append(inner)
                      inner = []
              if len(self.data) ==1 and len(other_copy)!= 1:
                  for l in range(len(other_copy)):
                      for k in range(len(other_copy[l])):
                          inner.append(other_copy[l][k]-self.data[0][k])
                      c.append(inner)
                      inner = []
              if len(other_copy) ==1 and len(self.data) !=1:
                  for j in range(len(self.data)):
                      for i in range(len(self.data[1])):
                          inner.append(self.data[j][i]-other_copy[0][i])
                      c.append(inner)
                      inner =[]
              if len(self.data) ==1 and len(other_copy) ==1:
                  for g in range(len(self.data[0])):
                      inner.append(self.data[0][g]-other_copy[0][g])
                  c.append(inner)


      else:
          for i in range(len(self.data)):
              c.append([])
              for j in range(len(self.data[0])):
                  c[i].append(self.data[i][j] - other)
      return c 
      
    def transpose(self):
        """original array has rows indexed by i and columns indexed by
        j. then A.transpose()[i, j] = A[j, i]
        Output: nested list"""
        A = self.data
        B = list(map(list, zip(*A))) #transpose the matrix
        return B



    def sums(self, sumtype='default'):
        """default argument returns sum of all elements in the array
        0 argument returns an n dimensional column vector
        1 argument returns an n dimensional row vector
        Output: With sumtype=default = int or float
        with sumtype= 0 or 1 : outputs list of lists"""

        self_copy = self.getdata()[:]
        raw_numbers = []
        initial = True
        result = []
        result_0 = []
        current_sum = 0
        
        if sumtype == 'default':
            for i in self_copy:
                for j in i:
                    raw_numbers.append(j)

            return sum(raw_numbers)

        elif sumtype == 0:
            for i in range(len(self_copy[0])):
                if initial == True: #so this doesn't append an empty list on index 1
                    initial = False
                else:
                    result.append(current_sum)
                    current_sum = 0
                for j in self_copy:
                    current_sum += j[i]
                    
            result.append(current_sum) #for the very last row b/c for loop ends and doesn't append

            result_0.append(result)
                

            return result_0

        elif sumtype == 1:
            for i in self_copy:
                if initial == True: #so this doesn't append an empty list on index 1
                    initial = False
                else:
                    result.append([current_sum])
                    current_sum = 0
                for j in i:
                    current_sum += j

            result.append([current_sum]) #for the very last row b/c for loop ends and doesn't append

            return result
    
    def __truediv__(self, other):
        """Overloads the true div operator to allow division of:
        array/array, array/int or float, and array/nested list of same dimension.
        Outputs: the result in a nested list"""

        self_copy = self.getdata()[:]
        
        raw_result = []        
        current_row = []
        counter = 0
        div_result = []
        
        #non-scalars
        if type(other) != int and type(other)!= float and type(other)!= list: 
            other_copy = other.getdata()[:]

            #for column = 1
            if len(other_copy[0]) == 1:
                #get a raw_result which is a list of all self/other
                for i in range(len(self_copy)):    
                    for j in range(len(self_copy[i])):
                        raw_result.append(self_copy[i][j]/other_copy[i][j])

                #process raw_result into list of lists aka array
                for i in raw_result:
                    div_result.append([i])
            else:
                #get a raw_result which is a list of all self/other
                for i in range(len(self_copy)):    
                    for j in range(len(self_copy[i])):
                        raw_result.append(self_copy[i][j]/other_copy[i][j])

                #process raw_result into list of lists aka array
                for i in range(len(raw_result)):
                    #very last element of raw_result
                    if i == len(raw_result) -1:
                        current_row.append(raw_result[i])
                        div_result.append(current_row)
                    #end of column chunk then add that chunk to div_result
                    elif counter == len(self_copy[0]): #[important] this assumes column number of self = column number of other
                        div_result.append(current_row)
                        current_row = []
                        counter = 0
                        current_row.append(raw_result[i])
                        counter += 1
                    else:
                        current_row.append(raw_result[i])
                        counter += 1
        #for scalars
        elif type(other) == int or type(other) == float:
            for i in range(len(self.data)):
                div_result.append([])
                for j in range(len(self.data[0])):
                    div_result[i].append(self.data[i][j] / other)              
        
        #for list of lists
        else:
            other_copy = other[:]
            #for column = 1
            if len(other_copy[0]) == 1:
                #get a raw_result which is a list of all self/other
                for i in range(len(self_copy)):    
                    for j in range(len(self_copy[i])):
                        raw_result.append(self_copy[i][j]/other_copy[i][j])

                #process raw_result into list of lists aka array
                for i in raw_result:
                    div_result.append([i])

            else:
                #get a raw_result which is a list of all self/other
                for i in range(len(self_copy)):    
                    for j in range(len(self_copy[i])):
                        raw_result.append(self_copy[i][j]/other_copy[i][j])

                #process raw_result into list of lists aka array
                for i in range(len(raw_result)):
                    #very last element of raw_result
                    if i == len(raw_result) -1:
                        current_row.append(raw_result[i])
                        div_result.append(current_row)
                    #end of column chunk then add that chunk to div_result
                    elif counter == len(self_copy[0]): #[important] this assumes column number of self = column number of other
                        div_result.append(current_row)
                        current_row = []
                        counter = 0
                        current_row.append(raw_result[i])
                        counter += 1
                    else:
                        current_row.append(raw_result[i])
                        counter += 1
        return div_result

    def mean(self, sumtype='default'):
        """default argument returns mean of all elements in the array
        0 argument returns an n dimensional column vector
        1 argument returns an n dimensional row vector
        Output: With sumtype=default = int or float
        with sumtype= 0 or 1 : outputs list of lists"""

        self_copy = self.getdata()[:]
        raw_numbers = []
        result = []
        result_0 = []

        if sumtype == 'default':
            for i in self_copy:
                for j in i:
                    raw_numbers.append(j)

            return self.sums()/len(raw_numbers)

        elif sumtype == 0:
            for i in self.sums(0):
                for j in i:
                    result.append(j/len(self_copy))

            result_0.append(result)

            return result_0

        elif sumtype == 1:
            for i in self.sums(1):
                for j in i:
                    result.append([j/len(self_copy[0])])

            return result
        
    def dot(self, matrixB): #takes input matrices A and B
        A = self.data
       
        C = list()
        if type(matrixB) == list:
            B = matrixB
            if self.shape[1] == len(matrixB):
                emptyRow = [0]*len(B[0])

                for i in range(len(A)): #initialize zero-matrix to store results
                    C.append([0]*len(B[0]))

                for j in range(0,len(B[0])):
                    for i in range(0,len(A)):
                        temp = 0
                        for k in range(0,len(A[0])):
                            temp += B[k][j] * A[i][k]
                        C[i][j] = temp
                return C
        else:
            B = matrixB.getdata()
            if self.shape[1] == matrixB.shape[0]:
                emptyRow = [0]*len(B[0])
                for i in range(len(A)): #initialize zero-matrix to store results
                    C.append([0]*len(B[0]))

                for j in range(0,len(B[0])):
                    for i in range(0,len(A)):
                        temp = 0
                        for k in range(0,len(A[0])):
                            temp += B[k][j] * A[i][k]
                        C[i][j] = temp
                return C

    def var(self):
        '''This code finds the covariance matrix. To match the output of numpy, we start with the transposed array (Y).
        Yc = Y - Ybar, transpose Yc to get Yct, divide Yct by n-1 to get Yct_n and then find the dot product of Yct_n and 
        Yc to get the covariance matrix'''
        Y = array(array(self.getdata()[:]).transpose())
        Ybar_raw = Y.mean(0)
        Ybar = []
        Yc = []
        inner_y = []

        #to fix output error in Y.mean(0)
        for i in Ybar_raw:
            for j in i:
                Ybar.append([j])

                
        for n in range(len(Y.getdata()[:])):
            for o in range(len(Y.getdata()[:][n])):
                inner_y.append(Y.getdata()[:][n][o]- Ybar[o][0])
            Yc.append(inner_y)
            inner_y = []
        Yct = array(Yc).transpose()
        n_y = Y.getshape()[0]
        Yct_n = array(Yct)/(n_y-1)
        C_y = array(Yct_n).dot(Yc)
        return C_y

a = array([[1,2,3],[1,2,3]])


print(a[(1)])