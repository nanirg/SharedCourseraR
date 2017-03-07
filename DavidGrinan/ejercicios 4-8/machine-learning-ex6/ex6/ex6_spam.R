#exercise 6 spam filter


#email prepro
source("processEmail.R")
source("emailFeatures.R")
fName <- 'emailSample1.txt'
file_contents <- readChar(fName,file.info(fName)$size)
word_indices  <- processEmail(file_contents)

# Print Stats
cat(sprintf('Word Indices: \n'))
cat(sprintf(' %d', word_indices))
cat(sprintf('\n\n'))



fName <- 'emailSample1.txt'
file_contents <- readChar(fName,file.info(fName)$size)
word_indices  <- processEmail(file_contents)
features <- emailFeatures(word_indices)

#Train Linear SVM

data <- readMat('spamTrain.mat')
X <- data$X
y <- data$y

C <- 0.1
model <- svmTrain(X, y, C, linearKernel)

p <- svmPredict(model, X)

cat(sprintf('Training Accuracy: %f\n', mean(p==y) * 100))


######

data <- readMat('spamTrain.mat')
Xtest <- data$Xtest
ytest <- data$ytest
p <- svmPredict(model, Xtest)

cat(sprintf('Test Accuracy: %f\n', mean(p==ytest) * 100))


#########




















