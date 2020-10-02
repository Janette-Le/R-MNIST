install.packages("keras")
library(keras)
install_keras()

### Truth table
truth_table <- read.csv("Truth table.csv", header = TRUE)

### Create dataset
data_matrix <- data.frame(matrix(nrow=10000, ncol = 11))

create_dataset <- function(input, output){
  for(i in c(1:nrow(output))){
    output[i,] <- input[sample(1:16,1),]
  }
  return(output)
}

data_matrix <- create_dataset(truth_table, data_matrix)
colnames(data_matrix) <- c("D3", "D2", "D1", "D0", "A", "B", "C", "D", "E", "F", "G" )

### Train and Test dataset
set.seed(12345)
row.number <- sample(x =1:nrow(data_matrix), size = 0.75 *nrow(data_matrix))

x_train <- data_matrix[row.number,1:4]
y_train <- data_matrix[row.number,5:11]


x_test<- data_matrix[-row.number,1:4]
y_test<- data_matrix[-row.number,5:11]




### Neutral network model

model <- keras_model_sequential()
model %>%
  layer_dense(units = 64, activation = 'sigmoid', input_shape = c(4)) %>%
  layer_dropout(rate = 0.2) %>%
  
  layer_dense(units = 32, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.2) %>%
  
  layer_dense(units = 7, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)



history <- model %>% fit(
  x_train, y_train,
  epochs = 50, batch_size = 128,
  validation_split = 0.2
)
  # batch size = the number of training examples in one forward/backward pass. The higher the batch size, the more memory space you'll need
  # one epoch = one forward pass and one backward pass of all the training examples
plot(history)

model %>% evaluate(x_test, y_test)