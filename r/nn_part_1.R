library(neuralnet)

## Defining neural net parameters
time1 <- Sys.time()
nn <- neuralnet::neuralnet(plan.losses ~ real.system_balance + real.ac_balance + real.production + real.consumption, 
                           data=trainset,hidden=c(3,8,3), linear.output=FALSE, threshold=0.1)

print(paste0("Time it took to train - ",Sys.time()-time1))

## seeing the results and plotting the nn for no obvious reason
nn$result.matrix
plot(nn)


#Test the resulting output
temp_test <- subset(testset, select = c("real.system_balance", "real.ac_balance", "real.production" ,"real.consumption","real.production_renewable"))
head(temp_test)

## predicting values
nn.results <- neuralnet::compute(nn, temp_test)

## combining results
results <- data.frame(actual = testset$plan.losses, prediction = nn.results$net.result) %>%
  mutate(row_id = row_number(),
         dev = (actual - prediction))

## plotting the model accuracy

print(paste0("Model accuracy is: ",round(1-abs(mean(results$dev)),3),"%"))

## Unscaling to real values

# final_results <- unscale(nn.results$net.result,data_num)


ggplot(results[(nrow(results)-100):nrow(results),])+
  geom_line(aes(x = row_id, y = actual), color = "blue")+
  geom_line(aes(x = row_id, y = prediction), color = "red")+
  scale_y_continuous(limits = c(0,1))



ggplot(results)+
  geom_point(aes(x = actual, y = prediction))+
  scale_y_continuous(limits = c(0.25,1))+
  scale_x_continuous(limits = c(0.25,1))


ggplot(results, aes(dev))+
  geom_histogram()
