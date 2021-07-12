library(dplyr)
library(data.table)
library(caret)
library(FNN)

## generating a train -and testsest
set.seed(1)
trn_index = createDataPartition(y = final_clean$number_of_checkouts, p = 0.70, 
                                list = FALSE)
trn_checkouts = final_clean[trn_index, ]
tst_checkouts = final_clean[-trn_index, ]


# fit regression 
reg <- lm(number_of_checkouts ~ PublicationYear+CheckoutDateTime_1+CheckoutDateTime_2+
            CheckoutDateTime_3+CheckoutDateTime_4+CheckoutDateTime_5+CheckoutDateTime_6+
            CheckoutDateTime_7+CheckoutDateTime_8+CheckoutDateTime_9+ItemType_acbk+ItemType_accas+
            ItemType_accd+ItemType_accdrom+ItemType_acdvd+ItemType_aceq+ItemType_acmap+ItemType_acmus+
            ItemType_acpam+ItemType_acper+ItemType_alaptop+ItemType_arbk+ItemType_ardvd+ItemType_arper+
          ItemType_atablet+ItemType_bcbk+ItemType_bccd+ItemType_bcdvd+ItemType_dcillb+ItemType_jcbk+
            ItemType_jccas+ItemType_jccd+ItemType_jcdvd+ItemType_jckit+ItemType_jcmus+ItemType_jrbk+
            ItemType_jrbk+ItemType_pkbknh+ItemType_ucfold+ItemType_ucunkn, data = trn_checkouts)

reg01 <- lm(number_of_checkouts ~ PublicationYear+CheckoutDateTime_1+CheckoutDateTime_2+
            CheckoutDateTime_3+CheckoutDateTime_4+CheckoutDateTime_5+CheckoutDateTime_6+
            CheckoutDateTime_7+CheckoutDateTime_8+CheckoutDateTime_9+ItemType_acbk+ItemType_accas+
            ItemType_accd+ItemType_acdvd+ItemType_acmap+ItemType_acmus+
            +ItemType_arbk+ItemType_bcbk+ItemType_jcbk+ItemType_jccd+ItemType_jcdvd+
            ItemType_jckit+ItemType_jcmus+ItemType_jrbk+
            ItemType_jrbk+ItemType_pkbknh, data = trn_checkouts)

summary(reg01)

accuracy_train <- trn_checkouts %>%
  mutate(pred_reg_train = predict(reg01))

mse <- accuracy_train %>%
  mutate(error = pred_reg_train - number_of_checkouts,
         sq_error = error^2) %>%
  summarise(mean(sq_error))

rmse <- sqrt(mse) # 32.43

# check RMSE on the test data
pred_reg_test <- predict(reg01, newdata = tst_checkouts)
reg_rms_test <- sqrt(mean((pred_reg_test - tst_checkouts$number_of_checkouts)^2))

rmse <- sqrt() # 33.577 , quite good!!



