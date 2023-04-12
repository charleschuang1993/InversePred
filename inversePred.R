#' 反預測(inverse prediction)
#' 需要建立模擬樣本集，擴充response與variables之間的組合
#' @param data 完整的表格，含自變數
#' @param variable 輸入自變數名稱(單一)
#' @param size 輸入想要生成多少個模擬樣本，預設1000
#' @param mode 輸入不同的生成樣本模式。g: 常態分佈，預設標準差為1，以初始樣本平均作為平均值。 s: 基於初始樣本抽樣。 int: 基於初始樣本，最小與最大值之間切割等差數列。 
#' @param seg 設定等差數列之間固定的間隔數值，預設為1
#' @return 回傳生成的模擬樣本集
#' @export

simulated_single_variable_value <- function(data, variable, size = 1000, mode="s", seg = 1){
    if(mode=="g"){
        #new_data <- sapply(seq_len(nrow(data)),function(i) sample(round(rnorm(1000, data[i,variables], 1), 2),size))
        new_data <- round(rnorm(size, mean(data[[variables]]), 1), 2)
    }else if(mode == "int"){
        new_data <- sample(seq(min(data[[variables]]), max(data[,variables]),seg),size,replace=TRUE)
    }else if(mode == "s"){
        new_data <- sample(data[[variables]], size, replace=TRUE)
    }
    new_data <- as.data.frame(new_data)
    colnames(new_data ) <- variables
    return(new_data)
}

#' 需要建立模擬樣本集，擴充response與variables之間的組合。記得各個自變數要設定相對應的mode，依照對數值的特性。
#' @param data 完整的表格，含自變數
#' @param variable 輸入自變數名稱(複數)
#' @param size 輸入想要生成多少個模擬樣本，預設1000
#' @param mode 輸入不同的生成樣本模式 (複數)。g: 常態分佈，預設標準差為1，以初始樣本平均作為平均值。 s: 基於初始樣本抽樣。 int: 基於初始樣本，最小與最大值之間切割等差數列。 
#' @param seg 設定等差數列之間固定的間隔數值，預設為1
#' @return 回傳生成的模擬樣本集
#' @export


sim_multi_vars_value <- function(data, variables, size, mode){
    seq_ <- lapply(c(variables), function(v) 1/10^max(sapply(data[[v]], decimalplaces)))
    new_data  <- lapply(1:length(variables), function(x) simulated_single_variable_value(data, variables[x], size , mode[x],seq_[[x]]))
    new_data <- as.data.frame(new_data)
    colnames(new_data ) <- variables
    return(new_data)
 }

#' 針對單一數值，判斷有幾位小數
#' @param x 待判斷數值
#' @return 回傳小數點後有幾位數
#' @export

decimalplaces <- function(x) {
    if ((x %% 1) != 0) {
        nchar(unlist(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE))[[2]])
    } else {
        return(0)
    }
}


#' 進行反預測
#' @param data 完整的表格，含自變數
#' @param model 輸入已經被建立好的預測model
#' @param target_value 輸入應變數目標值
#' @param simulated_data 將生成的模擬樣本集帶入
#' @param return_number 回傳與目標值最接近的值之數量 
#' @return 回傳得到近似目標值所需的自變數
#' @export
#' 
inverse_prediction <- function(model, target_value, simulated_data, return_number){
    predicted_values <- predict(model, simulated_data)
    simulated_data$pred_ <- predicted_values 
    ordered_<- simulated_data[order(abs(predicted_values-target_value)),]
    final_ <- ordered_[1:return_number,]
    close_true_value <- which.min(abs(predicted_values -target_value))
    return(list(final_))
 }