
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Implement three scenarios. First scenario is buying shares on the first day when previous price is 0, second scenario is buying when price today is less than previous price, third scenario is on the last day of trading when you sell the shares.
for (i in 1:nrow(amd_df)) {
#buying on the first day of trading
  if (previous_price==0){
    amd_df$trade_type[i]<- "buy"
    amd_df$costs_proceeds[i]<- -amd_df$close[i]*share_size
    accumulated_shares<- share_size  }
  
#when price today is lesser than previous price  
  if (amd_df$close[i]<previous_price)
    {amd_df$trade_type[i]<-"buy"
    amd_df$costs_proceeds[i]<- -amd_df$close[i]*share_size
    accumulated_shares<- accumulated_shares +share_size}
  
#on the last day you sell the shares
  if (i== nrow(amd_df)){
    amd_df$trade_type[i]<-"sell"
    amd_df$costs_proceeds[i] <- amd_df$close[i]*accumulated_shares
    accumulated_shares<- 0}
   
    previous_price<- amd_df$close[i]
    amd_df$accumulated_shares[i]<-accumulated_shares
  }
print(amd_df)

```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
#defining the dates I chose
start_date <- as.Date("2021-12-17")
end_date <- as.Date("2023-06-29")
amd_df<- subset(amd_df,date >=start_date & date <= end_date)

```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Calculation of total profit or loss
total_profit_or_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE)
# Calculation of total capital invested
total_capital_invested<- -sum(amd_df$costs_proceeds[amd_df$trade_type =="buy"],na.rm= TRUE)
#Calculation of ROI
ROI<- 100*(total_profit_or_loss/total_capital_invested)
  
print("the total profit/loss is" )
print(total_profit_or_loss)
print("the total capital invested is")
print(total_capital_invested)
print("the ROI is")
print(ROI) 
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Setting the stop loss percentage
stop_loss<- 0.30

previous_price <- 0
share_size <- 100
accumulated_shares <- 0
average_purchase_price<-0
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  
amd_df$accumulated_shares <- 0 

#code from step 2 trading algorithm and another condition of stop loss mechanism implementation where half of shares sold when stock falls by 30% from average purchase price
for (i in 1:nrow(amd_df)) {
#buying on first day  
  if (previous_price==0){
    amd_df$trade_type[i]<- "buy"
    amd_df$costs_proceeds[i]<- -amd_df$close[i] * share_size
    accumulated_shares<- share_size 
    average_purchase_price<- -amd_df$close[i]}
  
#buy when price today less than previous price  
  if (amd_df$close[i]<previous_price)
    {amd_df$trade_type[i]<-"buy"
    amd_df$costs_proceeds[i]<- -amd_df$close[i]*share_size
    accumulated_shares<- accumulated_shares +share_size
    average_purchase_price<-(-amd_df$close[i]*share_size)/accumulated_shares}
#implement stop loss mechanism  
  if (amd_df$close[i]< average_purchase_price*(1-stop_loss) )
    {accumulated_shares<- 1/2*accumulated_shares
    amd_df$costs_proceeds[i]<-half_accumulated_shares*-amd_df$close[i]
    amd_df$trade_type<-"sell"}
    accumulated_shares<- accumulated_shares - 1/2*accumulated_shares
    
#sell on last day  
  if (i== nrow(amd_df)){
    amd_df$trade_type[i]<-"sell"
    amd_df$costs_proceeds[i] <-amd_df$close[i]*accumulated_shares
    accumulated_shares<- 0}
  
    previous_price<- amd_df$close[i]
    amd_df$accumulated_shares[i]<-accumulated_shares }
#calculate updated profit and ROI and capital invested
total_profit_or_loss_after_strategy<-sum(amd_df$costs_proceeds, na.rm= TRUE)
total_capital_invested_after_strategy<- -sum(amd_df$costs_proceeds[amd_df$costs_proceeds<0], na.rm= TRUE)
ROI_after_strategy<- (total_profit_or_loss_after_strategy/total_capital_invested_after_strategy)*100
print("the total profit after strategy employed is:")
print(total_profit_or_loss_after_strategy)
print("the total capital invested after strategy employed is:")
print(total_capital_invested_after_strategy)
print("the ROI is:")
print(ROI_after_strategy)

```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
On 14 February 2022, AMD acquired Xilinx for $35 billion dollars. This resulted in a decrease in profit by $2220636, from $383555 in my first strategy to -1837081 in my second strategy. In addition, ROI decreased by 120.60515%, with my first strategy yielding an ROI of 20.83128% and my stop loss mechanism yielding an ROI of -99.77387%. This is likely due to the costly acquisition and highly significant costs resulting from operational adjustments when Xilinx was acquired.
```





