    <h1>Sales Generator</h1>

    <p>This shiny app was built to generate daily sales data for the SAP CAR module. The main idea is that you can generate random sales for each day which then get weighted by different the day of the week, the months and the years. Further, you can create influencing factors and effects for holidays, other external events, and offers.</p>

    <p>The app is uploaded to <a href="https://fvr1210.shinyapps.io/sales_generator/" target="_blank">fvr1210.shinyapps.io/sales_generator/</a></p>
    <p>GitHub Repository: <a href="https://github.com/fvr1210/sales_generator" target="_blank">github.com/fvr1210/sales_generator</a></p>

    <h2>Guide</h2>

    <p><strong>Choose CSV File:</strong> At the end of the application, you can download the parameters you used to create fake sales data for a product location as a CSV file. With this feature, you can then upload this file the next time, so you don’t have to go through all the steps every time if you want to create new fake sales with the same or similar parameter settings. Events and Offer settings are saved in this parameter file and cannot be uploaded.</p>

    <p><strong>Separator:</strong> What separator the chosen CSV file has</p>

    <p><strong>Sales Dates:</strong> The End date is set by default by today and the start date by today minus 729 days. When you change the dates, you need to press the “Update Dates” button.</p>

    <p><strong>Master Data:</strong> This data should match with the data you have for the product location in your CAR system.</p>

    <p><strong>Variable weight:</strong> A weight of 1 is set for the most variables as standard. If you choose a number below 1, this variable has a lower weight -> tends to decrease the sales, and if you choose a number higher than 1, it gets a higher weight -> tends to increase the sales. If you use a zero all days where this parameter is active will have zero sales.</p>

    <p><strong>Effects (do not change the Effect Name):</strong> You can increase or decrease the sales over a certain period you choose. Changing the number of effects will remove any settings you already did for the effects. Make sure to choose first the right number of effects.</p>

    <p><strong>Offers (do not change the Offers Name):</strong> Offers do not work with weighting but additional sales are generated. Changing the number of offers will remove any settings you already did for the offers. Make sure to choose first the right number of offers. You can decide if you want to keep the normal sales in the chosen time period (for example if you have a conditional offer) or if you want to have only offer sales.</p>

    <p><strong>Sales Data:</strong> You can choose between two different sales patterns. “Normal” uses the passion distribution to create random sales where the mean is the chosen expected average sales value. “Intermittent” can be used to simulate products where you have a lot of periods with zero sales. Here you can choose the average number of days you don’t have sales for this product-location in a row and the average demand once you have sales.</p>

    <p><strong>Generate Sales data:</strong> Once you are done with choosing the parameters click on this button to generate the fake sales data. You then get two time series graphs of the simulated sales. The first one shows the daily sales, the second one the sales by week.</p>

    <p><strong>Outputfile:</strong> You can choose between the format for a BI_SALES upload or to fake POSDAT. When you use BI_SALES you can add the number of locations you want to get the exact same fake sales data.</p>
