#Bar Plot urban areas 
H <- c(50000,20000,70000,30000,38000,68000,50000,47578,50000)
M <- c("Auto driver","Labour","teacher"," Farming"," Fishing","Trading","carpenter","Lawyer","Police")


# Plot the bar chart 
barplot(H,names.arg=M,xlab="Job",ylab="salary",col="blue",
        main="Income in Urban areas ",border="red")
 
 
#Line Plot
Urban <- c(50000,20000,70000,30000,38000,68000,50000,47578,50000)
# Give the chart file a name.
png(file = "line plot.jpg")

# Plot the bar chart. 
plot(Urban,type = "o")
# Save the file.
dev.off()

#Bar Plot for Rural areas
H <- c(30000,10000,50000,15000,30000,50000,40000,37578,40000)
M <- c("Auto driver","Labour","teacher"," Farming"," Fishing","Trading","carpenter","Lawyer","Police")


# Plot the bar chart 
barplot(H,names.arg=M,xlab="Job",ylab="salary",col="red",
        main="Income in rural areas",border="blue")
        
#Line Plot for Rural areas
Rural<- c(30000,10000,50000,15000,30000,50000,40000,37578,40000)
# Give the chart file a name.
png(file = "nota1.jpg")

# Plot the bar chart. 
plot(Rural,type = "o")

# Save the file.
dev.off()


