# Install the package from CRAN
install.packages("AdhereR", type = "binary")

# # Get the development version from github
# install.packages("devtools")
# library(devtools)
# install_github("ddediu/AdhereR@devel")

# load package
library(AdhereR)

# get example medication events
data <- med.events

# compute CMA7
cma7 <- CMA7(data=data,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y")

#plot the first patient
plot(cma7, patients.to.plot = 1)
