#source('./src/PSE_Reshape.R') # to prep data if needed.

# push long dataset to Google Docs
library(RGoogleDocs)

gusername <- readline('Enter Google Docs/Drive username: ')
gpassword <- readline('Enter Google Docs/Drive password: ')

auth = getGoogleAuth(gusername, gpassword)
con = getGoogleDocsConnection(auth)
docname = 'PSE_change_long'
csvtoupload = './data-output/PSE_change_long.csv'
remove = deleteDoc(docname,con, auth)
upload = uploadDoc(csvtoupload, auth=auth, con=con, name=docname,
                   type='text/csv', asText=FALSE)