// Assisted by WCA@IBM
// Latest GenAI contribution: ibm/granite-20b-code-instruct-v2
# Use the latest version of the Nginx image
FROM nginx:latest

# Copy the index.html file into the /usr/share/nginx/html directory
COPY index.html /usr/share/nginx/html

# Expose port 80 on the host machine
EXPOSE 80