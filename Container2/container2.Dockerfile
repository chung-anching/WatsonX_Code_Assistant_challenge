# // Assisted by WCA@IBM
# // Latest GenAI contribution: ibm/granite-20b-code-instruct-v2
# Use the official Tomcat image as the base image
FROM tomcat:9-jre8-openjdk

# Copy the WAR file to the Tomcat webapps directory
COPY target/restapi.war /usr/local/tomcat/webapps/
