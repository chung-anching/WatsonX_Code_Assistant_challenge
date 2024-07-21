# // Assisted by WCA@IBM
# // Latest GenAI contribution: ibm/granite-20b-code-instruct-v2
# Use the official Tomcat image as the base
FROM tomcat:9-jre8-openjdk

# Copy the WAR file into the Tomcat webapps directory
COPY target/java-rest-api-docker.war /usr/local/tomcat/webapps/

# Copy the dataset.json file into the Tomcat webapps directory
COPY src/main/resources/dataset.json /usr/local/tomcat/webapps/

# Expose port 8080
EXPOSE 8080
