# Use the official Tomcat image from the Docker Hub
FROM tomcat:9-jre8-openjdk

ENV CATALINA_HOME /usr/local/tomcat

# Copy the WAR file to the Tomcat webapps directory
COPY target/restapi.war /usr/local/tomcat/webapps/

# # Copy the dataset.json file to the Tomcat webapps directory
# COPY dataset.json /usr/local/tomcat/webapps/

# Expose port 8080
EXPOSE 8080

# Start Tomcat
CMD ["catalina.sh", "run"]

