# Use an official Tomcat runtime as a parent image
FROM tomcat:9-jre8

# Copy the WAR file to the Tomcat webapps directory
COPY target/my-rest-api-1.0-SNAPSHOT.war /usr/local/tomcat/webapps/

# Expose port 8080
EXPOSE 8080

# Run Tomcat
CMD ["catalina.sh", "run"]

