# Assisted by WCA@IBM
# // Latest GenAI contribution: ibm/granite-20b-code-instruct-v2

# Use the official maven/Java 8 image to create a build artifact.
# https://hub.docker.com/_/maven
FROM maven:3.6-jdk-8-alpine AS MAVEN_BUILD

# Copy local code to the container image.
WORKDIR /app
COPY pom.xml .
# COPY src ./src

# Build a release artifact.
RUN mvn package -DskipTests

# Use the official tomcat image as the runtime environment.
# https://hub.docker.com/_/tomcat
FROM tomcat:9-jre8-alpine

# Copy the WAR file to the Tomcat webapps directory
COPY --from=MAVEN_BUILD /app/target/*.war /usr/local/tomcat/webapps/

# Copy dataset.json to the Tomcat webapps directory
COPY --from=MAVEN_BUILD app/src/main/resources/dataset.json /usr/local/tomcat/webapps/

# Set the CATALINA_OPTS environment variable
ENV CATALINA_OPTS "-Djava.security.egd=file:/dev/./urandom"

# Expose port 8080
EXPOSE 8080

# Start Tomcat
CMD ["catalina.sh", "run"]

