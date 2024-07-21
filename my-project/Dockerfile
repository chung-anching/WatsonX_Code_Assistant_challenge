# Use the official JDK image to create a build artifact.
# https://hub.docker.com/_/openjdk
FROM openjdk:11 AS build
# FROM tomcat:9-jre8-openjdk

# Copy jar file into the working directory
COPY target/java-rest-api-1.0-SNAPSHOT.war ./

# Copy local code to the container image.
WORKDIR /app
# COPY pom.xml .
# RUN mvn dependency:go-offline

# # Build a release artifact.
# COPY src ./src
# RUN mvn package -DskipTests

# # Use the official Tomcat image to create a run artifact.
# # https://hub.docker.com/_/tomcat
# FROM tomcat:9-jre11-openjdk

# # Copy the WAR file from the build stage into the Tomcat image.
# COPY --from=build /app/target/*.war /usr/local/tomcat/webapps/ROOT.war

# # Set the Tomcat environment variables.
# ENV CATALINA_OPTS="-Djava.security.egd=file:/dev/./urandom"

# Expose port 8080 on the container.
EXPOSE 8080

# Start the Java application with Tomcat
CMD ["java", "-jar", "java-rest-api-1.0-SNAPSHOT.war"]




