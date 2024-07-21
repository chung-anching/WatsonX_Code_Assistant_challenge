# Use the OpenJDK 11 image as the base image
FROM openjdk:11

# Copy the compiled Java application into the Docker image
COPY target/my-java-app-1.0.jar /my-java-app.jar

# Set the working directory to the root directory of the Java application
WORKDIR /

# Run the Java application when the Docker container starts
CMD ["java", "-jar", "my-java-app.jar"]

