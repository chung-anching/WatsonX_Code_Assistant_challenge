# // Assisted by WCA@IBM
# // Latest GenAI contribution: ibm/granite-20b-code-instruct-v2

# FROM maven:3.8.6-openjdk-17-slim  AS build

# Build stage
FROM maven:3.8.5-openjdk-17-slim AS build

WORKDIR /my-app
COPY pom.xml .
RUN mvn dependency:go-offline
COPY . .
RUN mvn clean package

# Final stage
FROM tomcat:9.0.56-jdk17-openjdk-slim
COPY --from=build /my-app/target/my-app-1.0-SNAPSHOT.war maven:3.8.5-openjdk-17-slim AS build
EXPOSE 8080
CMD ["catalina.sh", "run"]
