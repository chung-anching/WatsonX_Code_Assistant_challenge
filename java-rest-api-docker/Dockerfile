# // Assisted by WCA@IBM
# // Latest GenAI contribution: ibm/granite-20b-code-instruct-v2
FROM maven:3.8.6-openjdk-17-slim AS build
WORKDIR /app
COPY pom.xml .
RUN mvn dependency:go-offline
COPY src ./src
RUN mvn package

FROM openjdk:17-jdk-alpine
WORKDIR /app
COPY --from=build /app/target/java-rest-api-docker-1.0-SNAPSHOT.jar ./java-rest-api-docker.jar
EXPOSE 8080
CMD ["java", "-jar", "java-rest-api-docker.jar"]

