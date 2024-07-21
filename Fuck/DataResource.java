package Fuck;

public class fuck {
    
}
// Assisted by WCA@IBM
// Latest GenAI contribution: ibm/granite-20b-code-instruct-v2
package com.example.restapi;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/data")
public class DataResource {

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public String getData() throws IOException {
        String json = new String(Files.readAllBytes(Paths.get("src/main/resources/dataset.json")));
        return json;
    }
}
