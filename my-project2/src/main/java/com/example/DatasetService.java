// package my-project2.src.main.java.com.example;
package com.example;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.io.FileReader;
import java.io.IOException;

@Path("/dataset")
public class DatasetService {

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public String getDataset() throws IOException, ParseException {
        JSONParser parser = new JSONParser();
        Object obj = parser.parse(new FileReader("src/main/resources/dataset.json"));
        JSONObject json = (JSONObject) obj;
        return json.toJSONString();
    }
}


