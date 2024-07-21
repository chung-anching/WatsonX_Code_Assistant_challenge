// Assisted by WCA@IBM
// Latest GenAI contribution: ibm/granite-20b-code-instruct-v2
package com.example.java.rest.api.docker.controller;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

@RestController
@RequestMapping("/api")
public class DataController {

    @GetMapping(value = "/data", produces = MediaType.APPLICATION_JSON_VALUE)
    public String getData() throws IOException {
        return new String(Files.readAllBytes(Paths.get("dataset.json")));
    }
}
