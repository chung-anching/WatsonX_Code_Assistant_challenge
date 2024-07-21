package com.example;

import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.AbstractHandler;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Server extends AbstractHandler {
    private static final String DATASET_PATH = "dataset.json";

    @Override
    public void handle(String target, Request baseRequest, HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setContentType("application/json;charset=utf-8");
        response.setStatus(HttpServletResponse.SC_OK);

        if (request.getMethod().equals("GET")) {
            String json = new String(Files.readAllBytes(Paths.get(DATASET_PATH)));
            PrintWriter writer = response.getWriter();
            writer.println(json);
            writer.close();
        } else {
            response.setStatus(HttpServletResponse.SC_METHOD_NOT_ALLOWED);
        }

        baseRequest.setHandled(true);
    }

    public static void main(String[] args) throws Exception {
        Server server = new Server(8080);
        server.start();
        server.join();
    }

    public Server(int port) {
        try {
            org.eclipse.jetty.server.Server server = new org.eclipse.jetty.server.Server(port);
            server.setHandler(this);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
