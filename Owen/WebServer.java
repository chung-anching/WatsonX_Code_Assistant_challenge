// import com.google.gson.Gson;
import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class WebServer {
    public static void main(String[] args) throws IOException {
        // Create a new HTTP server on port 8000
        HttpServer server = HttpServer.create(new InetSocketAddress(8000), 0);

        // Register a handler for the "/api" endpoint
        server.createContext("/api", exchange -> {
            // Check if the request method is GET
            if (exchange.getRequestMethod().equals("GET")) {
                // Read the data.json file
                String dataJson = Files.readString(Paths.get("data.json"));

                // Set the response headers and status code
                exchange.getResponseHeaders().set("Content-Type", "application/json");
                exchange.sendResponseHeaders(200, dataJson.length());

                // Write the response body
                OutputStream outputStream = exchange.getResponseBody();
                outputStream.write(dataJson.getBytes());
                outputStream.close();
            } else {
                // Send an error message if the request method is not GET
                exchange.sendResponseHeaders(405, 0);
            }
        });

        // Start the HTTP server
        server.start();
    }

    private static void sendResponse(HttpExchange exchange, int responseCode, String responseBody) throws IOException {
        Headers responseHeaders = exchange.getResponseHeaders();
        responseHeaders.set("Access-Control-Allow-Origin", "*");
        exchange.sendResponseHeaders(responseCode, responseBody.length());
        OutputStream os = exchange.getResponseBody();
        os.write(responseBody.getBytes());
        os.close();
    }
}

// This program uses the sendResponse method to set the Access-Control-Allow-Origin header and send the response body. Note that you will need to replace the hard-coded path "data.json" with the actual path to your data file.
