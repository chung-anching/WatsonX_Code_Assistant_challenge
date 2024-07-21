package Owen_Anderson;
// Assisted by WCA@IBM
// Latest GenAI contribution: ibm/granite-20b-code-instruct-v2
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;

public class WebServer {
    public static void main(String[] args) throws IOException {
        // Create a ServerSocket to listen for connections from clients
        ServerSocket serverSocket = new ServerSocket(8080);
        System.out.println("Server started on port 8080");

        // Loop forever, listening for and accepting connections from clients
        while (true) {
            Socket socket = serverSocket.accept();
            System.out.println("Connection established");

            // Create a BufferedReader to read input from the client socket
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));

            // Read the request line from the client
            String requestLine = in.readLine();
            System.out.println("Request received: " + requestLine);

            // Check if the request is a GET request
            if (requestLine.startsWith("GET")) {
                // Get the filename from the request
                String filename = requestLine.split(" ")[1];

                // Check if the filename is "api/members.json"
                if (filename.equals("/api/members.json")) {
                    // Send the HTTP response header
                    socket.getOutputStream().write("HTTP/1.1 200 OK\r\n".getBytes());
                    socket.getOutputStream().write("Content-Type: application/json\r\n\r\n".getBytes());

                    // Read the members.json file and send it to the client
                    FileReader fileReader = new FileReader("C:\\Users\\yourusername\\Desktop\\members.json");
                    int c;
                    while ((c = fileReader.read()) != -1) {
                        socket.getOutputStream().write(c);
                    }
                    fileReader.close();
                } else {
                    // Send an error message to the client
                    socket.getOutputStream().write("HTTP/1.1 404 Not Found\r\n\r\n".getBytes());
                    socket.getOutputStream().write("File not found.".getBytes());
                }
            } else {
                // Send an error message to the client
                socket.getOutputStream().write("HTTP/1.1 405 Method Not Allowed\r\n\r\n".getBytes());
                socket.getOutputStream().write("Method not allowed.".getBytes());
            }

            // Close the input stream and socket
            in.close();
            socket.close();
        }
    }
}
