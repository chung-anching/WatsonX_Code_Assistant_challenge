package Container2_GPT.backend.src.main.java.com.example;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Collectors;

@Path("/data")
public class RestApi {
    @GET
    public String getData() {
        try {
            return Files.lines(Paths.get("/usr/local/tomcat/webapps/dataset.json"))
                        .collect(Collectors.joining("\n"));
        } catch (IOException e) {
            e.printStackTrace();
            return "Error reading dataset";
        }
    }
}
