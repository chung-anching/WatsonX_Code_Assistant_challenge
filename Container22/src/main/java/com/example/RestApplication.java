package Container22.src.main.java.com.example;

import javax.ws.rs.ApplicationPath;
import org.glassfish.jersey.server.ResourceConfig;

@ApplicationPath("/api")
public class RestApplication extends ResourceConfig {
    public RestApplication() {
        packages("com.example");
    }
}
