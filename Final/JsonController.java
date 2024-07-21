package Final;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api")
public class JsonController {

   @GetMapping("/json")
   public String getJson() throws IOException {
       return JsonReader.readJsonFile("data.json");
   }
}