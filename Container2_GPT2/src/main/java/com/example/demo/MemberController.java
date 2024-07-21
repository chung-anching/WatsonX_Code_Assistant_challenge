package Container2_GPT2.src.main.java.com.example.demo;


import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.IOException;

@RestController
public class MemberController {

    @GetMapping("/members")
    public String getMembers() throws IOException {
        String path = "/mnt/data/dataset.json";
        return new String(Files.readAllBytes(Paths.get(path)));
    }
}
