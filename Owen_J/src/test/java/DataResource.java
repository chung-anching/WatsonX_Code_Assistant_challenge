package Owen_J.src.test.java;
// Assisted by WCA@IBM
// Latest GenAI contribution: ibm/granite-20b-code-instruct-v2
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;


@Path("/data")
public class DataResource {

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public List<Member> getData() throws IOException {
        String filePath = "/path/to/dataset.json";
        BufferedReader reader = new BufferedReader(new FileReader(filePath));
        StringBuilder jsonString = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null) {
            jsonString.append(line);
        }
        reader.close();

        Gson gson = new Gson();
        Member[] members = gson.fromJson(jsonString.toString(), Member[].class);
        List<Member> memberList = new ArrayList<>();
        for (Member member : members) {
            memberList.add(member);
        }
        return memberList;
    }

}
