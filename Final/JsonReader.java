package Final;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class JsonReader {

   public static String readJsonFile(String fileName) throws IOException {
       ClassLoader classLoader = JsonReader.class.getClassLoader();
       return Files.readString(Paths.get(classLoader.getResource(fileName).getPath()));
   }
}