// package Finalll.src.main.java.com.example.jsonreader;
// package com.example.jsonreader;

import com.fasterxml.jackson.databind.ObjectMapper;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;
@WebServlet("/readjson")
public class JsonReaderServlet extends HttpServlet {
    private static final long serialVersionUID = 1L;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        Path path = Paths.get(getServletContext().getRealPath("/WEB-INF/data.json"));
        String json = new String(Files.readAllBytes(path));

        response.setContentType("application/json");
        response.getWriter().write(json);
    }
}

// import javax.servlet.ServletException;
// import javax.servlet.annotation.WebServlet;
// import javax.servlet.http.HttpServlet;
// import javax.servlet.http.HttpServletRequest;
// import javax.servlet.http.HttpServletResponse;
// import java.io.IOException;
// import java.nio.file.Files;
// import java.nio.file.Paths;
// import java.nio.file.Path;

// @WebServlet("/readjson")
// public class JsonReaderServlet extends HttpServlet {
//     private static final long serialVersionUID = 1L;

//     @Override
//     protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
//         Path path = Paths.get(getServletContext().getRealPath("/WEB-INF/data.json"));
//         String json = new String(Files.readAllBytes(path));

//         response.setContentType("application/json");
//         response.getWriter().write(json);
//     }
// }
