// package my-app.src.main.java.com.example;

package com.example;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.databind.ObjectMapper;


@WebServlet("/api/data")
public class MyApp extends HttpServlet {

    private static final long serialVersionUID = 1L;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        // 读取 JSON 文件的内容
        String json = new String(Files.readAllBytes(Paths.get("src/main/resources/data.json")));

        // 使用 Jackson 库将 JSON 字符串转换为对象
        ObjectMapper mapper = new ObjectMapper();
        Object obj = mapper.readValue(json, Object.class);

        // 设置响应内容类型为 JSON 并写回响应
        response.setContentType("application/json");
        response.getWriter().write(mapper.writeValueAsString(obj));
    }
}
