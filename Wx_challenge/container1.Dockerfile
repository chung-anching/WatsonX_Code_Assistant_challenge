# # Use the latest version of the Nginx image
# FROM nginx:latest

# # Copy the index.html file into the /usr/share/nginx/html directory
# COPY index.html /usr/share/nginx/html

# # Expose port 80 on the host machine
# EXPOSE 80


# Use the official NGINX image from Docker Hub
FROM nginx:latest

# Copy your HTML files into the container
COPY index.html /usr/share/nginx/html

# Expose port 80
EXPOSE 80

# Start NGINX when the container launches
CMD ["nginx", "-g", "daemon off;"]