# Use the official Nginx image from the Docker Hub
FROM nginx:latest

# Copy the custom configuration file from the current directory
# to the Nginx configuration directory
COPY default.conf /etc/nginx/conf.d/

# Copy the HTML file to the Nginx default static files directory
COPY index.html /usr/share/nginx/html/

# Expose port 80 to the outside world
EXPOSE 80

# Start Nginx when the container launches
CMD ["nginx", "-g", "daemon off;"]




# # Use the official NGINX image from Docker Hub
# FROM nginx:latest

# # Copy your HTML files into the container
# COPY index.html /usr/share/nginx/html

# # Expose port 80
# EXPOSE 80

# # Start NGINX when the container launches
# CMD ["nginx", "-g", "daemon off;"]




