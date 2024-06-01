This app simulates bracketed leverage tokens using the R programming language and the shiny web framework.

You can clone this repository and run the program locally using RStudios or you can build a docker image using the dockerfile and then creating a docker container that runs that image.

Commands:

docker build -t shinyapp .

docker run -p 8180:8180 -name leverage_simulation shinyapp

A list of things I want to add:
- All price history of all assets
- More tokens to select from
- Date ranges to isolate different areas of time
