FROM ghcr.io/lambda-feedback/evaluation-function-base/wolfram:latest as base

RUN apt-get install curl

# Command to start the evaluation function with
ENV FUNCTION_COMMAND="bash"

# Args to start the evaluation function with
# ENV FUNCTION_ARGS="-f,/app/evaluation_function.wl"
ENV FUNCTION_ARGS="-f,/app/test.sh"

# Interface to use for the evaluation function
ENV FUNCTION_INTERFACE="file"

ENV LOG_LEVEL="DEBUG"

# Copy the evaluation function to the app directory
COPY ./evaluation_function.wl /app/evaluation_function.wl
COPY ./test.sh /app/test.sh