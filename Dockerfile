FROM ghcr.io/lambda-feedback/evaluation-function-base/wolfram:latest as base

# Command to start the evaluation function with
ENV FUNCTION_COMMAND="wolframscript"

# Args to start the evaluation function with
ENV FUNCTION_ARGS="-f,/app/evaluation_function.wl"

# Interface to use for the evaluation function
ENV FUNCTION_INTERFACE="file"

ENV LOG_LEVEL="DEBUG"

# Copy the evaluation function to the app directory
COPY ./evaluation_function.wl /app/evaluation_function.wl

FROM base AS with-license
COPY ./dist/LICENSE.txt /home/wolframengine/.WolframEngine/Licensing/mathpass

FROM base AS without-license
# no COPY, no error

# Choose final stage with build arg
ARG WITH_LICENSE=false
FROM ${WITH_LICENSE:+with-license}${WITH_LICENSE:-without-license}