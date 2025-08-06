FROM ghcr.io/lambda-feedback/evaluation-function-base/wolfram:latest AS final

# Use the WSTP server as a kernel backend
ENV WSTP_SERVER="true"

# Command to start the evaluation function with
ENV FUNCTION_COMMAND="wolframscript"

# Args to start the evaluation function with
ENV FUNCTION_ARGS="-f,/app/evaluation_function.wl"

# Interface to use for the evaluation function
ENV FUNCTION_INTERFACE="file"

# Copy the evaluation function to the app directory
COPY ./evaluation_function.wl /app/evaluation_function.wl

# Final layer for private images, which contains the wolfram licence key,
# and is started with the shimmy handle command.
FROM final AS final-private

# Copy the mathpass secret to the Wolfram Engine licensing directory.
# See https://hub.docker.com/r/wolframresearch/wolframengine for more information.
RUN --mount=type=secret,id=mathpass \
    mkdir -p /tmp/home/.WolframEngine/Licensing && \
    cp /run/secrets/mathpass /tmp/home/.WolframEngine/Licensing/mathpass


RUN apt-get update && apt-get install -y \
    netcat \
    && rm -rf /var/lib/apt/lists/*

COPY ./entrypoint.sh /entrypoint.sh