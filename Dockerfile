FROM ghcr.io/lambda-feedback/evaluation-function-base/wolfram:latest as base

# Interface to use for the evaluation function
ENV FUNCTION_INTERFACE="rpc"
ENV FUNCTION_RPC_TRANSPORT="tcp"
ENV FUNCTION_WORKER_START_TIMEOUT="60s"

ENV LOG_LEVEL="DEBUG"

# The shared Wolfram evaluation-function toolkit (JSON comms layer) is
# installed in the base image -- see evaluation-function-base/wolfram/Dockerfile.
#
# Optional local-dev override: run scripts/sync-local-toolkit.sh to populate
# ./.local-toolkit from a local toolkit-wolfram checkout before building, to
# test unreleased toolkit-wolfram changes here. Left empty (the default,
# tracked via .local-toolkit/.gitkeep), this COPY is a no-op and the image
# keeps using the toolkit-wolfram version pinned in the base image.
#COPY ./.local-toolkit /opt/lambda-feedback/toolkit-wolfram

COPY ./evaluate.m /app/evaluate.m
COPY ./preview.m /app/preview.m