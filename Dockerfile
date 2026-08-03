FROM ghcr.io/lambda-feedback/evaluation-function-base/wolfram:latest as base

# Interface to use for the evaluation function
ENV FUNCTION_INTERFACE="file"

ENV LOG_LEVEL="DEBUG"

COPY ./evaluate.m /app/evaluate.m
COPY ./preview.m /app/preview.m