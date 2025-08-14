# Wolfram Language Evaluation Function

This repository contains the boilerplate code needed to create a containerized evaluation function written in Wolfram Language.

## Quickstart

This chapter helps you to quickly set up a new Wolfram evaluation function using this template repository.

> [!NOTE]
> After setting up the evaluation function, delete this chapter from the `README.md` file, and add your own documentation.

#### 1. Create a new repository

- In GitHub, choose `Use this template` > `Create a new repository` in the repository toolbar.

- Choose the owner, and pick a name for the new repository.

  > [!IMPORTANT]
  > If you want to deploy the evaluation function to Lambda Feedback, make sure to choose the Lambda Feedback organization as the owner.

- Set the visibility to `Public` or `Private`.

  > [!IMPORTANT]
  > If you want to use GitHub [deployment protection rules](https://docs.github.com/en/actions/deployment/targeting-different-environments/using-environments-for-deployment#deployment-protection-rules), make sure to set the visibility to `Public`.

- Click on `Create repository`.

#### 2. Clone the new repository

Clone the new repository to your local machine using the following command:

```bash
git clone <repository-url>
```

#### 3. Configure the evaluation function

When deploying to Lambda Feedback, set the evaluation function name in the `config.json` file. Read the [Deploy to Lambda Feedback](#deploy-to-lambda-feedback) section for more information.

#### 4. Develop the evaluation function

You're ready to start developing your evaluation function. Head over to the [Development](#development) section to learn more.

#### 5. Update the README

In the `README.md` file, change the title and description so it fits the purpose of your evaluation function.

Also, don't forget to delete the Quickstart chapter from the `README.md` file after you've completed these steps.

## Usage

You can run the evaluation function either using [the pre-built Docker image](#run-the-docker-image) or build and run [the binary executable](#build-and-run-the-binary).

### Run the Docker Image

The pre-built Docker image comes with [Shimmy](https://github.com/lambda-feedback/shimmy) installed.

> [!TIP]
> Shimmy is a small application that listens for incoming HTTP requests, validates the incoming data and forwards it to the underlying evaluation function. Learn more about Shimmy in the [Documentation](https://github.com/lambda-feedback/shimmy).

The pre-built Docker image is available on the GitHub Container Registry. You can run the image using the following command:

```bash
docker run -p 8080:8080 ghcr.io/lambda-feedback/evaluation-function-boilerplate-wolfram:latest
```

### Run the Script

You can choose between running the Wolfram evaluation function itself, ore using Shimmy to run the function.

**Raw Mode**

Use the following command to run the evaluation function directly:

```bash
wolframscript -f evaluation_function.wl request.json response.json
```

This will run the evaluation function using the input data from `request.json` and write the output to `response.json`.

**Shimmy**

To have a more user-friendly experience, you can use [Shimmy](https://github.com/lambda-feedback/shimmy) to run the evaluation function.

To run the evaluation function using Shimmy, use the following command:

```bash
shimmy -c "wolframscript" -a "-f" -a "evaluation_function.wl" -i file
```

## Development

### Prerequisites

- [Docker](https://docs.docker.com/get-docker/)
- [Wolfram Engine](https://www.wolfram.com/engine/)
- [Wolfram Engine License](#development-license)

### Repository Structure

```bash
.github/workflows/
    build.yml          # builds the public evaluation function image
    deploy.yml         # deploys the evaluation function to Lambda Feedback

evaluation_function.wl # evaluation function source code

config.json            # evaluation function deployment configuration file
```

### Development Workflow

In its most basic form, the development workflow consists of writing the evaluation function in the `evaluation_function.wl` file and testing it locally. As long as the evaluation function adheres to the Evaluation Function API, a development workflow which incorporates using Shimmy is not necessary.

Testing the evaluation function can be done by running the script using the Wolfram Engine / WolframScript like so:

```bash
wolframscript -f evaluation_function.wl request.json response.json
```

> [!NOTE]
> Put the input data in the `request.json` file, and the output will be written to the `response.json` file.

### Building the Docker Image

To build the Docker image, run the following command:

```bash
docker build -t my-wolfram-evaluation-function .
```

## Deployment

This section guides you through the deployment process of the evaluation function. If you want to deploy the evaluation function to Lambda Feedback, follow the steps in the [Lambda Feedback](#deploy-to-lambda-feedback) section. Otherwise, you can deploy the evaluation function to other platforms using the [Other Platforms](#deploy-to-other-platforms) section.

### Deploy to Lambda Feedback

Deploying the evaluation function to Lambda Feedback is simple and straightforward, as long as the repository is within the [Lambda Feedback organization](https://github.com/lambda-feedback).

After configuring the repository, a [GitHub Actions workflow](.github/workflows/deploy.yml) will automatically build and deploy the evaluation function to Lambda Feedback as soon as changes are pushed to the main branch of the repository.

**Configuration**

The deployment configuration is stored in the `config.json` file. Choose a unique name for the evaluation function and set the `EvaluationFunctionName` field in [`config.json`](config.json).

> [!IMPORTANT]
> The evaluation function name must be unique within the Lambda Feedback organization, and must be in `lowerCamelCase`. You can find a example configuration below:

```json
{
  "EvaluationFunctionName": "compareStringsWithWolfram"
}
```

### Deploy to other Platforms

If you want to deploy the evaluation function to other platforms, you can use the Docker image to deploy the evaluation function.

Please refer to the deployment documentation of the platform you want to deploy the evaluation function to.

If you need help with the deployment, feel free to reach out to the Lambda Feedback team by creating an issue in the template repository.

## Wolfram Engine License

Wolfram Engine requires a valid license to run. For developing purposes, you can obtain a free Wolfram Engine license. This process is described in the following steps. If you want to read more about licensing, please refer to the [Wolfram Engine Licensing Documentation](https://hub.docker.com/r/wolframresearch/wolframengine).

### Production License

**1. Sign in to Wolfram Cloud**

Head over to the [Wolfram Cloud](https://www.wolframcloud.com/) and sign in or create a new Wolfram ID. If you don't have a Wolfram subscription, you can sign up for a free Wolfram Cloud Basic subscription.

**2. Create License Entitlement**

After signing in, open a Wolfram Cloud notebook and evaluate the [`CreateLicenceEntitlement`](https://reference.wolfram.com/language/ref/CreateLicenseEntitlement.html) function.

```text
In[1]:= entitlement = CreateLicenseEntitlement[]

Out[1]= LicenseEntitlementObject[O-WSTD-DA42-GKX4Z6NR2DSZR, ...]
```

**3. Obtain the License Key**

Run the following command to obtain the entitlement ID:

```text
In[2]:= entitlement["EntitlementID"]

Out[2]= O-WSTD-DA42-GKX4Z6NR2DSZR
```

**4. Use the License Key**

Create an environment variable named `WOLFRAMSCRIPT_ENTITLEMENTID` with the entitlement ID:

```text
WOLFRAMSCRIPT_ENTITLEMENTID=O-WSTD-DA42-GKX4Z6NR2DSZR
```

This environment variable activates Wolfram Engine when running the `wolframscript` command.

### Development License

**1. Sign in or create a Wolfram ID.**

Head over to the [Wolfram Account Portal](https://account.wolfram.com/login/create) and sign in or create a new account.

**2. Get the Wolfram Engine license**

[Obtain the free license](https://www.wolfram.com/engine/free-license/) by following the instructions.

**3. Activate the Wolfram Engine license**

Run the following command and enter your Wolfram Account credentials to generate a password for the license:

```bash
docker run -it wolframresearch/wolframengine
```

While still in the container, run the following command to print the password:

```plain
In[1] := $PasswordFile // FilePrint
1e1d781ed0a3    6520-03713-97466        4304-2718-2K5ATR        5095-179-696:2,0,8,8:80001:20190627
```

This gives you a password that you can copy to a `mathpass` file on your host machine.

**4. Run the Wolfram Engine container**

Run the following command to start the Wolfram Engine container with the license:

```bash
docker run -it --rm -v $(pwd)/mathpass:/home/wolframengine/.WolframEngine/Licensing/mathpass wolframresearch/wolframengine
```

This command assumes that you have a `mathpass` file in the current directory, and the container is started with the `wolframengine` user.

## FAQ

### Pull Changes from the Template Repository

If you want to pull changes from the template repository to your repository, follow these steps:

1. Add the template repository as a remote:

```bash
git remote add template https://github.com/lambda-feedback/evaluation-function-boilerplate-wolfram.git
```

2. Fetch changes from all remotes:

```bash
git fetch --all
```

3. Merge changes from the template repository:

```bash
git merge template/main --allow-unrelated-histories
```

> [!WARNING]
> Make sure to resolve any conflicts and keep the changes you want to keep.
