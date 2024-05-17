+++
title = "machine learning"
author = ["Shuvrojit Biswas"]
lastmod = 2024-05-17T22:24:40+06:00
draft = false
+++

It's part of [Artificial Intelligent]({{< relref "20231215173255-artificial_intelligent.md" >}}). We're going to learn about tensor today.

Three things that happen in machine learning.

-   Initial - start training
-   Adjust - adjust the value according to the expected value
-   Repeat - repeat the process

Vector data is one dimensional array.
Scalar data is

ML systems learn how to combine input to produce useful predictions


## Context window {#context-window}

Imagine you're reading a story in a book, and you come across a word that you don't know the meaning of. To figure out what the word might mean, you don't just look at that one word in isolation; instead, you look at the words and sentences around it to get clues about its meaning. The words and sentences around the unfamiliar word provide context – they give you hints about what the word could be.


## How to run LLM models on your machine locally? {#how-to-run-llm-models-on-your-machine-locally}

The easiest way to run llm model on your machine is to use ollama. It's a software that automatically downloads and runs it on your local computer. It gives us a local server which we can use to interact with the model. We can simply use it like chatgpt using the terminal. When running it gives us a shell to write prompts. We can write any text and will get a text response in markdown format.

Go to ollama.com and install it for your respective operating system. Well the website has good documentation. After installing run the server using

```sh
ollama serve
```

It will expose an api at `localhost:11434`. When running this command in the command line we can also see other api entrypoint that are being exposed by the server. It will start with `/api/..` something like this. For instane to generate an prompt request we can hit `/api/generate/`. An example is given below:

```sh
curl http://localhost:11434/api/generate -d '{                                ✘ INT  11:46:02
  "model": "llama3",
  "prompt": "Why is the sky blue?"
}'
```

Now we can use the api if we want to. generate response using


## The easiest way to run llm locally on your own machine {#the-easiest-way-to-run-llm-locally-on-your-own-machine}

:EXPORT_FILE_NAME:  The-easiest-way-to-install-llm-locally
:EXPORT_DATE: 2024-05-17
:EXPORT_HUGO_MENU: :menu "main"
:EXPORT_CUSTOM_FRONT_MATTER: :draft false :cedric yes

Welcome! This guide will walk you through the easiest way to run Ollama on your local machine. Ollama is a powerful tool for running your ml models locally. It also creates a webserver for us to connect with. Normally when you’re trying to run a llm model such as llama3 from meta, you will need to go through a verification process in order to download the models from the official website. You can also get it from huggingface community. But with ollama we don’t need to do anything, just install it and download a model and run it.

Go to [ollama.com](<http://ollama.com>) and download the software according to your os spec. It will also available as a docker version. Recently they have released a windows preview.

[Ollama](<https://ollama.com/>)

First we need to start ollama by using the serve command. It will create a server for us to interact with. The server will be running on <http://localhost:11434/> . We can download the model using pull or we can download and run using run command. It will take some time to download.

```sh

ollama serve
# ollama pull model-name
ollama pull llama3

ollama run llama3
```

You can find all the models ollama has in their [library](<https://ollama.com/library>)

We can use curl to send a request through their api.

```sh
curl http://localhost:11434/api/generate -d '{                                       13:58:47
  "model": "llama3",
  "prompt": "What is a dyson sphere?"
}'

```

You can even do a lot of more. Read the API documentation in here: <https://github.com/ollama/ollama/blob/main/docs/api.md>

Running LLM (Large Language Models) like Ollama locally requires significant computational resources. This is due to the extensive processing power needed to handle the large number of parameters these models typically have. You will need a high-performance computer with a strong CPU and a considerable amount of RAM. Additionally, a powerful GPU is highly recommended for improved processing speed. Please remember that the exact requirements will depend on the specific model and the complexity of the tasks you plan to perform.

Although you can run it without gpu, it will be extremly slow and you will see some heavy cpu usage.
