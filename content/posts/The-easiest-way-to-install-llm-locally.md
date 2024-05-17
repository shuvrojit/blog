+++
title = "The easiest way to run llm locally on your own machine"
author = ["Shuvrojit Biswas"]
date = 2024-05-17
lastmod = 2024-05-17T22:37:02+06:00
tags = ["machine-learning", "artificial-intelligence"]
categories = ["AI"]
draft = false
weight = 1001
[menu]
  [menu.main]
    weight = 1001
    identifier = "the-easiest-way-to-run-llm-locally-on-your-own-machine"
+++

Welcome! This guide will walk you through the easiest way to run Ollama on your local machine. Ollama is a powerful tool for running your ml models locally. It also creates a webserver for us to connect with. Normally when you’re trying to run a llm model such as llama3 from meta, you will need to go through a verification process in order to download the models from the official website. You can also get it from huggingface community. But with ollama we don’t need to do anything, just install it and download a model and run it.

Go to [ollama.com](<http://ollama.com>) and download the software according to your os spec. It will also available as a docker version. Recently they have released a windows preview.

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
