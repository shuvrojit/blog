+++
title = "Node Typescript Starter Template"
author = ["Shuvrojit Biswas"]
date = 2023-04-20
lastmod = 2023-04-20T23:46:42+06:00
tags = ["node", "typescript", "eslint"]
draft = false
weight = 1002
+++

Today we're going to create a node typescript setup or starter project.
let's make a directory with `mkdir`. You can name it whatever you want.
We're going to initialize git into this directory. Git is version control software which manages the working state of our application.

```shell
git init
```

We need to initialize a npm project directory. We can do that with `npm init -y` . `-y` means yes to all the questions that npm init is going to ask you. It's going to ask you a bunch of questions related to the project.

```shell
npm init -y
```

We can see the package.json result in the terminal.

We're going to add all development packages that are needed one by one and learn along the way which packages does exactly what.


## Typescript {#typescript}

Let's setup typescript right away. Whenever we are working with typescript, always make sure to check that if the needed package has built-in types definition. If it does not then head to [Definitely Typed](https://github.com/DefinitelyTyped/DefinitelyTyped) and look for the type definitions for that package.

```shell
npm install --save-dev typescript @types/node
```

or you can do shortcut like this

```shell
npm i -D typescript @types/node
```

Since we're using typescript, we're gonna have to create a typescript config file. We can either use `npx tsc --init` or manually create `tsconfig.json` file ourselves. Whenever you're tsc init you're gonna see complex looking tsconfig file. We don't need to keep all things in the file. Most of them are just comments discribing what particular settings do.

```json
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "lib": ["es6"],
    "allowJs": true,
    "outDir": "build",
    "rootDir": "src",
    "strict": true,
    "noImplicitAny": true,
    "esModuleInterop": true,
    "resolveJsonModule": true
  }
}
```

Just to recap what's happening here. This file is a json file. We've a bunch of compileroptions rules. The `target` is for which javascript version the source code will compiler into. The typescript documentation has a lot of information regarding this.

Whenvever working with Javascript or Typescript application, it's good practice to always use src folder as your main code folder. Let's create our source directory.

```shell
mkdir src
```

Let's create a new ts file called index.ts. We're going to write some code to see if it's working or not.

```shell
touch src/index.ts
```

```typescript
console.log("hello")
```

Let's run the file that we've created.

```shell
npx tsc src/index.ts
```

Well, nothing showed up. Beacause we only compiled the typescript file. You can see that there's a directory called build. Where did it come from? `build` directory is defined in tsconfig as `outdir`. `tsc` command just compile's the typescript file. We can see in the build folder that there's a file called index.js. Typescript still compile's to JS. Because node and the browser can't understand typescript. It only understand's javascript. On the other hand, deno has native support for typescript.

Let's install ts-node which will help node run typescript.

```shell
npm i -D ts-node
```

Let's rerun the program using `ts-node`

```shell
npx ts-node src/index.ts
```

And we get our expected result hello world. And we're going to use nodemon to restart our server automatically whenever the files gets edited.
Install nodemon with npm and save it as devdependencies.

```shell
npm i -D nodemon
```

Let's take a look  into our package.json file. We can see that all our packages are there and there's a lot of options. We're going to work in the scripts section in this file. There's already a script named test in there as sample. Before we've been using npx to use tools that are installed in this project. Now we're going to use them using npm run syntax.

The first script will be a dev script which runs our application in development environment. We don't need the test one right now. So you can delete that line.

```json { hl_lines=["7"] }
{
  "name": "starter",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "dev": "nodemon src/index.ts",
  },
  "keywords": [],
  "author": "",
  "license": "ISC",
  "devDependencies": {
    "@types/node": "^18.15.11",
    "nodemon": "^2.0.22",
    "ts-node": "^10.9.1",
    "typescript": "^5.0.4"
  }
}
```

```shell
npm run dev
```

Let's run our app. If you get an error, it's because of the comma at the end of our properties. In JSON the last property from any group doesn't have a comma. Let's remove the comma from there.

```json { hl_lines=["7"] }
{
  "name": "starter",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "dev": "nodemon src/index.ts"
  },
  "keywords": [],
  "author": "",
  "license": "ISC",
  "devDependencies": {
    "@types/node": "^18.15.11",
    "nodemon": "^2.0.22",
    "ts-node": "^10.9.1",
    "typescript": "^5.0.4"
  }
}
```

```shell
npm run dev
```

And run again. We can see the message hello on the terminal and it doens't stop running. It's waiting for file changes before restarting the server again automatically.


## Formatting &amp; Linting. {#formatting-and-linting-dot}

When we're writing code we don't want to get distracted by manual indenting or  making our code look pretty and nice. It should be done automatically without needing to get our attention. `prettier` is a very good formatting tool for javascript and typescript ecosystem. I am using emacs as my code editor. I have set it to use prettier to format the file as soon as I hit the save command keys. VScode is also very good at this. It has options for format on save. You need to search it yourself.

We're going to add a script command to format our code when we run that command. Let's install prettier with:

```shell
npm i -D prettier
```

Create a `.prettierrc` file in project directory and add `{}` in there. It will tell pretttier to use the default config files. Default config is pretty much ok. You can customize this further by diving into prettier documentation.

Edit the package.json file as follows.

```json { hl_lines=["8"] }
{
  "name": "starter",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "dev": "nodemon src/index.ts",
    "format": "prettier --write \"src/**/*.{ts,tsx}\""
  },
  "keywords": [],
  "author": "",
  "license": "ISC",
  "devDependencies": {
    "@types/node": "^18.15.11",
    "nodemon": "^2.0.22",
    "ts-node": "^10.9.1",
    "typescript": "^5.0.4"
  }
}
```

Linting helps us use good coding styles and practices. It has a lot of customization options. Let's install it.

```shell
npm install --save-dev @typescript-eslint/parser @typescript-eslint/eslint-plugin eslint
```

Create a new file called `.eslintrc.json`. This file will have all our rules. We need to install `eslint-config-prettier` which integrates prettier with eslint.

```shell
npm i -D eslint-config-prettier
```

```json
{
  "root": true,
  "parser": "@typescript-eslint/parser",
  "plugins": ["@typescript-eslint"],
  "extends": [
    "eslint:recommended",
    "plugin:@typescript-eslint/eslint-recommended",
    "plugin:@typescript-eslint/recommended",
    "prettier"
  ],
  "rules": {
   "no-console": 1
  }
}
```

About the rules section. There are a lot of rules for eslint. You can find them in the docs.

```json
    "lint": "eslint . --ext .ts"
```

Add this line after the format command.

```json { hl_lines=["9"] }
{
  "name": "starter",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "dev": "nodemon src/index.ts",
    "format": "prettier --write \"src/**/*.{ts,tsx}\"",
    "lint": "eslint . --ext .ts"
  },
  "keywords": [],
  "author": "",
  "license": "ISC",
  "devDependencies": {
    "@types/node": "^18.15.11",
    "nodemon": "^2.0.22",
    "ts-node": "^10.9.1",
    "typescript": "^5.0.4"
  }
}
```
