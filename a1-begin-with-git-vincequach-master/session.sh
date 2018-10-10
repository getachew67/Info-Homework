#!/usr/bin/env bash
## session.sh

## Use this file to record the command-line commands you use in the assignment.
## Add each command BELOW the descriptive comment:


# Display the current working directory
pwd

# Make a new `img` folder inside your repo
mkdir img

# Change directory into the `img` folder and display its contents (2 commands)
cd ./img/
ls -a

# Copy the `.gitignore` file into the `img` folder without changing directories
cp ../.gitignore .

# Display the contents of the `img` folder to show it contains the hidden
# `.gitignore` filer
ls -a

# Add the `img` folder to git
git add .

# Commit your change to git (with a descriptive message)
git commit -m "Created img folder, copied .gitignore, and committed to git"
