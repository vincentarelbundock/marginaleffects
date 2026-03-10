## What is this?

The [marginaleffects.com](https://marginaleffects.com) website hosts the code and documentation for the open source marginaleffects package for R and Python, and a complete free version [of this book:](https://routledge.com/9781032908724)

> Model to Meaning: How to interpret statistical models in R and Python. Arel-Bundock, Vincent. 2026. CRC Press.
Model to Meaning How to Interpret Statistical Models with R and Python. This software empowers R and Python users to translate the outputs of statistical and machine learning models into accurate insights that are accessible to a wide audience.

This repository includes a "skill" that allows large language models to discuss and explain every chapter in the book, and every function in the package.

## Install `marginaleffects-SKILL`

Many coding agents use the [Agent Skills coding standard](https://agentskills.io/). We can thus use this `marginaleffects` skill with [Claude Code](https://docs.anthropic.com/en/docs/claude-code), [Codex](https://platform.openai.com/docs/codex), [OpenCode](https://opencode.ai/), and [pi.dev](https://pi.dev/). 

The only trick is that these tools look in different directories for the skill. We thus clone the repository where we want to store it. Then, we symlink the skill to an appropriate directory for each tool.

### 1) Clone the repo

```bash
# Run this in the directory where you want to save the skill
# Example: cd ~/repos
git clone git@github.com:vincentarelbundock/marginaleffects-SKILL.git
```

### 2) Create symlinks

```bash
# Codex
# Run this from the same directory where you cloned the repo (e.g., ~/repos)
SKILL_DIR="$(pwd)/marginaleffects-SKILL"

mkdir -p ~/.codex/skills
ln -sfn "$SKILL_DIR" ~/.codex/skills/marginaleffects-SKILL

# pi.dev
mkdir -p ~/.pi/agent/skills/pi-skills
ln -sfn "$SKILL_DIR" ~/.pi/agent/skills/pi-skills/marginaleffects-SKILL

# Claude
mkdir -p ~/.claude/skills
ln -sfn "$SKILL_DIR" ~/.claude/skills/marginaleffects-SKILL

# Open Code (example local skills folder)
mkdir -p ~/.opencode/skills
ln -sfn "$SKILL_DIR" ~/.opencode/skills/marginaleffects-SKILL
```

### 3) Test the skill

```bash
claude
/marginaleffects
```
