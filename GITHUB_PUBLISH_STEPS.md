# Publish to GitHub (s-rani1/optbinningR)

Run from:
`/Users/sudheerrani/Documents/Sahaj_coding_codex/optbinningR`

## 1) Initialize git (if not initialized yet)
```bash
git init
git add .
git commit -m "Initial optbinningR release prep"
```

## 2) Set default branch
```bash
git branch -M main
```

## 3) Connect to GitHub repo
```bash
git remote add origin git@github.com:s-rani1/optbinningR.git
# If origin already exists:
# git remote set-url origin git@github.com:s-rani1/optbinningR.git
```

## 4) Push
```bash
git push -u origin main
```

## 5) Create release tag (optional, recommended)
```bash
git tag v0.2.0
git push origin v0.2.0
```

## 6) Attach source tarball to GitHub release (optional)
Tarball path:
`/Users/sudheerrani/Documents/Sahaj_coding_codex/optbinningR/optbinningR_0.2.0.tar.gz`
