# GitHub Package Setup Guide

## Step-by-Step Instructions

### 1. Prepare Your GitHub Repository

1. **Create a new repository on GitHub**:
   - Go to https://github.com/new
   - Repository name: `sppt.aggregated.data`
   - Description: "Spatial Pattern Point Test for Aggregated Data"
   - Choose "Public" (required for easy installation by others)
   - DO NOT initialize with README (we already have one)
   - Click "Create repository"

### 2. Upload Package Files to GitHub

**Option A: Using Git Command Line** (Recommended)

1. Extract the downloaded ZIP file to a folder on your computer
2. Open Terminal/Command Prompt and navigate to the folder:
   ```bash
   cd path/to/sppt.aggregated.data
   ```

3. Initialize Git and push to GitHub:
   ```bash
   git init
   git add .
   git commit -m "Initial commit - sppt.aggregated.data package"
   git branch -M main
   git remote add origin https://github.com/yourusername/sppt.aggregated.data.git
   git push -u origin main
   ```
   Replace `yourusername` with your actual GitHub username.

**Option B: Using GitHub Desktop** (Easier for beginners)

1. Download and install GitHub Desktop: https://desktop.github.com/
2. Open GitHub Desktop
3. Click "File" → "Add Local Repository"
4. Browse to your extracted package folder
5. Click "Publish repository" 
6. Uncheck "Keep this code private" if you want it public
7. Click "Publish Repository"

**Option C: Using GitHub Web Interface**

1. On your new repository page, click "uploading an existing file"
2. Drag and drop ALL files from the extracted folder
3. Add commit message: "Initial commit"
4. Click "Commit changes"

### 3. Update Package Information

Before publishing, edit these files with your information:

**DESCRIPTION file**:
- Replace `"Your Name"` and email with your information
- Replace `yourusername` in the URL with your GitHub username

**README.md file**:
- Replace `yourusername` in installation instructions with your GitHub username
- Add your citation information
- Update contact information

**LICENSE file**:
- Replace `[Your Name]` with your name

### 4. Test Package Installation

After uploading to GitHub, test that others can install it:

```r
# In R or RStudio
install.packages("devtools")
devtools::install_github("yourusername/sppt.aggregated.data")

# Test that it works
library(sppt.aggregated.data)
?sppt
```

### 5. Create a Release (Optional but Recommended)

1. On your GitHub repository page, click "Releases" → "Create a new release"
2. Tag version: `v0.1.0`
3. Release title: `Initial Release v0.1.0`
4. Description: Copy content from NEWS.md
5. Click "Publish release"

### 6. Add a Package Badge (Optional)

Add this to the top of your README.md for a professional look:

```markdown
[![R-CMD-check](https://github.com/yourusername/sppt.aggregated.data/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yourusername/sppt.aggregated.data/actions/workflows/R-CMD-check.yaml)
```

## Common Issues and Solutions

### Issue: "Could not find package"
- Make sure repository is public
- Check that you're using the correct username
- Verify all files were uploaded correctly

### Issue: Package won't install
- Run `devtools::check()` locally first to find errors
- Make sure all dependencies are listed in DESCRIPTION
- Check that NAMESPACE file exists

### Issue: Functions not exported
- Make sure `#' @export` is above the function
- Regenerate documentation with `devtools::document()`

## Updating Your Package

After making changes:

```bash
# Update version in DESCRIPTION file (e.g., 0.1.0 → 0.1.1)
# Update NEWS.md with changes
# Commit and push changes
git add .
git commit -m "Description of changes"
git push
```

Users can then update with:
```r
devtools::install_github("yourusername/sppt.aggregated.data")
```

## Getting Help

- R Packages book: https://r-pkgs.org/
- devtools documentation: https://devtools.r-lib.org/
- GitHub Help: https://docs.github.com/

## Package Structure

```
sppt.aggregated.data/
├── DESCRIPTION          # Package metadata
├── NAMESPACE           # Exported functions
├── LICENSE            # License file
├── README.md          # Package documentation
├── NEWS.md            # Version history
├── .Rbuildignore      # Files to ignore when building
├── .gitignore         # Files to ignore in Git
├── R/
│   └── sppt.R         # Main function code
├── man/               # Documentation (auto-generated)
│   └── sppt.Rd
└── data-raw/          # Scripts to create data (if needed)
```

## Next Steps

1. Generate documentation: `devtools::document()`
2. Check package: `devtools::check()`
3. Install locally: `devtools::install()`
4. Test thoroughly before sharing
5. Consider adding vignettes for detailed examples
6. Add unit tests for reliability

## Sharing Your Package

Once published, share with others:

```r
# Installation instructions
install.packages("devtools")
devtools::install_github("yourusername/sppt.aggregated.data")

# Load and use
library(sppt.aggregated.data)
```

Good luck with your package! 🎉
