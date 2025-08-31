# Create a README.md file with the project title
echo "# OxygenDelivery_country" >> README.md

# Initialize the local repository
git init

# Add the README file to staging
git add README.md

# Commit the file
git commit -m "first commit"

# Rename the default branch to main
git branch -M main

# Link your local repo to GitHub (replace with your repo URL if different)
git remote add origin https://github.com/kasimallel/OxygenDelivery_country.git

# Push changes to GitHub
git push -u origin main
