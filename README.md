# Global Matching Model (Osth & Dennis, 2015)
This repository implements the Global Matching Model (GMM) from Osth & Dennis (2015) to explore the test position effect in recognition memory.

## File Descriptions
- `runme.R`: Runs model simulations. This is the main script to generate predictions. You can set parameter values and task design features here (e.g., list length, task type).
- `model.R`: Computes the means and variances of the old and new item distributions based on item- and context-level match/mismatch.
- `tpe.R`: Estimates changes in accuracy over the course of testing (i.e., the test position effect) for yes-no recognition and alternative forced-choice tasks.
- `afc.R`: Contains a function to compute accuracy for an Alternative Forced Choice (AFC) task using parameters from normal distributions. 

## Getting Started
To generate predictions, run the runme.R script after adjusting the parameters and task design as needed.

For theoretical background on the model, please refer to:
- Osth & Dennis (2015). Psychological Review.

Feel free to reach out with any questions: aytac.sinem@gmail.com
