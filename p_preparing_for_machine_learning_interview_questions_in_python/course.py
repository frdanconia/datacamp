#%%
The hunt for missing values

Questions about processing missing values are integral to any machine learning interview. If you are provided with a dataset with missing values, not addressing them will likely skew your results and lower your model's accuracy.

In this exercise, you'll practice the first pre-processing step by finding and exploring ways to handle missing values using pandas and numpy on a customer loan dataset.

The dataset, which you'll use for many of the exercises in this course, is saved to your workspace as loan_data.


#%%
# Import modules
import numpy as np
import pandas as pd

loan_data = pd.read_csv("loan_missing_filled.csv")

# Print missing values
print(loan_data.isna().sum())

#%%
Iterative imputation

In the previous exercise, you derived mean imputations for missing values of loan_data. However, in a machine learning interview, you will probably be asked about more dynamic imputation techniques that rely on other features in the dataset.

In this exercise, you'll practice a machine-learning based approach for imputing missing values by imputing missing values as a function of remaining features using IterativeImputer() from sklearn.impute.

Note that this function is considered experimental, so please read the documentation for more information.

#%%
# Import imputer module
from sklearn.impute import SimpleImputer

# Subset numeric features: numeric_cols
numeric_cols = loan_data.select_dtypes(include=[np.number])
#%%
Simple imputation

As you saw in the last exercise, deleting data can reduce your dataset by too much. In an interview context, this can lead to biased results of your machine learning model.

A more dynamic way of handling missing values is by imputing them. There are a variety of ways of doing this in python, but in this exercise you will be using the SimpleImputer() function from the sklearn.impute module on loan_data.

You will then use pandas and numpy to convert the imputed dataset into a DataFrame. 

#%%
# Import imputer module
from sklearn.impute import SimpleImputer

# Subset numeric features: numeric_cols
numeric_cols = loan_data.select_dtypes(include=[np.number])

#%%
Iterative imputation

In the previous exercise, you derived mean imputations for missing values of loan_data. However, in a machine learning interview, you will probably be asked about more dynamic imputation techniques that rely on other features in the dataset.

In this exercise, you'll practice a machine-learning based approach for imputing missing values by imputing missing values as a function of remaining features using IterativeImputer() from sklearn.impute.

Note that this function is considered experimental, so please read the documentation for more information.

#%%
# Explicitly require this experimental feature
from sklearn.experimental import enable_iterative_imputer
# Now you can import normally from sklearn.impute
from sklearn.impute import IterativeImputer

# Subset numeric features: numeric_cols
numeric_cols = loan_data.select_dtypes(include=[np.number])

# Subset numeric features: numeric_cols
numeric_cols = loan_data.select_dtypes(include=[np.number])

# Iteratively impute
imp_iter = IterativeImputer(max_iter=5, sample_posterior=True, random_state=123)
loans_imp_iter = imp_iter.fit_transform(numeric_cols)

#%%
