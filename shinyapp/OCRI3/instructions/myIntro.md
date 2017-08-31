---
output: html_document
---


**The SOCRISP app allows users to upload and analyze exfoliative cytology data to provide an index for developing an oral cancer risk.**

- *Explore* the app's features with the example data set pre-loaded by clicking on the tabs above.
- *Upload* your data in the "Input Data" tab.

#### <a name="introductions"></a> OCRI-I -- EdTAR

Initial effor was focused on developing a statistic model for early diagnosis of oral squamous cell carcinoma (OSCC). The paper [Liu, Y, et al. (2015) "Quantitative risk stratification of oral leukoplakia with exfoliative cytology." PloSOne.     doi: 10.1371/journal.pone.0126760.](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0126760) described a procedure involving a step called expert-guided data transformation and reconstruction (EdTAR) which allows automatic data processing and reconstruction and reveals informative signals for subsequent risk stratification. Modern machine learning techniques were utilized to build statistical prediction models on the reconstructed data. The reported method achieved a high sensitivity (median>0.98) and specificity (median>0.99). One OLK patient with an initial OCRI of 0.88 developed OSCC after 40 months of follow-up. Our approach could potentially improve cost-effectiveness of clinical follow-up of OLK patients, and help design clinical chemoprevention trial for high-risk populations.

The method starts from a collection of exfoliated cytometry data with the following distribution:

<img src="ocri-I-intro.png" alt="PCA Plot" style="width: 80%"/>
#### <a name="features"></a> OCRI/EdTAR Features

The EdTAR procedure consists of three main steps:

- *Data reconctruction* parse DNA index data and recontruct/reengineer new variables.
- *Statistical model building* build and train model on historical data.
- *Risk index prediction* predict on a new data.

<img src="ocri-I-data-reconstruction.png" alt="Data Reconstruction" style="width: 30%"/>
<img src="ocri-I-model-training.png" alt="Model Training" style="width: 30%"/>
<img src="ocri-I-data-prediction.png" alt="Prediction" style="width: 30%"/>


#### <a name="OCRI-II"></a> OCRI-II features

Yicheng,please fill out the OCRI-II parts

- Must be a .CSV *comma-separated-value* file (you may export from Excel).
- File must have a header row.
- etc.


#### <a name="OCRI-II-more"></a> More on OCRI-II

**Count or Expression Data**
- Each row denotes a gene, each column denotes a sample.



#### App Info

The START app has been developed by Yicheng Li, Jianying Li, and Luke Chen
North Carolina Central University Cancer Research Program.


