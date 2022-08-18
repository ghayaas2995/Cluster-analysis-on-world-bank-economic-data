# Cluster-analysis-on-world-bank-economic-data
The purpose of this project is to cluster the countries and detect groups, characteristics to support World Bank to assess the countries for decision making  

The World Bank Group works in every major area of development. They provide a wide array of financial products and technical assistance to  help countries to eradicate poverty and increase life’s quality. 

We performed K medoid (PAM), K means, and agglomerative hierarchical clustering with single, complete, average and ward.D2 linkage methods to choose the most relevant clustering technique and assess the quality and reliability of clustering results for our dataset. The goodness of clustering is assessed using Silhouette score and Calinski-Harabasz score.  The key parameters of each algorithm applied, and their respective results are discussed below.

1.	PAM (K Medoids):
The PAM algorithm was implemented using the Euclidian distance dissimilarity matrix performed on the scaled dataset. The algorithm was iterated for 5 trails and for K values between 2 to 10. A maximum silhouette score of 0.3675 and CH score of 105.22 was observed with optimal K = 2 for our dataset. 

2.	K means:
Next, we implemented the K means algorithm on the scaled dataset and evaluated the clusters formed using the SW and CH score metrics for k varying between 2 and 10. With this method, a maximum SW score of 0.3652 and CH score of 106.86 for 2 optimal number of clusters was observed.

3.	Agglomerative Hierarchical Clustering:
The hierarchical clustering was performed using the hclust() function from the “cluster” package in R on the Euclidean distance dissimilarity matrix obtained from the scaled dataset. The algorithm was iterated for all 4 linkage methods. We obtained a maximum Agglomerative coefficient for 'hclust' object of 0.9526 for the “Ward.D2” linkage, followed by complete, average, and single linkages with their coefficients being 0.8496, 0.6895, and 0.4988 respectively.  The algorithm was also iterated for K values between 2 to 10 to determine the optimal number of groups in the dataset. “2” was found to be the optimal number with a maximum silhouette score of 0.3694 for the Ward.D2 linkage.

Interpretation of clusters:

Cluster 1 has 45 countries and cluster 2 has 106 countries. Some countries belonging to cluster 1 are Afghanistan, Ethiopia, Burki Faso, South Africa, South Sudan, Central African Republic, Nigeria, Rwanda, Yemen, Uganda etc.  and that belonging to cluster 2 are Russia, Australia, Oman, New Zealand, Denmark, Greece, Israel etc. There is a clear distinction that the cluster 1 countries are underdeveloped or developing countries and that in the cluster 2 are developed countries.

