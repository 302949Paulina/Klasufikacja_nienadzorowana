install.packages(c("raster", "terra", "ggplot2", "cluster", "viridis", "mclust"))
library(raster)
library(terra)
library(ggplot2)
library(cluster)
library(viridis)
library(mclust)
install.packages("tinytex")
library(tinytex)
# Ustal ścieżkę do folderu z plikami TIFF
image_folder <- "dane"

# Wczytaj wszystkie pliki TIFF z folderu
tiff_files <- list.files(image_folder, pattern = "\\.tiff", full.names = TRUE)

#Utwórz stos rasterów z plików TIFF
if(length(tiff_files) > 0) {
  image_stack <- stack(tiff_files)
} else {
  stop("No TIFF files found in the specified directory.")
}

# Wyświetlenie stosu rastrowego
plotRGB(image_stack, r = 3, g = 2, b = 1, stretch = "lin", main = "Stos rastrowy")

# Pobranie danych granic powiatów
url <- "https://www.gis-support.pl/downloads/2022/powiaty.zip"
download.file(url, "powiaty.zip", mode = "wb")
unzip("powiaty.zip", exdir = "powiaty_dir")

# Wczytanie danych granic powiatów
powiaty <- vect("powiaty_dir/powiaty.shp")

# Wybranie powiatu o kodzie JPT_KOD_JE 0617
powiat_swidnicki <- powiaty[grep("0617", powiaty$JPT_KOD_JE)]
print(powiat_swidnicki)



# Transformacja układu współrzędnych powiatu świdnickiego do układu współrzędnych danych rastrowych
powiat_swidnicki_transformed <- project(powiat_swidnicki, crs(image_stack))
print(powiat_swidnicki_transformed)
# Konwertuj powiat_swidnicki do formatu Spatial
powiat_swidnicki_sp <- as(powiat_swidnicki_transformed, "Spatial")


#Sprawdzenie  ukladu
raster_extent <- extent(image_stack)
vector_extent <- extent(powiat_swidnicki_sp)

# Przycinanie rastra do wybranego powiatu
cropped_image_stack <- crop(image_stack, powiat_swidnicki_sp)
raster_extend1 <-  extent(cropped_image_stack)
masked_image_stack <- mask(cropped_image_stack, powiat_swidnicki_sp)

# Wyświetlenie przyciętego rastra
plotRGB(masked_image_stack, r = 3, g = 2, b = 1, stretch = "lin", main = "Przycięty obraz powiatu świdnickiego")

# Konwertuj przycięty obraz na macierz
#image_matrix <- as.matrix(cropped_image_stack)

# Konwersja przyciętego stosu rastrowego do macierzy, uwzględniając tylko wartości nie-NA
image_matrix <- values(cropped_image_stack)
image_matrix <- na.omit(image_matrix)


# Wybór próby danych do k-means
set.seed(42)
sample_indices_kmeans <- sample(1:nrow(image_matrix))
sample_image_matrix_kmeans <- image_matrix[sample_indices_kmeans, ]

# K-means clustering na próbie danych
k_means_result <- kmeans(sample_image_matrix_kmeans, centers = 5, nstart = 25)

# Przypisanie wyników klasteryzacji do oryginalnych danych rastrowych
clustered_raster <- setValues(raster(cropped_image_stack), NA)
values(clustered_raster)[sample_indices_kmeans] <- k_means_result$cluster

# Wyświetlenie wyników klasteryzacji
plot(clustered_raster, col = viridis(5), main = "Wyniki klasteryzacji k-means")

# Wybór mniejszej próby danych do wskaźnika silhouette
set.seed(42)
sample_indices_silhouette <- sample(1:nrow(sample_image_matrix_kmeans), size = 10000)
sample_image_matrix_silhouette <- sample_image_matrix_kmeans[sample_indices_silhouette, ]
sample_clusters_silhouette <- k_means_result$cluster[sample_indices_silhouette]

# Walidacja wyników za pomocą wskaźnika silhouette - użycie mniejszej próby danych
silhouette_values_kmeans <- silhouette(sample_clusters_silhouette, dist(sample_image_matrix_silhouette))
# Wyświetlenie wskaźnika
mean(silhouette_values_kmeans[, 3])
#------------
# Gaussian Mixture Model (GMM) clustering na próbie danych
gmm_result <- Mclust(sample_image_matrix_kmeans, G = 5)

# Przypisanie wyników klasteryzacji GMM do oryginalnych danych rastrowych
clustered_raster_gmm <- setValues(raster(cropped_image_stack), NA)
values(clustered_raster_gmm)[sample_indices_kmeans] <- gmm_result$classification

# Wyświetlenie wyników klasteryzacji GMM
plot(clustered_raster_gmm, col = viridis(max(gmm_result$classification)), main = "Wyniki klasteryzacji GMM")

# Wybór mniejszej próby danych do wskaźnika silhouette dla GMM
set.seed(42)
sample_indices_silhouette_gmm <- sample(1:nrow(sample_image_matrix_kmeans), size = 1000)
sample_image_matrix_silhouette_gmm <- sample_image_matrix_kmeans[sample_indices_silhouette_gmm, ]
sample_clusters_silhouette_gmm <- gmm_result$classification[sample_indices_silhouette_gmm]

# Walidacja wyników GMM za pomocą wskaźnika silhouette - użycie próby danych
silhouette_values_gmm <- silhouette(sample_clusters_silhouette_gmm, dist(sample_image_matrix_silhouette_gmm))
mean(silhouette_values_gmm[, 3])


# Interpretacja klas przy użyciu wykresów pudełkowych
image_df <- as.data.frame(sample_image_matrix_kmeans)
image_df$cluster <- k_means_result$cluster

ggplot(image_df, aes(x = factor(cluster), y = X2024.05.13.00_00_2024.05.13.23_59_Sentinel.2_L1C_B01_.Raw.)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Klaster", y = "Wartości spektralne", title = "Wykres pudełkowy wartości spektralnych dla klastrów")

# Zapisz wyniki klasteryzacji do plików GeoTIFF
writeRaster(clustered_raster, filename = "wyniki/clustered_raster_kmeans.tif", format = "GTiff", overwrite = TRUE)
writeRaster(clustered_raster_gmm, filename = "wyniki/clustered_raster_gmm.tif", format = "GTiff", overwrite = TRUE)
