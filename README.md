**Relación entre pobreza y voto en las Elecciones Legislativas 2025**

Este repositorio presenta un análisis estadístico y territorial del desempeño electoral de La Libertad Avanza (LLA) y Fuerza Patria (FP) en las elecciones legislativas de 2025, utilizando como variable explicativa el nivel de pobreza departamental. El objetivo es identificar patrones provinciales, así como departamentos que se desvían significativamente del comportamiento esperado según su contexto socioeconómico.

**Contenido del análisis**

1.Base integrada con datos electorales departamentales (DINE) y datos de pobreza (Atlas Argentino de la Pobreza).

2.Análisis descriptivo nacional: mapas, boxplots y correlaciones entre pobreza y voto.

3.Estimación de modelos provinciales de regresión lineal pobreza→voto para LLA y FP.

4.Cálculo de residuos y residuos estandarizados para detectar sobrevoto y subvoto en cada provincia.

5.Validación cruzada Leave-One-Out (LOOCV) para evaluar la estabilidad de las pendientes provinciales.

6.Tablas de departamentos atípicos y evaluación de supuestos del modelo.

**Herramientas utilizadas**

Lenguaje: RStudio.
Fuentes de datos: Dirección Nacional Electoral (Elecciones 2025) y Atlas Argentino de la Pobreza (by Matias Iglesias). 
