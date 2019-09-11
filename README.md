# Distributacalcul_Package
Package des fonctions pour l'application [distributacalcul](https://alec42.shinyapps.io/distributacalcul/)


Fonctions incluses marquées par un x.
Fonctions incluses dans le package [TVaR package](https://github.com/gabrielcrepeault/tvarPackage) marquées par y.

Distributions continues:

| Fonction                         	| Erlang 	| Inverse Gaussienne 	| Weibull 	| Burr 	| Log-logistique 	| Beta 	| Gamma 	| Pareto 	| F-Généralisée 	| lognormale 	| Exponentielle 	| Uniforme| Normale | 
|----------------------------------	|:------:	|:------------------:	|:-------:	|:----:	|:--------------:	|:----:	|:-----:	|:------:	|:-------------:	|:----------:	|:------------:	  |:------:	| :------:|
| Espérance            	            |    x   	|          x         	|    x    	|   y  	|                	|   y  	|   y   	|    y   	|               	|      y     	|       y    	    |    y   	|       	|
| K-ème moment           	          |    x   	|                    	|    x    	|   x  	|        x       	|   x  	|   x   	|    x   	|               	|      x     	|           	    |       	|      	  |
| Espérance tronquéee       	      |    x   	|          x         	|    x    	|   x  	|        x       	|      	|       	|        	|               	|            	|            	    |        	|       	|
| Espérance limitée                	|    x   	|          x         	|    x    	|   x  	|        x       	|      	|       	|        	|               	|            	|            	    |        	|       	|
| Variance                         	|    x   	|          x         	|    x    	|   y  	|        x       	|   y  	|   y   	|    y   	|               	|      y     	|       y    	    |    y   	|       	|
| Stop-loss                        	|    x   	|          x         	|    x    	|   x  	|        x       	|   y  	|   y   	|    y   	|               	|      y     	|       y    	    |    y   	|   y   	|
| Excès-moyen                      	|    x   	|          x         	|    x    	|   x  	|        x       	|   y  	|   y   	|    y   	|               	|      y     	|            	    |    y   	|   y   	|
| Fonction Génératrice des Moments 	|        	|          x         	|         	|      	|                	|      	|       	|        	|               	|            	|            	    |        	|       	|
| Densité                          	|    x   	|          x         	|         	|      	|                	|      	|       	|        	|               	|            	|            	    |        	|       	|
| Répartition                      	|    x   	|          x         	|         	|      	|                	|      	|       	|        	|       x       	|            	|            	    |        	|       	|
| TVaR                             	|    x   	|          x         	|    x    	|   x  	|        x       	|   y  	|   y   	|    y   	|               	|      y     	|       y    	    |        	|   y   	|
| VaR (quantile)                   	|        	|          x         	|    x    	|   x  	|        x       	|   y  	|   y   	|    y   	|       x       	|      y     	|       y    	    |    y   	|   y   	|

Distributions discrètes:

|             Fonction             	| Binomiale 	| Bernoulli 	| Binomiale Négative 	| Géométrique 	| Poisson 	| Uniforme 	| Logarithmique 	| Hypergéométrique 	|
|:--------------------------------:	|:---------:	|:---------:	|:------------------:	|:-----------:	|:-------:	|:--------:	|---------------	|------------------	|
|             Espérance            	|      y     	|           	|          y         	|             	|         	|     x    	| x             	| x                	|
|           K-ème moment           	|           	|           	|                    	|             	|         	|          	|               	|                  	|
|        Espérance tronquéee       	|           	|           	|                    	|             	|         	|          	|               	|                  	|
|         Espérance limitée        	|           	|           	|                    	|             	|         	|          	|               	|                  	|
|             Variance             	|      y    	|           	|          y         	|             	|         	|     x    	| x             	| x                	|
|             Stop-loss            	|           	|           	|                    	|             	|         	|          	|               	|                  	|
|            Excès-moyen           	|           	|           	|                    	|             	|         	|          	|               	|                  	|
| Fonction Génératrice des Moments 	|           	|           	|                    	|             	|         	|          	|               	|                  	|
|              Densité             	|           	|           	|                    	|             	|         	|     x    	|               	|                  	|
|            Répartition           	|           	|           	|                    	|             	|         	|     x    	|               	|                  	|
|               TVaR               	|      y    	|           	|                    	|             	|    y    	|          	|               	|                  	|
|          VaR (quantile)          	|      y    	|           	|                    	|             	|         	|          	|               	|                  	|

Distributions composées:

| Fonction                         	| Binomiale Négative Composée 	| Binomiale Composée 	| Poisson Composée 	|
|----------------------------------	|:---------------------------:	|:------------------:	|:----------------:	|
|             Espérance            	|              x              	|          x         	|         x        	|
|           K-ème moment           	|                             	|                    	|                  	|
|        Espérance tronquéee       	|                             	|                    	|                  	|
| Espérance limitée                	|                             	|                    	|                  	|
| Variance                         	|              x              	|          x         	|         x        	|
| Stop-loss                        	|                             	|                    	|                  	|
| Excès-moyen                      	|                             	|                    	|                  	|
| Fonction Génératrice des Moments 	|                             	|                    	|                  	|
| Densité                          	|                             	|                    	|                  	|
| Répartition                      	|              x              	|          x         	|         x        	|
| TVaR                             	|              x              	|          x         	|         x        	|
| VaR (quantile)                   	|              x              	|          x         	|         x        	|
