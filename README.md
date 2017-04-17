<h2>Spatialtemporal Interval Logic Formalism - STILF</h2>

This package perfoms event extraction from a set of time series classified. The events extracted consist of a sequence of events that answer a specific question, such as <i>Which events of "Forest" areas were replaced by "Pasture"?</i>

With package "stilf is possible to build questions using thirteen Allen's interval temporal logic relationships and also others extended from their study. I suggest the reader read <a href="http://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf"> (Allen 1983)</a> and <a href="http://www.cs.ucf.edu/~lboloni/Teaching/EEL6938_2007/papers/Allen-GeneralTheoryActionTime.pdf"> (Allen 1984) </a> for more details. Besides, is possible to generate graphics with event information and plot maps with results. Using these events the user can to perform analysis on time series data discover land use and land cover changes.

wtss is an R client package for handling Web Time-Series Service (WTSS) in the client side at <a href="https://github.com/e-sensing/wtss.R">https://github.com/e-sensing/wtss</a>

Classification method can be found using package dtwSat at <a href="https://github.com/vwmaus/dtwSat">https://github.com/vwmaus/dtwSat</a>

Tools to Satellite Image Time Series analysis can be found using package sits at <a href="https://github.com/gilbertocamara/sits">https://github.com/gilbertocamara/sits</a>


<h3>Prerequisites: </h3> 
<ul>
  <li><a href="http://git-scm.com/">Git</a></li>
  <li><a href="http://www.r-project.org/">R</a></li>
  <li><a href="http://www.rstudio.com/">Rstudio</a></li>
  <li>A set of time series classified data </li>
  <li>The stilf requires "devtools" package is available. </li> 
</ul>

<h3>How to use the package:</h3>
<ul>
  <li>Open RStudio</li>
  <li>Install devtools <code>install.packages("devtools")</code> </li>
  <li>Load devtools <code>library(devtools)</code> </li>
  <li>Install the stilf package <code>install_github("ammaciel/stilf")</code> </li>
</ul>

<br />
<h3>Example 1</h3>

 - Load the stilf package <code>library(stilf)</code>

 - Load a example data <code>data("example_data_TWDTW")</code>

 - Create new variable tibble format and apply stilf_standard_date_events to standardize start_date and end_date columns
<pre class="R">
 # alter start_date and end_date to a especific range in order to extract events
example_1.tb <- stilf_data %>% 
  stilf_standard_date_events(data_tb = stilf_data, month_year = "09", day_month = "01")
  example_1.tb
</pre>

 - Plot example_1.tb <code>stilf_plot_maps_input(example_1.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))</code>

<table width="700" border="0">
<tr>
<td align="center" valign="center">
<img src="inst/figures/example1.1.png" alt="Fig. 1. Plot time series classified data" />
<p class="caption">
Fig. 1. Plot time series classified data
</p>
</td>
</tr>
</table>

 - Apply stilf_predicate_occur function to discover events of <i>Pasture</i>. For this is necessary create a for loop to read all data and not only one pixel over time 

<pre class="R">
# p = properties of objects :
p1 <- "Pasture"

# t = interval:
t1 <- stilf_interval("2000-09-01","2017-03-01")

# Test occur for many time series 
QuestionOccurs <- function(data_tb, p, t){
  
  tb <- data_tb 
  coord <- unique(tb$index)
  output.tb <- tb[FALSE,]
  
  for(x in 1:length(coord)){
    #x=1
    temp <- tb[which(as.character(tb$index) == coord[x]),]
    
    if (nrow(event2 <- stilf_predicate_occur(temp, p1, t1)) >= 1
        
    ){
      temp0 <- rbind(event2)
    } else {
      temp0 <- NULL
    }
    output.tb <- dplyr::bind_rows(output.tb,temp0)
  }
  return(output.tb)
}

output.tb <- QuestionOccurs(example_1.tb, p = p1, t = t1)
output.tb

remove(t1,p1)
</pre>

 - See example_1.tb plot with stilf_plot_maps_input function <code>stilf_plot_maps_input(example_1.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))</code>

 - See all events with stilf_plot_maps_events function <code>stilf_plot_maps_events(output.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), shape_point = 0, colour_point = "black", size_point = 2.3)</code>

<table width="700" border="0">
<tr>
<td align="center" valign="center">
<img src="inst/figures/example1.2.png" alt="Fig. 2. Plot events discovered from time series classified data" />
<p class="caption">
Fig. 2. Plot events discovered from time series classified data
</p>
</td>
</tr>
</table>

- See barplot total area in square kilometers <code>stilf_plot_barplot_events(output.tb, custom_palette = TRUE, RGB_color = "#929e6e", pixel_resolution = 250)</code> and sequence plot <code>stilf_plot_sequence_events(output.tb, show_y_index = FALSE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = "#929e6e")</code>

<table width="700" border="0" cellspacing="0" cellpadding="0">
<tr>
<td align="center" valign="center">
<img src="inst/figures/example1.3.png" alt="Fig. 3.(a) Barplot with total area" />
<br />
Fig. 3.(a) Barplot with total area
</td>

<td align="center" valign="center">
<img src="inst/figures/example1.4.png" alt="Fig. 3.(b) Sequence plot" />
<br />
Fig. 3.(b) Sequence plot
</td>

</tr>
</table>

<br />
<h3>Example 2</h3>

- Apply stilf_predicate_occur function to discover events for only one pixel with events of <i>Forest and Pasture</i>. 

<pre class="R">
data("example_data_TWDTW")
stilf_data

# select only one time serie with index equals 13
# alter start_date and end_date to a especific range in order to extract events
example_2.tb <- stilf_data %>% 
  dplyr::filter(., .$index == 13) %>% 
  stilf_standard_date_events(data_tb = ., month_year = "09", day_month = "01")

example_2.tb

# p = properties of objects :
p1 <- "Forest"
p2 <- "Pasture"

# t = interval:
t1 <- stilf_interval("2000-09-01","2004-09-01")
t2 <- stilf_interval("2004-09-01","2017-09-01")

# Test occur for one time serie
QuestionOccurs <- function(data_tb, p, t){
 
  output.tb <- data_tb[FALSE,]
  data_tb
  
  if (nrow(ev1 <- stilf_predicate_occur(data_tb, p1, t1)) >= 1 &
      nrow(ev2 <- stilf_predicate_occur(data_tb, p2, t2)) >= 1 &
      
      isTRUE(stilf_relation_meets(tail(stilf_interval(ev1$start_date, ev1$end_date), 1),
                                  head(stilf_interval(ev2$start_date, ev2$end_date),1)))
  ){
    temp0 <- rbind(ev1,ev2)
  } else {
    temp0 <- NULL
  }
  output.tb <- dplyr::bind_rows(output.tb,temp0)

  return(output.tb)
}

output.tb2 <- QuestionOccurs(example_2.tb, p = c(p1, p2), t = c(t1,t2))
output.tb2

remove(p1, p2, t1, t2)
</pre>


 - View example_2.tb plot with stilf_plot_maps_input function <code>stilf_plot_maps_input(example_2.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e"))</code>

 - View all events with stilf_plot_maps_events function <code>stilf_plot_maps_events(output.tb2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e"), shape_point = 4, colour_point = "blue", size_point = 8)</code>

<table width="700" border="0" cellspacing="0" cellpadding="0">
<tr>
<td align="center" valign="center">
<img src="inst/figures/example2.1.png" alt="Fig. 4.(a) Pixel over time" />
<br />
Fig. 4.(a) Pixel over time
</td>

<td align="center" valign="center">
<img src="inst/figures/example2.2.png" alt="Fig. 4.(b) Pixel with events" />
<br />
Fig. 4.(b) Pixel with events
</td>

</tr>
</table>

<br />
<h3>Example 3</h3>

 - Apply stilf_predicate_occur function to discover for a sequence of events of <i>Forest, Pasture, Single cropping and Double cropping</i> in this order. 

<pre class="R">
data("example_data_TWDTW")
stilf_data

example_3.tb <- stilf_data %>% 
  stilf_standard_date_events(data_tb = ., month_year = "09", day_month = "01")

example_3.tb

# p = properties of objects :
p1 <- c("Forest", "Pasture", "Single_cropping", "Double_cropping")

# t = interval:
t1 <- stilf_interval("2000-08-01","2017-03-01")

tb <- example_3.tb
output.tb3 <- tb[FALSE,]
coord <- unique(tb$index)

# Apply for each time series based on index
for(x in 1:length(coord)){
  temp.tb <- tb[which(as.character(tb$index) == coord[x]),]
  temp_final.tb <- stilf_event_transitions(temp.tb, properties = p1, time_intervals = t1)
  output.tb3 <- dplyr::bind_rows(output.tb3, temp_final.tb)
}
output.tb3
</pre>

 - See plot with stilf_plot_maps_input function <code>stilf_plot_maps_input(example_3.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))</code>

 - View all events with stilf_plot_maps_events function <code>stilf_plot_maps_events(output.tb3, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), shape_point = 0, colour_point = "blue", size_point = 2.3)</code>

<table width="700" border="0">
<tr>
<td align="center" valign="center">
<img src="inst/figures/example3.1.png" alt="Fig. 5. Plot events discovered from time series classified data" />
<p class="caption">
Fig. 5. Plot events discovered from time series classified data
</p>
</td>
</tr>
</table>

- See barplot total area in square kilometers <code>stilf_plot_barplot_events(output.tb3, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), pixel_resolution = 250)</code> and sequence plot <code>stilf_plot_sequence_events(output.tb3, show_y_index = TRUE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))</code>

<table width="700" border="0" cellspacing="0" cellpadding="0">
<tr>
<td align="center" valign="center">
<img src="inst/figures/example3.2.png" alt="Fig. 6.(a) Barplot with total area" />
<br />
Fig. 6.(a) Barplot with total area
</td>

<td align="center" valign="center">
<img src="inst/figures/example3.3.png" alt="Fig. 6.(b) Sequence plot" />
<br />
Fig. 6.(b) Sequence plot
</td>

</tr>
</table>

<br />

