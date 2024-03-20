# Function to open file in RStudio or text editor
  js_code <- "
<script>
function openInRStudio(file_location) {
  console.log('Executing JavaScript function openInRStudio');
  console.log('File location: ' + file_location);

  var cmd = 'openInRStudio(\"' + file_location + '\")';
  console.log(cmd); // For debugging
  if (window.external) { // Check if in RStudio
    window.external(cmd);
  }
}
</script>
"
js_function <- "
  function openInRStudio(file_location) {
  console.log('Executing JavaScript function openInRStudio');
  console.log('File location: ' + file_location);

  var cmd = 'openInRStudio(\"' + file_location + '\")';
  console.log(cmd); // For debugging
  if (window.external) { // Check if in RStudio
      window.external(cmd);
    }
  }
  "
js_function <- "
function openInRStudio(file_location) {
  console.log('Executing JavaScript function openInRStudio');
  console.log('File location: ' + file_location);

  var url = 'rstudio://open/?file=' + encodeURIComponent(file_location);
  console.log('Opening RStudio with URL: ' + url);

  window.open(url);
}
"
js_function <- "function(el, x) {
  function openInRStudio(file_location) {
    console.log('Executing JavaScript function openInRStudio');
    console.log('File location: ' + file_location);

    // Check if in RStudio
    if (window.external) {
      var cmd = 'openInRStudio(\"' + file_location + '\")';
      console.log('Opening RStudio with URL: ' + cmd);
      window.external(cmd);
    } else {
      console.error('openInRStudio function is only supported in RStudio.');
    }
  }
}"

# Add the JavaScript function to the HTML content of the nodes
df_nodes$html <- paste0("<script>", js_function, "</script>")
