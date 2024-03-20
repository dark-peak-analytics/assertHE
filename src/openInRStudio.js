function openInRStudio(file_location) {
  console.log('Executing JavaScript function openInRStudio');
  console.log('File location: ' + file_location); // For debugging

  // Send message to Shiny server
  Shiny.setInputValue('openRStudio', file_location);
}
