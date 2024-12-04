// Initialize a variable to mirror the values of 'aiAssist',
// 'openInShiny' and 'openInRStudio'
var currentAiAssistValue = null;
var currentOpenInShinyValue = null;
var currentOpenInRstudioValue = null;

function aiAssist(function_name) {
  console.log('Executing JavaScript function aiAssist');
  console.log('Function name: ' + function_name);

  // Check if the current value is the same as 'function_name'
  if (currentAiAssistValue === function_name) {

    // Temporarily set 'aiAssist' and 'close_tab' to null
    // to ensure the change is detected
    Shiny.setInputValue('aiAssist', null, {priority: 'event'});
    Shiny.setInputValue('close_tab', null, {priority: 'event'});

    // Use setTimeout to ensure the null value is processed before setting
    // the new value
    setTimeout(function() {
      Shiny.setInputValue('aiAssist', function_name, {priority: 'event'});
    }, 10);

  } else {

    // Directly set 'aiAssist' to 'function_name'
    Shiny.setInputValue('aiAssist', function_name);
  }

  // Update the mirror variable to reflect the new value
  currentAiAssistValue = function_name;
}

function openInShiny(file_location) {
  console.log('Executing JavaScript function openInShiny');
  console.log('File location: ' + file_location);

  // Check if the current value is the same as 'file_location'
  if (currentOpenInShinyValue === file_location) {

    // Temporarily set 'openInShiny' and 'close_tab' to null
    // to ensure the change is detected
    Shiny.setInputValue('openInShiny', null, {priority: 'event'});
    Shiny.setInputValue('close_tab', null, {priority: 'event'});

    // Use setTimeout to ensure the null value is processed before setting
    // the new value
    setTimeout(function() {
      Shiny.setInputValue('openInShiny', file_location, {priority: 'event'});
    }, 10);
  } else {

    // Directly set 'openInShiny' to 'file_location' if the values are
    // different
    Shiny.setInputValue('openInShiny', file_location, {priority: 'event'});
  }

  // Update the mirror variable to reflect the new value
  currentOpenInShinyValue = file_location;
}

function openInRStudio(file_location) {
  console.log('Executing JavaScript function openInRStudio');
  console.log('File location: ' + file_location);

  // Check if the current value is the same as 'file_location'
  if (currentOpenInRstudioValue === file_location) {

    // Temporarily set 'openInRStudio' to null to ensure the change is
    // detected
    Shiny.setInputValue('openInRStudio', null, {priority: 'event'});

    // Use setTimeout to ensure the null value is processed before setting
    // the new value
    setTimeout(function() {
      Shiny.setInputValue('openInRStudio', file_location, {priority: 'event'});
    }, 10);

  } else {

    // Directly set 'openInRStudio' to 'file_location'
    Shiny.setInputValue('openInRStudio', file_location);
  }

  // Update the mirror variable to reflect the new value
  currentOpenInRstudioValue = file_location;
}

$(document).on('shiny:connected', function(event) {
  function adjustTabHeight() {
    var windowHeight = $(window).height();
    // Distance from the top of the viewport
    var offsetTop = $('#fileTabs').offset().top;
    // Subtract any additional margin or padding
    var tabHeight = windowHeight - offsetTop - 20;
    $('.tab-content').css('height', tabHeight + 'px');
  }

  // Adjust the height on window resize
  $(window).resize(adjustTabHeight);
  // Initial adjustment
  adjustTabHeight();
});
