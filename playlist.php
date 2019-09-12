<?php // This is `classes' for storing and working with 
  // playlist objects. Single, Back2Back, and Cycle are all 
  // represented.
  // As a special case, Schedule is presented to do the getFrame task.

// Query: make queries to the database.
require_once('query.php');

// Single holds a single playlist. Its generated by calling the database
// to generate the requisite series list on call.
class Single {
  // 
  private $playlist = Array();
  // Create the Single object.
  function __construct($series) {
    $query = 
      "SELECT episodes.id as episodeID ".
      "FROM   episodes ".
      "WHERE episodes.series_id = $series ".
      "ORDER BY seasonnum, epnumber ";
    $this->playlist = queryPHP($query,'queryError.html');
    $temp = Array();
    foreach($this->playlist as $item)
      {

	    $temp[] = $item[0];
      }
    $this->playlist = $temp;
    return $this;
  }
  // Get the length of a playlist
  public function length() {
    return count($this->playlist);
  }
  // Get an item at an index
  public function index($index) {
    return $this->playlist[$index%$this->length()];
  }
}

// Create a Back2Back playlist.
// This takes a list of series, and a root.
// It extends its parent class, but rewrites everything (same API,
// but completely different meaning.
class Back2Back extends Single {
  function __construct($series){
    $this->playlist = $series;
    return $this;
  }

  // The length of this list. Necessary for the indexing function.
  function length() {
    $temp = 0;
    foreach($this->playlist as $item)
      {
	$temp = $temp + $item->length();
      }
    return $temp;
  }

  // Find the next item to play.
  function index($index){
    // convert the extended index to an index within the length of the
    // back2back list.
    $index = $index % $this->length();
    foreach($this->playlist as $item)
      {
	if($index < $item->length())
	  {
	    return $item->index($index);
	  }
	$index = $index - $item->length();
      }
  }
}

class Cycle extends Back2Back {
  // This indicates an error condition.
  function length() {
    throw new Exception('This should never be called.');
  }
  function index($index) {
    return $this->playlist[$index % count($this->playlist)]->index($index/count($this->playlist));
  }
}

// Actually this is just Cycle with one extra function, but hell,
// I want this to work nicely.
class Schedule extends Cycle {
  function __construct($schedule) {
    $this->playlist = $schedule;
  }
  function getFrame($frame) {
    $collect = Array();
    for($i = 0; $i < count($this->playlist); $i = $i + 1) 
      {
	$collect[] = $this->index($frame*count($this->playlist)+$i);
      }
    return $collect;
  }
}

// Functionality for converting xml into the schedule classes
// for use.
function xml2Schedule($xml){
  switch($xml->getName()) 
    {
    case 'schedule': 
      $collect = Array();
      foreach($xml->children() as $item)
	{
	  $collect[] = xml2Schedule($item);
	}
      return new Schedule($collect);
    case 'single': return new Single((string)$xml);
    case 'back2back': 
      $collect = Array();
      foreach($xml->children() as $item)
	{
	  $collect[] = xml2Schedule($item);
	}
      return new Back2Back($collect);
    case 'cycle':
      $collect = Array();
      foreach($xml->children() as $item)
	{
	  $collect[] = xml2Schedule($item);
	}
      return new Cycle($collect);
    default: 
      throw new Exception('Unknown tag found in playlist: '.$xml->getName());
    }

}

function fetchPlaylist($playlistID) {
  $query = 
    "SELECT playlist FROM playlist where id = $playlistID";
  $query = queryPHP($query,'queryError.html');
  return xml2Schedule(simplexml_load_string($query[0][0]));
}