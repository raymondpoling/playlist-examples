\documentclass{article}

\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }

\usepackage{fullpage}

\begin{document}
\section{Module Exports}

Nothing in this module should be hidden at this moment in time. All of its 
functions are simple file manipulations functions, none of which do internal 
things, so far.

\begin{code}
module Playlist where
\end{code}

\section{Imports}

The List module is required for sort operations, used in solving the length 
of Complex Playlists.

\begin{code}
import List
import Primes (factor)
\end{code}

\section{Defined Types}

This module defines its own types (and by extension its own type constructors) 
for dealing with near arbitrarily complex playlists. That is, column based 
complexity can be represented, however row based complexity (in terms of a 
playlist, back to back episodes would be row based), this has to be taken 
care of and then put into the playlist.

\begin{code}
data Playlist = Single   [String]
              | Complex  [Playlist]
              | Merge    [Playlist]
                deriving (Eq, Ord, Show, Read)
\end{code}

Single is a single playlist, while Complex is a group of Single or Complex 
playlists.

\section{Length of Members}

One of the requirements of the system is to be able to tell the 'length' of 
various playlist items. These items lengths are determined differently. For 
example, the Complex length is equal to the length before it starts repeating, 
while Single is just the length of a simple list.

\begin{code}
playlength :: Playlist -> Int
playlength (Single a) = length a
-- New version: its shorter, easier to read!
playlength (Merge a) = sum (map playlength a)
-- New Complex Calc
playlength (Complex a) = (length a) * product (prodList values [])
    where
      values = map factor (map playlength a)
      prodList (hd:tl) acc = prodList tl (acc ++ (hd \\ acc))
      prodList [] acc = acc

\end{code}

The following, due to code change, no longer apply.

For Complex, the functions have the tasks of:

\begin{itemize}
\item sublength = Get all the lengths of the members of a in Complex a.
\item reduce = A function used to test if one number subsumes another.
\item subsume = A function to remove all numbers that are factors of other 
      numbers.
\end{itemize}

\section{Indexed Item Selection}

Using the types defined above, writing code for selecting an item should be 
relatively straightforward. For every complex, select a row of it using mod, 
applying the index div length of that level. Then use the index mod length to 
select a line item from Singles.

\begin{code}
select :: Playlist -> Int -> String
select (Complex []) _ = error "Empty Complex list in select."
select (Complex a) index = 
    select (a !! (index `mod` length a)) (index `div` (length a))
select (Single a) index = 
    ((!! mod index (length a)) a)
select (Merge []) _ = 
    error "Empty Merge list in select."
select (Merge a) index | (playlength (Merge a) - 1) < index = 
                           select (Merge a) (mod index (playlength (Merge a)))
select (Merge a) index | playlength (head a) > index = 
                           select (head a) index
select (Merge a) index = 
    select (Merge (tail a)) (index - (playlength (head a)))
\end{code}

\section{Getting a Frame}

Since we define a Complex playlist in such a way as to be a list of lists, and this is being used to reflect the idea of a tv station, its only natural to want to get an entire 'schedule'. Schedules are going to be refered to as frames, which is a generic term that seems to fit the idea. Another function, align, will return an index for the first item in a frame.

A frame returns a list of items fitting within a frame, and a new index.

\begin{code}
getFrame :: Playlist -> Int -> (Int, [String])
getFrame (Single list)  index = (index + 1 , [(select (Single list) index)])
getFrame (Merge list) index = (index + 1, [(select (Merge list) index)])
getFrame (Complex list) index = 
    (index + (length list), (listing (Complex list) [] index (length list)))
         where 
           listing :: Playlist -> [String] -> Int -> Int -> [String]
           listing playlist acc index count | count > 0 =  
                   listing playlist (acc ++ [(select playlist index)]) (index + 1) (count -1)
           listing _ acc _ _ = acc
\end{code}

The code above may look somewhat complex, but all it actually says that, if its a Single, then don't bother doing anything, just return the index + 1 and a single playlist item. Otherwise, get a number of items equal to the length of the topmost list, and return the index + that length. However, it makes no attempt to assure that you are actually starting from the front of the list (and as the user, you may not actually care).

\end{document}
