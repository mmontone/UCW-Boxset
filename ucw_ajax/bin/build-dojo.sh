
UCW_HOME="`dirname $0`/.."
DOJO_HOME="`dirname $0`/../../dojo"
DIJIT_HOME="`dirname $0`/../../dijit"

absolutize ()
{
  if [ ! -d "$1" ]; then
    echo
    echo "ERROR: '$1' doesn't exist or not a directory!"
    exit -1
  fi

  cd "$1"
  echo `pwd`
  cd - >/dev/null
}

if [ "$1" != "" ]; then
    DOJO_HOME=$1
fi

if [ "$2" != "" ]; then
    DIJIT_HOME=$2
fi

UCW_HOME=`absolutize "$UCW_HOME"`
DOJO_HOME=`absolutize "$DOJO_HOME"`
DIJIT_HOME=`absolutize "$DIJIT_HOME"`

echo "Assuming the following paths:"
echo "ucw   - $UCW_HOME"
echo "dojo  - $DOJO_HOME"
echo "dijit - $DIJIT_HOME"

if [ ! -d "$DOJO_HOME" -o ! -d "$DIJIT_HOME" -o ! -d "$UCW_HOME" ]; then
    echo Some of the paths are not correct!
    echo Some hints:
    echo svn co http://svn.dojotoolkit.org/dojo/dojo/trunk dojo
    echo svn co http://svn.dojotoolkit.org/dojo/dijit/trunk dijit
    exit -1
fi

cd "$DOJO_HOME/buildscripts"
cp "$UCW_HOME/etc/ucw.profile.js" "$DOJO_HOME/buildscripts/profiles/ucw.profile.js"
ant -Dprofile="ucw" -Ddocless=true clean release intern-strings

rm -rf "$UCW_HOME/wwwroot/dojo/src/"
rm -f "$UCW_HOME/wwwroot/dijit"

cp "$DOJO_HOME/release/dojo/dojo.js" "$UCW_HOME/wwwroot/dojo/"
cp "$DOJO_HOME/release/dojo/iframe_history.html" "$UCW_HOME/wwwroot/dojo/"
cp -r "$DOJO_HOME/release/dojo/src/" "$UCW_HOME/wwwroot/dojo/"
ln -s "$DIJIT_HOME" "$UCW_HOME/wwwroot/dijit"
