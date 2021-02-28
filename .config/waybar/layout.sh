name=$(swaymsg -t get_inputs | jq -r 'map(select(has("xkb_active_layout_name"))) | .[0].xkb_active_layout_name')

short=""
class=""

case $name in
  "English (UK)")
    short="GB"
    class="gb"
    ;;

  "German (Switzerland)")
    short="CH"
    class="ch"
    ;;

  "Russian")
    short="RU"
    class="ru"
    ;;

  *)
    short="??"
    class="unknown"
    ;;
esac

echo "{ \"text\": \"$short\", \"tooltip\": \"$name\", \"class\": \"$class\" }"
