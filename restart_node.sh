#!/bin/bash

# שם הסקריפט: restart_node.sh
# תפקיד: מפעיל מחדש צומת Erlang ספציפי במערכת.
# שימוש: ./restart_node.sh <node_type>
# דוגמה: ./restart_node.sh customers
#safe,customers,waiters,machines,tables

# 1. בדיקה אם המשתמש סיפק קלט
if [ -z "$1" ]; then
  echo "שגיאה: יש לספק את סוג הצומת להפעלה."
  echo "שימוש: $0 <node_type>"
  echo "דוגמאות:"
  echo "  $0 safe"
  echo "  $0 customers"
  exit 1
fi

# --- הוספת הגדרות תואמות ---
# 2. הגדרת משתנים זהים לאלו שבסקריפט הראשי
NODE_TYPE=$1
HOST="127.0.0.1"
COOKIE="secret" # <-- הוספת העוגייה הזהה
SNAME=""
# --- סוף התוספת ---

# 3. בניית שם הצומת המלא (--name)
if [ "$NODE_TYPE" == "safe" ]; then
  SNAME="safe_node@${HOST}"
else
  SNAME="${NODE_TYPE}_node@${HOST}"
fi

# 4. הרצת הפקודה בתוך חלון טרמינל חדש
echo "--- Launching node ${SNAME} in a new terminal... ---"
gnome-terminal -- bash -c "echo 'Starting ${SNAME}...'; \
  rebar3 shell --name '${SNAME}' --setcookie '${COOKIE}' --eval 'general_start:start(${NODE_TYPE}).'; \
  exec bash" &