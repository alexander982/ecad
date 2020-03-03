page_setup:dialog {
  label = "Настройка страниц";
  :boxed_column {
    label="Параметры новых ссылок";
    :popup_list {
      label="Цвет";
      key="xrColor";
      edit_width=14;
    }
    :edit_box{
      label="Размер шрифта";
      key="xrSize";
      value="3.0";
      edit_width=4;
    }
  }
  :boxed_column {
    label="Настройка страницы";
    :popup_list {
      label="Формат листов";
      key="pageFormat";
      list="A4 книжн.\nА3 альб.";
      edit_width=14;}
    :toggle {
      label="Только горизонтальные зоны";
      key="horizontalOnly";
      is_enabled=false;
      value="1";}
    :edit_box {
      label="Горизонтальных зон";
      key="hZones";
      value="6";
      edit_width=4;}
    :edit_box {
      label="Вертикальных зон";
      key="vZones";
      value="0";
      is_enabled=false;
      edit_width=4;}
  }
  :boxed_radio_column {
    label="Расположение листов";
    value="listInTable";
    :radio_button {label="В одну строку"; key="listInRow";}
    :radio_button {label="Таблично"; key="listInTable";}
    :edit_box {label="Листов в строке"; key="qntListsInRow"; value="4";edit_width=4;}
  }
  spacer_1;
  :toggle {label="Указать первый лист";key="pickFirstPage";}
  spacer_1;
  ok_cancel;
}
