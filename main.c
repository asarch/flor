/*
 *  Nombre del programa: flor
 *  Autor              : Alef Sheridan Ariel R. Ch.
 *  Version            : 1.0
 *  Nombre del archivo : main.c
 *  Descripcion        : Rendereo de la funcion de flor usando
 *                       el widget GtkGLExt de GTK+ 2.x.
 */

#include <gtk/gtk.h>
#include <gtk/gtkgl.h>

#include <GL/gl.h>

#include <math.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
	GtkWidget *window;
	GtkWidget *box;

	GtkWidget *handle_box_1;
	GtkWidget *handle_box_2;

	GtkWidget* menu_bar;

	GtkWidget *file_menu_item;
	GtkWidget *file_menu;
	GtkWidget *quit_menu_item;

	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	g_signal_connect(GTK_WIDGET(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

	box = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), box);

	/* Menu bar */
	handle_box_1 = gtk_handle_box_new();
	gtk_box_pack_start(GTK_BOX(box), handle_box_1, FALSE, FALSE, 0);

	menu_bar = gtk_menu_bar_new();
	gtk_container_add(GTK_CONTAINER(handle_box_1), menu_bar);

	file_menu_item = gtk_menu_item_new_with_mnemonic("_File");
	gtk_menu_bar_append(GTK_MENU_BAR(menu_bar), file_menu_item);

	file_menu = gtk_menu_new();
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_menu_item), file_menu);

	quit_menu_item = gtk_menu_item_new_with_mnemonic("_Quit");
	gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), quit_menu_item);

	/* Toolbar */
	handle_box_2 = gtk_handle_box_new();
	gtk_box_pack_start(GTK_BOX(box), handle_box_2, FALSE, FALSE, 0);

	gtk_widget_show_all(window);
	gtk_main();

	return EXIT_SUCCESS;
}
